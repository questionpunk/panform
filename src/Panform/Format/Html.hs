-- | A static, printable HTML form: real @<form>@ controls (radio, checkbox,
-- select, textarea, number and date inputs), one @<fieldset>@ per page,
-- and logic kept in @data-relevant@ attributes in the SurveyJS dialect
-- with a plain-English "Shown if" line beside it. Writer only; no script.
module Panform.Format.Html (writeHtml) where

import Control.Monad (forM, when)
import Data.Aeson (Value (String))
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Panform.AST
import Panform.Conv
import Panform.Expr
import Panform.Format.Markdown (Names, english, surveyNames)
import Panform.Format.TextUtil

writeHtml :: Form -> Conv BL.ByteString
writeHtml s@(Form meta items) = do
  warnTranslations "html" s
  let names = surveyNames s
      title = maybe "Survey" txt (metaTitle meta)
  pages <- forM (zip [1 :: Int ..] (paginate items)) $ \(n, (g, kids)) -> do
    body <- concat <$> mapM (writeItem names) kids
    case g of
      Nothing -> pure body
      Just grp -> do
        rel <- printLogic surveyjs ("page " <> pageId n g) (groupRelevant grp)
        pure $
          [ "<fieldset class=\"page\"" <> attr "id" ("page-" <> groupId grp) <> maybeAttr "data-relevant" rel <> ">"
          , "<legend>" <> maybe ("Page " <> T.pack (show n)) (html . txt) (groupTitle grp) <> "</legend>"
          ]
            ++ shownIf names (groupRelevant grp) rel
            ++ body
            ++ ["</fieldset>"]
  let doc =
        [ "<!DOCTYPE html>"
        , "<html" <> maybe "" (attr "lang") (headMay (metaLanguages meta)) <> ">"
        , "<head>"
        , "<meta charset=\"utf-8\">"
        , "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
        , "<title>" <> html title <> "</title>"
        , "<style>" <> css <> "</style>"
        , "</head>"
        , "<body>"
        , "<form" <> maybe "" (attr "id") (metaId meta) <> ">"
        ]
          ++ ["<h1>" <> html (txt t) <> "</h1>" | Just t <- [metaTitle meta]]
          ++ ["<p class=\"description\">" <> html (txt d) <> "</p>" | Just d <- [metaDescription meta]]
          ++ concat pages
          ++ ["<p><button type=\"submit\">Submit</button></p>", "</form>", "</body>", "</html>"]
  pure . BL.fromStrict . TE.encodeUtf8 $ T.unlines doc
  where
    headMay = \case
      (x : _) -> Just x
      [] -> Nothing

css :: Text
css =
  T.concat
    [ "body{font-family:system-ui,sans-serif;max-width:44rem;margin:2rem auto;padding:0 1rem;line-height:1.5;color:#1a1a1a;background:#fff}"
    , "fieldset{border:1px solid #ccc;border-radius:6px;margin:0 0 1.5rem;padding:1rem}"
    , "fieldset.question,fieldset.section{border:0;padding:0;margin:0 0 1.25rem}"
    , "legend,.label{font-weight:600;display:block}"
    , ".question{margin:0 0 1.25rem}"
    , ".hint,.logic{color:#555;font-size:.9em;margin:.2rem 0}"
    , ".logic{font-style:italic}"
    , ".required{color:#b00}"
    , ".options label{display:block}"
    , ".scale label{display:inline-block;margin-right:.6rem}"
    , "table{border-collapse:collapse}th,td{padding:.25rem .5rem;text-align:center}th[scope=row]{text-align:left;font-weight:normal}"
    , "input[type=text],input[type=email],input[type=url],input[type=tel],textarea{width:100%;box-sizing:border-box}"
    , "@media print{fieldset{break-inside:avoid}button{display:none}}"
    ]

html :: Text -> Text
html = T.replace "\n" "<br>" . escape

escape :: Text -> Text
escape = T.concatMap $ \case
  '&' -> "&amp;"
  '<' -> "&lt;"
  '>' -> "&gt;"
  '"' -> "&quot;"
  '\'' -> "&#39;"
  c -> T.singleton c

attr :: Text -> Text -> Text
attr k v = " " <> k <> "=\"" <> escape v <> "\""

maybeAttr :: Text -> Maybe Text -> Text
maybeAttr k = maybe "" (attr k)

flag :: Text -> Bool -> Text
flag k b = if b then " " <> k else ""

shownIf :: Names -> Maybe Expr -> Maybe Text -> [Text]
shownIf names e printed = ["<p class=\"logic\">Shown if " <> html (english names x) <> ".</p>" | Just x <- [e], isJust printed]

writeItem :: Names -> Item -> Conv [Text]
writeItem names = \case
  ItemNote i t rel -> do
    r <- printLogic surveyjs i rel
    pure ["<div class=\"note\"" <> attr "id" ("q-" <> i) <> maybeAttr "data-relevant" r <> ">" <> html (txt t) <> "</div>"]
  ItemRaw (Format "html") (String t) -> pure [t]
  ItemRaw (Format f) _ -> [] <$ warn "html" ("raw " <> f <> " content dropped")
  ItemGroup g -> do
    r <- printLogic surveyjs (groupId g) (groupRelevant g)
    kids <- concat <$> mapM (writeItem names) (groupItems g)
    when (groupKind g == Repeat) $ warn (groupId g) "repeat group shown once"
    let cls = case groupKind g of
          Page -> "page"
          Section -> "section"
          Repeat -> "repeat"
    pure $
      ["<fieldset" <> attr "class" cls <> attr "id" ("group-" <> groupId g) <> flag "data-repeat" (groupKind g == Repeat) <> maybeAttr "data-relevant" r <> ">"]
        ++ ["<legend>" <> html (txt t) <> "</legend>" | Just t <- [groupTitle g]]
        ++ shownIf names (groupRelevant g) r
        ++ kids
        ++ ["</fieldset>"]
  ItemQuestion q -> writeQuestion names q

writeQuestion :: Names -> Question -> Conv [Text]
writeQuestion names q = do
  rel <- printLogic surveyjs i (qRelevant q)
  valE <- printLogic surveyjs i (qValidation q >>= valExpr)
  let v = qValidation q
      common =
        maybeAttr "data-validate" valE
          <> maybeAttr "title" (txt <$> (v >>= valMessage))
          <> maybeAttr "aria-describedby" (hintId <$ qHint q)
      label = html (txt (qLabel q)) <> (if qRequired q then " <span class=\"required\">*</span>" else "")
      numbered = numberOf <> label
      hint = ["<p class=\"hint\"" <> attr "id" hintId <> ">" <> html (txt h) <> "</p>" | Just h <- [qHint q]]
      logic = shownIf names (qRelevant q) rel
      container ctrl =
        ["<div class=\"question\"" <> attr "id" ("q-" <> i) <> maybeAttr "data-relevant" rel <> ">", "<label class=\"label\"" <> attr "for" i <> ">" <> numbered <> "</label>"]
          ++ hint
          ++ logic
          ++ ctrl
          ++ ["</div>"]
      group cls ctrl =
        ["<fieldset" <> attr "class" ("question " <> cls) <> attr "id" ("q-" <> i) <> maybeAttr "data-relevant" rel <> flag "data-required" (qRequired q) <> ">", "<legend>" <> numbered <> "</legend>"]
          ++ hint
          ++ logic
          ++ ctrl
          ++ ["</fieldset>"]
      input ty extra = "<input" <> attr "type" ty <> attr "id" i <> attr "name" i <> extra <> maybeAttr "value" (qDefault q) <> flag "required" (qRequired q) <> common <> ">"
      bounds lo hi = maybeAttr lo (showNumber <$> (v >>= valMin)) <> maybeAttr hi (showNumber <$> (v >>= valMax))
  case qType q of
    QText k -> pure . container $ case k of
      LongText -> ["<textarea" <> attr "id" i <> attr "name" i <> attr "rows" "4" <> bounds "minlength" "maxlength" <> flag "required" (qRequired q) <> common <> ">" <> maybe "" escape (qDefault q) <> "</textarea>"]
      Email -> [input "email" (bounds "minlength" "maxlength")]
      Url -> [input "url" (bounds "minlength" "maxlength")]
      Phone -> [input "tel" (bounds "minlength" "maxlength")]
      ShortText -> [input "text" (bounds "minlength" "maxlength")]
    QNumber k -> pure (container [input "number" (bounds "min" "max" <> attr "step" (if k == Integer then "1" else "any"))])
    QDate k -> pure (container [input (case k of Date -> "date"; Time -> "time"; DateTime -> "datetime-local") ""])
    QChoice sel Dropdown cs -> do
      noteFlags cs
      pure . container $
        ["<select" <> attr "id" i <> attr "name" i <> flag "multiple" (sel == Multiple) <> flag "required" (qRequired q) <> common <> ">"]
          ++ ["<option value=\"\">Choose...</option>" | sel == Single]
          ++ ["<option" <> attr "value" (choiceValue c) <> flag "selected" (qDefault q == Just (choiceValue c)) <> ">" <> html (txt (choiceLabel c)) <> "</option>" | c <- cs]
          ++ ["</select>"]
    QChoice sel _ cs -> pure (group "options" (options (if sel == Single then "radio" else "checkbox") (sel == Single) cs))
    QBoolean -> pure (group "options" (options "radio" True [choice "true" "Yes", choice "false" "No"]))
    QScale lo hi l h ->
      pure . group "scale" $
        ["<div>"]
          ++ ["<span class=\"hint\">" <> html (txt t) <> "</span>" | Just t <- [l]]
          ++ [ "<label><input type=\"radio\"" <> attr "name" i <> attr "value" n <> flag "required" (qRequired q) <> flag "checked" (qDefault q == Just n) <> "> " <> n <> "</label>"
             | x <- [lo .. hi]
             , let n = T.pack (show x)
             ]
          ++ ["<span class=\"hint\">" <> html (txt t) <> "</span>" | Just t <- [h]]
          ++ ["</div>"]
    QMatrix sel rows cols ->
      pure . group "matrix" $
        ["<table>", "<thead><tr><td></td>" <> T.concat ["<th scope=\"col\">" <> html (txt (choiceLabel c)) <> "</th>" | c <- cols] <> "</tr></thead>", "<tbody>"]
          ++ [ "<tr><th scope=\"row\">" <> html (txt (choiceLabel r)) <> "</th>"
                <> T.concat
                  [ "<td><input" <> attr "type" (if sel == Single then "radio" else "checkbox") <> attr "name" (i <> "." <> choiceValue r) <> attr "value" (choiceValue c)
                      <> attr "aria-label" (txt (choiceLabel r) <> ": " <> txt (choiceLabel c))
                      <> flag "required" (qRequired q && sel == Single)
                      <> "></td>"
                  | c <- cols
                  ]
                <> "</tr>"
             | r <- rows
             ]
          ++ ["</tbody>", "</table>"]
    QRanking cs ->
      pure . group "ranking" $
        ["<p class=\"hint\">Number the options in order, 1 = first.</p>", "<ol class=\"options\">"]
          ++ [ "<li><label><input type=\"number\"" <> attr "name" (i <> "." <> choiceValue c) <> attr "min" "1" <> attr "max" (T.pack (show (length cs))) <> attr "step" "1" <> flag "required" (qRequired q) <> "> " <> html (txt (choiceLabel c)) <> "</label></li>"
             | c <- cs
             ]
          ++ ["</ol>"]
    QFile -> pure (container [input "file" ""])
    QGeo -> container [input "text" (attr "placeholder" "latitude, longitude")] <$ warn i "geopoint written as a text box"
    QHidden -> pure ["<input type=\"hidden\"" <> attr "id" i <> attr "name" i <> maybeAttr "value" (qDefault q) <> ">"]
    QCalculate e -> do
      c <- printLogic surveyjs i (Just e)
      pure ["<div class=\"question calculated\"" <> attr "id" ("q-" <> i) <> ">" <> numbered <> ": <output" <> attr "name" i <> maybeAttr "data-calculate" c <> "></output></div>"]
    QOther t -> container [input "text" (attr "data-type" t)] <$ warn i ("type " <> t <> " written as a text box")
  where
    i = qId q
    hintId = i <> "-hint"
    numberOf = T.concat [n <> ". " | Just n <- [numberFor names i]]
    noteFlags cs = when (any choiceOther cs) $ warn i "other free-text box dropped from dropdown"
    options ty single cs =
      ["<div class=\"options\">"]
        ++ concat
          [ [ "<label><input" <> attr "type" ty <> attr "name" i <> attr "value" (choiceValue c)
                <> flag "required" (qRequired q && single)
                <> flag "checked" (qDefault q == Just (choiceValue c))
                <> flag "data-exclusive" (choiceExclusive c)
                <> "> "
                <> html (txt (choiceLabel c))
                <> (if choiceOther c then " <input type=\"text\"" <> attr "name" (i <> "-other") <> attr "aria-label" (txt (choiceLabel c) <> ", please specify") <> ">" else "")
                <> "</label>"
            ]
          | c <- cs
          ]
        ++ ["</div>"]

-- | The "Q3" number the English logic uses, so the two agree.
numberFor :: Names -> Text -> Maybe Text
numberFor names q = case english names (ERef q) of
  n | n /= q -> Just n
  _ -> Nothing
