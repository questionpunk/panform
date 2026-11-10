-- | Native visual settings. These are preserved as data, never rendered or
-- translated into another vendor's CSS. A format switch reports the loss.
module Panform.Design
  ( designFields
  , readDesign
  , writeDesign
  , warnDesignLoss
  , designSupport
  , designAttributes
  ) where

import Control.Monad (forM_, unless)
import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Panform.AST
import Panform.Conv

-- | Only visual properties: never access controls, webhooks or credentials.
-- Jotform's fields live in properties; the others live on the survey object.
designFields :: Text -> [Text]
designFields = \case
  "jotform" -> ["font", "fontsize", "fontcolor", "background", "styles", "formWidth", "labelWidth", "alignment", "lineSpacing", "highlightLine", "injectCSS"]
  "formbricks" -> ["styling"]
  "surveyjs" -> ["logo", "logoWidth", "logoHeight", "logoFit", "logoPosition", "backgroundImage", "backgroundOpacity", "width", "widthMode", "questionsOnPageMode", "showProgressBar", "progressBarLocation", "progressBarType", "showQuestionNumbers", "questionTitleLocation", "questionDescriptionLocation"]
  "msforms" -> ["otherInfo", "background", "header", "logo"]
  _ -> []

readDesign :: Text -> Value -> Attrs
readDesign name (Object o) =
  M.fromList [(name <> ":design", Object visual) | not (KM.null visual)]
  where
    visual = KM.filterWithKey (\k _ -> K.toText k `elem` designFields name) o
readDesign _ _ = M.empty

writeDesign :: Text -> Meta -> [(K.Key, Value)]
writeDesign name meta = case M.lookup (name <> ":design") (metaAttrs meta) of
  Just (Object o) -> [(k, v) | (k, v) <- KM.toList o, K.toText k `elem` designFields name]
  _ -> []

-- | Registry writers share this check, including writer-only outputs.
-- Native and QuestionPunk Markdown keep extension metadata unchanged.
warnDesignLoss :: Text -> Form -> Conv ()
warnDesignLoss target (Form meta _) =
  unless (target `elem` ["native", "questionpunk"]) $
    forM_ (M.toList (designAttributes (metaAttrs meta))) $ \(key, value) ->
      case T.stripSuffix ":design" key of
        Just source -> case value of
          Object fields -> forM_ (KM.keys fields) $ \field ->
            unless (source == target && K.toText field `elem` designFields target) $
              warn "design" (source <> " " <> K.toText field <> " is not mapped to " <> target <> "; set it in the destination editor")
          _ -> warn "design" ("invalid " <> key <> " metadata is not mapped to " <> target)
        Nothing -> whenTypeform key
  where
    whenTypeform key =
      if key `elem` ["typeform:theme", "typeform:cui_settings"] && target /= "typeform"
      then warn "design" (key <> " is not mapped to " <> target <> "; set it in the destination editor")
      else pure ()

-- | Describes this converter, not all features offered by a vendor's UI.
designSupport :: Text -> (Text, [Text], Text)
designSupport name
  | name `elem` ["native", "questionpunk"] =
      ("metadata", [], "Keeps native visual settings as metadata. Does not render them or translate designs between vendors.")
  | name == "typeform" =
      ("same_format", ["theme", "cui_settings"], "Keeps the existing theme reference and conversational UI settings. Referenced themes and images must remain accessible; assets are not downloaded.")
  | not (null fields) =
      ("same_format", fields, "Keeps the listed native visual settings when writing back to this format. Other destinations report unmapped design settings. Image references and CSS are data, not uploaded assets.")
  | otherwise =
      ("not_mapped", [], "No survey-wide visual design mapping in this converter. Configure branding in the destination; question structure and appearance have separate conversion checks.")
  where fields = designFields name

-- | Unlike incidental vendor metadata, visual settings count as information
-- in the fidelity audit. Keep nested values exact, including unknown keys.
designAttributes :: Attrs -> Attrs
designAttributes = M.filterWithKey (\key _ -> ":design" `T.isSuffixOf` key || key `elem` ["typeform:theme", "typeform:cui_settings"])
