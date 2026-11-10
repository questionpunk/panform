-- | The format registry: every reader and writer by name and extension.
module Panform.Formats
  ( FormatSpec (..)
  , formats
  , lookupFormat
  , formatForPath
  , convert
  , catalog
  ) where

import Control.Monad (when)
import Data.Aeson (Value, object, (.=))
import Data.Maybe (isJust)
import qualified Data.ByteString.Lazy as BL
import Data.Char (toLower)
import Data.List (find, isSuffixOf, sortOn)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T
import Panform.AST
import Panform.Conv
import Panform.Design (designSupport, warnDesignLoss)
import Panform.Format.Formbricks
import Panform.Format.Jotform
import Panform.Expr.Simplify (simplifyLogic)
import Panform.Format.Blaise
import Panform.Format.Decipher
import Panform.Format.MrScript
import Panform.Format.GoogleForms
import Panform.Format.Aiken
import Panform.Format.Gift
import Panform.Format.Html
import Panform.Format.JsPsych
import Panform.Format.Markdown
import Panform.Format.LimeSurveyLss
import Panform.Format.LimeSurveyTsv
import Panform.Format.MSForms
import Panform.Format.Native
import Panform.Format.Qualtrics
import Panform.Format.QualtricsTxt
import Panform.Format.QuestionPunk
import Panform.Format.QuestionPunkJson
import Panform.Format.REDCap
import Panform.Format.SurveyDown
import Panform.Format.SurveyJS
import Panform.Format.TripleS
import Panform.Format.DDI
import Panform.Format.Spss
import Panform.Format.ODM
import Panform.Format.QTI
import Panform.Format.SurveyMonkey
import Panform.Format.Tally
import Panform.Format.Typeform
import Panform.Format.XLSForm
import Panform.Format.XForm
import Panform.Format.FHIR

data FormatSpec = FormatSpec
  { fmtName :: String
  , fmtDescription :: Text
  , fmtExtensions :: [String]
  -- ^ Longest first; used to guess the format from a file name.
  , fmtReader :: Maybe (BL.ByteString -> Conv Form)
  , fmtWriter :: Maybe (Form -> Conv BL.ByteString)
  }

formats :: [FormatSpec]
formats = map guardConversion registered

-- | Empty reads and unmapped visual settings are visible to every registry
-- caller, including the CLI, audit and integration catalog consumers.
guardConversion :: FormatSpec -> FormatSpec
guardConversion f = f
  { fmtReader = fmap (\r bs -> r bs >>= check) (fmtReader f)
  , fmtWriter = fmap (\w s -> warnDesignLoss (T.pack (fmtName f)) s >> w s) (fmtWriter f)
  }
  where
    check s = do
      when (null (allItems (formItems s))) $
        warn (T.pack (fmtName f)) "no questions or text found in the input"
      pure s

registered :: [FormatSpec]
registered =
  [ FormatSpec "native" "panform AST as JSON (lossless)" [".panform.json", ".native.json"] (Just readNative) (Just writeNative)
  , FormatSpec "questionpunk" "QuestionPunk Markdown: a questionnaire as it reads on paper" [".qp.md"] (Just readQPMarkdown) (Just writeQPMarkdown)
  , FormatSpec "questionpunk-json" "QuestionPunk app survey JSON (chat-editor payload: items, sections, display logic)" [".qp.json"] (Just readQPJson) (Just writeQPJson)
  , FormatSpec "surveyjs" "SurveyJS Form Library JSON" [".surveyjs.json", ".survey.json"] (Just readSurveyJS) (Just writeSurveyJS)
  , FormatSpec "xlsform" "XLSForm workbook (ODK, KoboToolbox, SurveyCTO)" [".xlsx"] (Just readXLSForm) (Just writeXLSForm)
  , FormatSpec "xlsform-csv" "XLSForm as pyxform single-file CSV" [".xlsform.csv"] (Just readXLSFormCsv) (Just writeXLSFormCsv)
  , FormatSpec "formbricks" "Formbricks survey JSON (blocks or v1 questions)" [".formbricks.json"] (Just (fmap simplifyLogic . readFormbricks)) (Just writeFormbricks)
  , FormatSpec "surveymonkey" "SurveyMonkey API v3 survey details / create body" [".surveymonkey.json"] (Just readSurveyMonkey) (Just writeSurveyMonkey)
  , FormatSpec "jotform" "Jotform form questions + properties JSON (API)" [".jotform.json"] (Just readJotform) (Just writeJotform)
  , FormatSpec "tally" "Tally form blocks JSON (API)" [".tally.json"] (Just (fmap simplifyLogic . readTally)) (Just writeTally)
  , FormatSpec "google-forms" "Google Forms API Form resource (forms.get)" [".gform.json"] (Just (fmap simplifyLogic . readGoogleForms)) (Just writeGoogleForms)
  , FormatSpec "google-forms-batch" "Google Forms API forms.batchUpdate request body" [".gform-batch.json"] (Just (fmap simplifyLogic . readGoogleFormsBatch)) (Just writeGoogleFormsBatch)
  , FormatSpec "typeform" "Typeform Create API form JSON" [".typeform.json"] (Just readTypeform) (Just writeTypeform)
  , FormatSpec "decipher" "Forsta Surveys (Decipher) survey XML" [".decipher.xml"] (Just readDecipher) (Just writeDecipher)
  , FormatSpec "mrscript" "IBM SPSS Dimensions / Unicom Intelligence mrScript metadata and routing" [".mrs"] (Just readMrScript) (Just writeMrScript)
  , FormatSpec "blaise" "Blaise 4/5 data model source" [".bla"] (Just readBlaise) (Just writeBlaise)
  , FormatSpec "xform" "ODK XForms XML (as compiled by pyxform)" [".xform.xml", ".xform"] (Just readXForm) (Just writeXForm)
  , FormatSpec "fhir" "HL7 FHIR R4/R5 Questionnaire JSON (with SDC extensions)" [".fhir.json", ".questionnaire.json"] (Just readFHIR) (Just writeFHIR)
  , FormatSpec "qsf" "Qualtrics Survey Format (QSF) export" [".qsf"] (Just readQualtrics) (Just writeQualtrics)
  , FormatSpec "qualtrics-txt" "Qualtrics TXT import, Advanced Format" [".qualtrics.txt"] (Just readQualtricsTxt) (Just writeQualtricsTxt)
  , FormatSpec "limesurvey" "LimeSurvey survey structure (.lss XML)" [".lss"] (Just readLimeSurveyLss) (Just writeLimeSurveyLss)
  , FormatSpec "limesurvey-tsv" "LimeSurvey tab-separated survey structure" [".limesurvey.txt", ".limesurvey.tsv"] (Just readLimeSurveyTsv) (Just writeLimeSurveyTsv)
  , FormatSpec "msforms" "Microsoft Forms internal formapi JSON (undocumented)" [".msforms.json"] (Just readMSForms) (Just writeMSForms)
  , FormatSpec "redcap" "REDCap data dictionary CSV" [".redcap.csv", ".csv"] (Just readREDCap) (Just writeREDCap)
  , FormatSpec "triple-s" "Triple-S XML 2.0/3.0 data description" [".sss"] (Just readTripleS) (Just writeTripleS)
  , FormatSpec "ddi" "DDI Codebook 2.5 XML" [".ddi.xml"] (Just readDDI) (Just writeDDI)
  , FormatSpec "spss" "SPSS syntax data dictionary (labels, levels, MRSETS)" [".sps"] (Just readSpss) (Just writeSpss)
  , FormatSpec "odm" "CDISC ODM 1.3.2 metadata, with REDCap extensions (REDCap project XML)" [".odm.xml"] (Just readODM) (Just writeODM)
  , FormatSpec "qti" "IMS QTI 2.1 assessment test with embedded items" [".qti.xml"] (Just readQTI) (Just writeQTI)
  , FormatSpec "gift" "Moodle GIFT quiz text" [".gift"] (Just readGift) (Just writeGift)
  , FormatSpec "aiken" "Moodle Aiken single-choice quiz text" [".aiken.txt"] (Just readAiken) (Just writeAiken)
  , FormatSpec "markdown" "Printable questionnaire in Markdown (writer only; see questionpunk for lossless)" [".md", ".markdown"] Nothing (Just writeMarkdown)
  , FormatSpec "html" "Static printable HTML form (writer only)" [".html", ".htm"] Nothing (Just writeHtml)
  , FormatSpec "jspsych" "jsPsych timeline JSON (survey plugin or classic survey-* trials)" [".jspsych.json"] (Just readJsPsych) (Just writeJsPsych)
  , FormatSpec "surveydown" "surveydown Quarto survey (sd_question calls)" [".qmd"] (Just readSurveyDown) (Just writeSurveyDown)
  ]

lookupFormat :: String -> Maybe FormatSpec
lookupFormat n = find ((== map toLower n) . fmtName) formats

formatForPath :: FilePath -> Maybe FormatSpec
formatForPath p =
  let lp = map toLower p
   in snd <$> find ((`isSuffixOf` lp) . fst) (sortOn (Down . length . fst) [(e, f) | f <- formats, e <- fmtExtensions f])

-- | Read with one format and write with another.
convert :: FormatSpec -> FormatSpec -> BL.ByteString -> Either ConvError (BL.ByteString, [Warning])
convert from to input = runConv $ do
  r <- maybe (failWith "format has no reader") pure (fmtReader from)
  w <- maybe (failWith "format has no writer") pure (fmtWriter to)
  r input >>= w

-- | A versioned, machine-readable catalog for destination pickers. This is
-- offline conversion support, not a list of authenticated API integrations.
catalog :: Value
catalog = object
  [ "schema" .= ("panform.catalog/1" :: Text)
  , "mode" .= ("offline_conversion" :: Text)
  , "formats" .= map entry formats
  ]
  where
    entry f =
      let (support, fields, note) = designSupport (T.pack (fmtName f))
      in object
        [ "id" .= fmtName f
        , "description" .= fmtDescription f
        , "extensions" .= fmtExtensions f
        , "read" .= isJust (fmtReader f)
        , "write" .= isJust (fmtWriter f)
        , "design" .= object ["support" .= support, "fields" .= fields, "note" .= note]
        ]
