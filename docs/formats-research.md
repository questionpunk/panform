# Survey instrument interchange formats: a field guide for a pandoc-style converter

Scope: formats that define a questionnaire (questions, choices, logic, layout), not formats that carry response data. Every example encodes the same four-item instrument so the formats can be compared:

- **q1**: single choice, "Do you own a car?" (1 = Yes, 2 = No), required
- **q2**: multiple choice, "Which brands have you owned?" (A, B, C), shown only if q1 = Yes
- **q3**: free text, "Any comments?" (multi-line)
- **q4**: a Likert matrix, "Rate these" with rows r1 "Price" and r2 "Quality", columns 1 to 3 (Disagree, Neutral, Agree)

Confidence note: vendors document the syntax for the Qualtrics TXT formats, LimeSurvey TSV, XLSForm/XForms, REDCap, SurveyJS, Google Forms, Typeform, SurveyMonkey, Triple-S, GIFT/Aiken, FHIR and surveydown, and the examples below follow that documentation. I built the QSF, LSS, Tally and Jotform examples by hand from the structure of real exports, because those vendors publish no schema. Before writing a reader or writer for those four, check the examples against a real exported file. Specific uncertainties are flagged inline.

---

## 0. Summary matrix

| Format | Ext / MIME | Container | Pure to write? | Info richness | Public spec |
|---|---|---|---|---|---|
| Qualtrics QSF | `.qsf` / application/json | JSON | Yes | Very high (flow, logic, randomization, i18n, quotas, scoring) | No (reverse-engineered) |
| Qualtrics TXT Advanced | `.txt` / text/plain | tagged text | Yes (trivial) | Low-medium (types, recodes, blocks, page breaks; no logic) | Yes (support page) |
| Qualtrics TXT Simple | `.txt`/`.docx` | text | Yes | Low | Yes |
| LimeSurvey LSS | `.lss` / application/xml | XML (DB table dump) | Yes | Very high | Partly (schema = DB tables) |
| LimeSurvey TSV | `.txt` (tab-separated) | TSV | Yes (trivial) | High (same model as LSS) | Yes (manual) |
| SurveyJS | `.json` | JSON | Yes | Very high, open expression language | Yes (JSON schema + docs) |
| XLSForm | `.xlsx` (also `.xls`, `.csv`, `.md` in pyxform tests) | spreadsheet | xlsx needs zip+SpreadsheetML (the Hackage `xlsx` package covers it); CSV is trivial | High | Yes (xlsform.org) |
| ODK XForms | `.xml` | XML (W3C XForms subset + JavaRosa) | Yes | Very high (XPath logic, itext i18n) | Yes (getodk.github.io/xforms-spec) |
| REDCap data dictionary | `.csv` | CSV (18 fixed columns) | Yes (trivial) | Medium-high | Semi-official (in-app help) |
| REDCap project XML | `.xml` | CDISC ODM 1.3.x + `redcap:` extensions | Yes | High | ODM yes; extensions undocumented |
| Google Forms API | JSON (REST resource) | JSON | Yes, but creating a form needs `forms.create` then `batchUpdate` | Low-medium (jumps only, no i18n) | Yes |
| Typeform Create API | JSON | JSON | Yes | Medium-high (logic, variables, scoring) | Yes |
| SurveyMonkey v3 | JSON | JSON | Yes | Medium (the API does not expose skip logic well) | Yes (GitHub docs) |
| Triple-S | `.sss`/`.xml` + `.asc`/`.csv` data | XML | Yes | Low (variables and codes only, no logic or question text beyond labels) | Yes (triple-s.org) |
| DDI Codebook 2.5 | `.xml` | XML | Yes | Medium (variables, question text, categories; weak logic) | Yes (ddialliance.org) |
| DDI-Lifecycle 3.3 | `.xml` | XML | Yes, but verbose | Very high (instrument control constructs) | Yes |
| CDISC ODM 1.3/2.0 | `.xml` | XML | Yes | High (clinical CRFs) | Yes (cdisc.org) |
| HL7 FHIR Questionnaire | `.json`/`.xml` | JSON/XML | Yes | Very high (enableWhen, FHIRPath via SDC) | Yes (hl7.org/fhir) |
| Moodle GIFT | `.gift`/`.txt` | text | Yes | Low (quiz-oriented) | Yes (Moodle docs) |
| Aiken | `.txt` | text | Yes | Very low (single-answer quiz MC only) | Yes |
| Moodle XML | `.xml` | XML | Yes | Medium (quiz) | Yes |
| IMS QTI 2.1/3.0 | `.xml` inside IMS CP `.zip` | XML+zip | Items yes, packages need zip | High for assessment, awkward for surveys | Yes (1edtech) |
| Microsoft Forms | none | none | n/a | n/a | No public API for definitions |
| Tally | JSON (API) | JSON block list | Yes | Medium | Yes (developers.tally.so) |
| Jotform | JSON (API) | JSON, stringly-typed | Yes | Medium | Partly |
| oTree | `.py` | Python source | Write only | Medium | Docs only |
| jsPsych | `.js` | JS objects | Write only (or SurveyJS JSON) | Low (legacy plugins) / High (survey plugin = SurveyJS) | Docs |
| SPSS syntax | `.sps` | text | Yes | Low-medium (labels, value labels, MR sets, missing) | Yes (IBM command syntax ref) |
| SPSS `.sav` | `.sav` | binary records | Binary writer needed | Same as syntax plus types | Documented by GNU PSPP |
| surveydown | `.qmd` + `questions.yml` | Markdown + R calls | Yes | Medium | Docs |
| formr | `.xlsx`/Google Sheet | spreadsheet (XLSForm-like) | as XLSForm | Medium | Docs |
| Forsta/Decipher XML | `.xml` | XML | Yes | Very high (market research) | Vendor docs, login-gated |
| IBM SPSS Dimensions mrScriptMetadata | `.mdd` (XML) / mrScript text | text / XML | Text yes | Very high | IBM docs |
| Blaise | `.bla` | text DSL | Yes | Very high (CAPI/CATI) | Vendor docs |

---

## 1. Qualtrics

### 1a. QSF (Qualtrics Survey Format)

- **Extension/MIME**: `.qsf`, JSON (served as `application/octet-stream` on export). Qualtrics' public statement is "Do not edit the QSF file! Editing the file can corrupt the contents." No schema is published. Several open-source projects parse it: R `qsf`/`qualtRics` helpers, Python `QualtricsAPI`, `qsf2docx`-style tools, and `sjdaniel/qsf`. The v3 REST API `GET /survey-definitions/{id}` returns the same element model in a slightly different envelope.
- **Top-level shape**:

```json
{
  "SurveyEntry": { "SurveyID": "SV_xxx", "SurveyName": "Cars", "SurveyLanguage": "EN",
                   "SurveyStatus": "Inactive", "SurveyOwnerID": "...", ... },
  "SurveyElements": [
    { "SurveyID": "SV_xxx", "Element": "BL", "PrimaryAttribute": "Survey Blocks", "Payload": { ... } },
    { "Element": "FL", "PrimaryAttribute": "Survey Flow", "Payload": { ... } },
    { "Element": "SO", "PrimaryAttribute": "Survey Options", "Payload": { ... } },
    { "Element": "SQ", "PrimaryAttribute": "QID1", "SecondaryAttribute": "Do you own a car?", "Payload": { ... } },
    { "Element": "QC", "PrimaryAttribute": "Survey Question Count", "SecondaryAttribute": "4" },
    { "Element": "RS", ... }, { "Element": "SCO", ... }, { "Element": "STAT", ... }, { "Element": "PROJ", ... }
  ]
}
```

Element codes: `BL` blocks, `FL` flow, `SO` options (including `AvailableLanguages`), `SQ` one per question, `QC` count, `RS` response sets, `SCO` scoring, `QO`/`QG` quotas, `PL` preview link, `STAT`, `PROJ`, `NT` notes, `TR` triggers. Unknown element codes should be preserved raw.

- **Question payload** (the `SQ` Payload; example hand-built):

```json
{ "QuestionID": "QID1", "DataExportTag": "q1", "QuestionText": "Do you own a car?",
  "QuestionType": "MC", "Selector": "SAVR", "SubSelector": "TX",
  "Choices": { "1": { "Display": "Yes" }, "2": { "Display": "No" } },
  "ChoiceOrder": [ "1", "2" ], "RecodeValues": { "1": "1", "2": "2" },
  "Validation": { "Settings": { "ForceResponse": "ON", "ForceResponseType": "ON", "Type": "None" } },
  "Language": { "ES": { "QuestionText": "¿Tiene coche?", "Choices": { "1": { "Display": "Sí" }, "2": { "Display": "No" } } } } }

{ "QuestionID": "QID2", "DataExportTag": "q2", "QuestionText": "Which brands have you owned?",
  "QuestionType": "MC", "Selector": "MAVR", "SubSelector": "TX",
  "Choices": { "1": {"Display":"A"}, "2": {"Display":"B"}, "3": {"Display":"C"} }, "ChoiceOrder": ["1","2","3"],
  "DisplayLogic": { "0": { "0": { "LogicType": "Question", "QuestionID": "QID1", "QuestionIsInLoop": "no",
        "ChoiceLocator": "q://QID1/SelectableChoice/1", "Operator": "Selected",
        "QuestionIDFromLocator": "QID1", "LeftOperand": "q://QID1/SelectableChoice/1",
        "Type": "Expression", "Description": "..." }, "Type": "If" },
      "Type": "BooleanExpression", "inPage": false } }

{ "QuestionID": "QID3", "DataExportTag": "q3", "QuestionText": "Any comments?",
  "QuestionType": "TE", "Selector": "ML", "Validation": { "Settings": { "ForceResponse": "OFF", "Type": "None" } } }

{ "QuestionID": "QID4", "DataExportTag": "q4", "QuestionText": "Rate these",
  "QuestionType": "Matrix", "Selector": "Likert", "SubSelector": "SingleAnswer",
  "Choices": { "1": {"Display":"Price"}, "2": {"Display":"Quality"} },      // ROWS (statements)
  "Answers": { "1": {"Display":"Disagree"}, "2": {"Display":"Neutral"}, "3": {"Display":"Agree"} }, // COLUMNS
  "ChoiceOrder": ["1","2"], "AnswerOrder": ["1","2","3"], "ChoiceDataExportTags": { "1": "r1", "2": "r2" } }
```

- **Question types** (`QuestionType` / `Selector`): `MC` (SAVR, SAHR, SACOL, DL dropdown, SB select box, MSB multi-select box, MAVR, MAHR, MACOL, NPS), `Matrix` (Likert, Bipolar, TE, CS, RO, Profile, MaxDiff; SubSelector SingleAnswer/MultipleAnswer/DL), `TE` (SL, ML, ESTB essay, FORM, PW), `CS` constant sum, `RO` rank order (DND, RB, TB), `Slider` (HSLIDER, HBAR, STAR), `SBS` side-by-side, `DB` descriptive (TB, GRB), `DD` drill-down, `PGR` pick-group-rank, `HotSpot`, `HeatMap`, `Draw`, `FileUpload`, `Captcha`, `Timing`, `Meta`, `GAP`, `Highlight`, `SS` graphic slider. Treat the set as open.
- **Choices**: an object keyed by choice id string, with order in `ChoiceOrder`. The export value comes from `RecodeValues` (id to code) and the variable name from `VariableNaming`. Optional per-choice fields: `ExclusiveAnswer`, `TextEntry` ("other, specify"), `DisplayLogic`, and `Image`. In a matrix, `Choices` holds the rows and `Answers` the columns, the reverse of what most people expect.
- **Logic**:
  - `DisplayLogic` is a nested object keyed `"0","1",...`. Outer keys are OR groups and inner keys are conditions. Each condition has `Conjuction` (spelling as it appears in exports) `"And"`/`"Or"` for items after the first. Operands are locators: `q://QID1/SelectableChoice/1`, `q://QID1/ChoiceTextEntryValue`, `q://QID4/SelectableAnswer/1/3` for matrices, `e://Field/name` for embedded data. Operators include Selected, NotSelected, Displayed, EqualTo, GreaterThan, Contains, Empty, NotEmpty, MatchesRegex, and others.
  - `SkipLogic` is per question: `[{ "SkipLogicID": 1, "ChoiceLocator": "q://QID1/SelectableChoice/2", "Condition": "Selected", "SkipToDestination": "ENDOFBLOCK" | "ENDOFSURVEY" | "QID9", "Locator": ... }]`.
  - Survey flow (`FL` Payload): `{"Type":"Root","FlowID":"FL_1","Flow":[{"Type":"Block","ID":"BL_abc","FlowID":"FL_2"}, {"Type":"Branch","BranchLogic":{...same tree...},"Flow":[...]}, {"Type":"EmbeddedData","EmbeddedData":[{"Field":"src","Value":"..."}]}, {"Type":"BlockRandomizer","SubSet":1,"Flow":[...]}, {"Type":"Randomizer"}, {"Type":"Group"}, {"Type":"EndSurvey"}, {"Type":"Authenticator"}, {"Type":"WebService"}]}`.
  - Logic is therefore a DNF tree over typed atoms, not a text expression language.
- **Pages/blocks**: `BL` Payload is an object or array of blocks, each `{ "Type": "Default"|"Standard"|"Trash", "Description": "...", "ID": "BL_x", "BlockElements": [ {"Type":"Question","QuestionID":"QID1"}, {"Type":"Page Break"} ], "Options": { "BlockLocking", "RandomizeQuestions", "Looping": "Question", "LoopingOptions": {...} } }`. Blocks hold content and flow holds order, so the two are separate. The `Trash` block must be dropped on read.
- **i18n**: `SO.AvailableLanguages` (`{"EN":[], "ES":[]}`) plus a per-question `Language` map of overrides. Full translations are also available as separate XLIFF/CSV exports.
- **Piping**: `${q://QID1/ChoiceGroup/SelectedChoices}`, `${e://Field/name}`, `${lm://Field/1}` (loop and merge).
- **Verdict**: the most-requested format in academia and industry and very information-rich. Reading it is essential. Writing it is feasible, but Qualtrics rejects a QSF with inconsistent internal references (question counts, FlowIDs, block membership), so the writer must emit every cross-reference consistently.

### 1b. Qualtrics TXT import (Simple and Advanced)

- **Extension**: `.txt` (UTF-8) or `.docx` for Simple. Documented at qualtrics.com/support/.../import-and-export-surveys/.
- **Simple format rules** (from the doc): questions are separated by two blank lines. An optional export tag goes before the text as `Tag.` ("MC1. This is a multiple choice question"). Choices follow one blank line after the question. `[[MultipleAnswer]]` on the line after the question makes it multi-select. A question with no choices becomes a text/graphic question. A matrix is written as two groups of lines separated by a blank line: statements first, then scale points. `[[Block]]`/`[[Block:Name]]`, `[[PageBreak]]` and `[[Randomize]]` work as in the Advanced format. Simple format has no text-entry question type.
- **Advanced format** (the only one worth writing):

```
[[AdvancedFormat]]

[[Block:Main]]

[[Question:MC:SingleAnswer:Vertical]]
[[ID:q1]]
Do you own a car?
[[AdvancedChoices]]
[[Choice:1]]
Yes
[[Choice:2]]
No

[[Question:MC:MultipleAnswer:Vertical]]
[[ID:q2]]
Which brands have you owned?
[[Choices]]
A
B
C

[[PageBreak]]

[[Question:TE:Essay]]
[[ID:q3]]
Any comments?

[[Question:Matrix:SingleAnswer]]
[[ID:q4]]
Rate these
[[Choices]]
Price
Quality
[[AdvancedAnswers]]
[[Answer:1]]
Disagree
[[Answer:2]]
Neutral
[[Answer:3]]
Agree
```

- **Type tags**: `MC` (`SingleAnswer`/`MultipleAnswer` × `Vertical`/`Horizontal`, `Dropdown`, `Select`, `MultiSelect`), `Matrix` (`SingleAnswer`, `MultipleAnswer`), `TextEntry`/`TE` (`SingleLine`, `Essay`, `Form`, where Form's `[[Choices]]` are the form field labels), `ConstantSum`/`CS`, `RankOrder`/`RO`, `Text`/`DB` (descriptive). Other tags: `[[ID:x]]` (export tag), `[[Choice:recode]]`/`[[Answer:recode]]` for recode values, `[[ED:field]]`/`[[ED:field:value]]` for embedded data, `[[Block]]`, `[[PageBreak]]`, `[[Randomize]]`. Multi-line choice text is allowed only inside `[[AdvancedChoices]]`.
- **Missing**: display/skip logic, i18n, validation beyond type, per-choice export values when `[[Choices]]` is used.
- **Verdict**: trivial to write and a lossy but importable route into any Qualtrics account without API keys. It is worth a writer early, plus a reader, since people keep questionnaires in this form.

---

## 2. LimeSurvey

### 2a. LSS (XML)

- `.lss`, XML. The file is essentially a dump of LimeSurvey's database tables: each table becomes an element with `<fields><fieldname>…</fieldname></fields><rows><row>…</row></rows>`, and values are usually wrapped in CDATA. The schema changes with `DBVersion`. LS 4+ split the text into `*_l10ns` tables. Related formats: `.lsg` (one group), `.lsq` (one question), `.lsa` (a zip archive of survey plus responses).

```xml
<?xml version="1.0" encoding="UTF-8"?>
<document>
 <LimeSurveyDocType>Survey</LimeSurveyDocType>
 <DBVersion>600</DBVersion>
 <languages><language>en</language><language>es</language></languages>
 <groups><fields><fieldname>gid</fieldname><fieldname>sid</fieldname><fieldname>group_order</fieldname><fieldname>randomization_group</fieldname><fieldname>grelevance</fieldname></fields>
  <rows><row><gid><![CDATA[10]]></gid><sid><![CDATA[123]]></sid><group_order><![CDATA[1]]></group_order><grelevance><![CDATA[1]]></grelevance></row></rows></groups>
 <group_l10ns>…<row><gid>10</gid><group_name><![CDATA[Page 1]]></group_name><language>en</language></row>…</group_l10ns>
 <questions><fields>…qid, parent_qid, sid, gid, type, title, preg, other, mandatory, question_order, scale_id, same_default, relevance, modulename, encrypted…</fields>
  <rows>
   <row><qid>1</qid><parent_qid>0</parent_qid><gid>10</gid><type>L</type><title>q1</title><mandatory>Y</mandatory><other>N</other><relevance>1</relevance><question_order>1</question_order></row>
   <row><qid>2</qid><parent_qid>0</parent_qid><gid>10</gid><type>M</type><title>q2</title><relevance><![CDATA[q1 == "1"]]></relevance>…</row>
   <row><qid>3</qid>…<type>T</type><title>q3</title>…</row>
   <row><qid>4</qid>…<type>F</type><title>q4</title>…</row>
  </rows></questions>
 <subquestions>…<row><qid>5</qid><parent_qid>2</parent_qid><title>A</title><question_order>1</question_order><scale_id>0</scale_id></row>…
   <row><qid>8</qid><parent_qid>4</parent_qid><title>r1</title>…</row><row><qid>9</qid><parent_qid>4</parent_qid><title>r2</title>…</row></subquestions>
 <question_l10ns>…<row><qid>1</qid><question><![CDATA[Do you own a car?]]></question><help/><language>en</language></row>…</question_l10ns>
 <answers>…<row><aid>1</aid><qid>1</qid><code>1</code><sortorder>1</sortorder><scale_id>0</scale_id></row>…</answers>
 <answer_l10ns>…<row><aid>1</aid><answer><![CDATA[Yes]]></answer><language>en</language></row>…</answer_l10ns>
 <question_attributes>…<row><qid>3</qid><attribute>display_rows</attribute><value>5</value><language/></row>…</question_attributes>
 <conditions/> <surveys>…</surveys> <surveys_languagesettings>…surveyls_title, surveyls_welcometext, surveyls_endtext…</surveys_languagesettings>
</document>
```

(Hand-built. Field lists vary by DBVersion, so the reader should be tolerant: read `<fields>` dynamically and index rows by name.)

- **Question type codes**: `L` list (radio), `!` list (dropdown), `O` list with comment, `5` 5-point choice, `M` multiple choice, `P` multiple choice with comments, `F` array, `A` array 5-point, `B` array 10-point, `C` array Yes/No/Uncertain, `E` array Increase/Same/Decrease, `H` array by column, `1` array dual scale, `:` array numbers, `;` array texts, `S` short text, `T` long text, `U` huge text, `Q` multiple short text, `K` multiple numeric, `N` numeric, `D` date/time, `G` gender, `Y` yes/no, `R` ranking, `X` text display, `*` equation, `|` file upload, `I` language switch. LS 5+ adds theme variants through `question_theme_name` (bootstrap buttons, image select, ranking advanced). The base type letter stays the same, so keep the theme name as an attribute.
- **Choices**: answers (`A` rows, table `answers`) carry `code`, `sortorder`, `assessment_value` and `scale_id` (0 or 1; dual-scale arrays use both). Subquestions (`SQ`) are the rows of arrays and the items of multiple choice. Multiple choice stores `Y`/`""` per subquestion variable `q2_A` (the export uses `q2[A]`).
- **Logic**: ExpressionScript (formerly Expression Manager) in the `relevance` field of questions, groups (`grelevance`) and subquestions. The syntax is PHP/JS-like: `q1 == "1"`, `q2_A == "Y"`, `!is_empty(q3)`, `q1.NAOK == "1"`, `count(q2_A, q2_B)`, `if(a, b, c)`, `and`/`or`/`&&`/`||`, `regexMatch()`. Legacy `<conditions>` rows (cqid, cfieldname, method, value) still exist, and the reader should convert them. Validation goes in `preg` (a regex) and in the `em_validation_q` attribute.
- **Pages**: groups are pages under the default "group by group" format. The survey-level `format` is `G` (group), `S` (question-by-question) or `A` (all in one).
- **i18n**: the survey has a `language` plus `additional_languages`, and every text lives in `*_l10ns` or `surveys_languagesettings` rows keyed by language.
- **Spec**: none formal. The DB schema lives in the LimeSurvey source (`installer/create-database.php`) and the importer is `application/helpers/admin/import_helper.php`.

### 2b. LimeSurvey TSV (`.txt`)

- Tab-separated with a header row. Columns: `id`, `related_id`, `class`, `type/scale`, `name`, `relevance`, `text`, `help`, `language`, `validation`, `mandatory`, `other`, `default`, `same_default`, then advanced question-attribute columns in alphabetical order. `id` and `related_id` are only needed for quotas. The manual page is "Tab Separated Value survey structure".
- **Class codes**: `S` (survey global parameter; name = setting, text = value), `SL` (survey language parameter), `G` group, `Q` question (type/scale = type letter), `SQ` subquestion (type/scale = scale_id), `A` answer (type/scale = scale_id), `AS` assessment, `QTA`/`QTALS`/`QTAM` quotas, `C` condition.
- Nesting is **inferred from row order**: SQ and A rows belong to the preceding Q, and Q rows to the preceding G. The whole base language comes first, followed by a full translated copy for each additional language with identical names.

```
class	type/scale	name	relevance	text	help	language	validation	mandatory	other
S		language		en
S		additional_languages		es
SL		surveyls_title		Cars		en
G		Page 1	1			en
Q	L	q1	1	Do you own a car?		en		Y	N
A	0	1		Yes		en
A	0	2		No		en
Q	M	q2	q1 == "1"	Which brands have you owned?		en		N	N
SQ	0	A	1	A		en
SQ	0	B	1	B		en
SQ	0	C	1	C		en
Q	T	q3	1	Any comments?		en		N
Q	F	q4	1	Rate these		en		N
SQ	0	r1	1	Price		en
SQ	0	r2	1	Quality		en
A	0	1		Disagree		en
A	0	2		Neutral		en
A	0	3		Agree		en
```

(Hand-built from the documented columns. Verify the exact placement of the G row's name against an export.)

- **Verdict**: the easiest pure-text path into LimeSurvey and the same information as LSS. It is a good first LimeSurvey writer, and LSS reading should come soon after because most LimeSurvey instruments exist only as `.lss` files.

---

## 3. SurveyJS JSON

- `.json`. The public JSON schema is generated from the class metadata (`surveyjs_definition.json` in the survey-library repo; the "Survey JSON Schema" docs). The format is also used inside jsPsych's `plugin-survey`.

```json
{
  "title": { "default": "Cars", "es": "Coches" },
  "locale": "en",
  "pages": [
    { "name": "p1", "elements": [
      { "type": "radiogroup", "name": "q1", "title": { "default": "Do you own a car?", "es": "¿Tiene coche?" },
        "isRequired": true, "choices": [ { "value": 1, "text": "Yes" }, { "value": 2, "text": "No" } ] },
      { "type": "checkbox", "name": "q2", "title": "Which brands have you owned?", "visibleIf": "{q1} = 1",
        "choices": [ "A", "B", "C" ], "showNoneItem": false },
      { "type": "comment", "name": "q3", "title": "Any comments?" },
      { "type": "matrix", "name": "q4", "title": "Rate these",
        "columns": [ { "value": 1, "text": "Disagree" }, { "value": 2, "text": "Neutral" }, { "value": 3, "text": "Agree" } ],
        "rows": [ { "value": "r1", "text": "Price" }, { "value": "r2", "text": "Quality" } ] }
    ] }
  ]
}
```

- **Types**: `text` (with `inputType`: text, number, email, date, datetime-local, time, tel, url, range, color, password…), `comment`, `radiogroup`, `checkbox`, `dropdown`, `tagbox` (multi dropdown), `boolean`, `rating` (with `rateType`: labels/stars/smileys; `rateMin`, `rateMax`, `rateStep`, `rateValues`), `ranking`, `imagepicker`, `matrix` (single-choice grid), `matrixdropdown` (fixed rows, typed cells per column), `matrixdynamic` (roster), `multipletext`, `panel` (group), `paneldynamic` (repeat), `html`, `image`, `expression` (computed display), `file`, `signaturepad`, and newer additions (`slider`, `imagemap`, `customselect` in v2). Custom component types are registered at runtime, so unknown `type` values should become raw.
- **Choices**: a bare string, or `{value, text, visibleIf, enableIf, imageLink}`. Specials: `showNoneItem`, `showOtherItem` (`otherText`), `showSelectAllItem`, `choicesByUrl` (remote), `choicesFromQuestion` (+ `choicesFromQuestionMode`: selected/unselected/all).
- **Logic**: a string expression language. `{q1} = 1`, `{q2} contains 'A'`, `{q2} anyof ['A','B']`, `allof`, `notempty`, `empty`, `and`/`or`/`!`, arithmetic, and functions (`iif`, `age`, `today`, `sum`, `countInArray`, `isContainerReady`, custom). Matrix cells are addressed as `{q4.r1}`, and `{row.col}` works inside a matrix. It appears in `visibleIf`, `enableIf`, `requiredIf`, `setValueIf`, and page/panel `visibleIf`. `triggers: [{type:"complete"|"skip"|"setvalue"|"runexpression"|"copyvalue", expression, gotoName…}]` provides jumps. Top-level `calculatedValues: [{name, expression, includeIntoResult}]`.
- **Pages**: `pages[]`, with `panel`/`paneldynamic` for nesting. `questionsOnPageMode` can be "standard", "singlePage" or "questionPerPage". Also `questionsOrder: "random"`.
- **i18n**: any localizable string can be `{ "default": "...", "de": "..." }`.
- **Spec**: surveyjs.io/form-library/documentation (Survey JSON Schema).
- **Verdict**: the best open and richly documented JSON model. Its expression language is close to what a common IR needs. Implement early.

---

## 4. XLSForm (ODK, KoboToolbox, SurveyCTO, Ona, CommCare-ish)

- `.xlsx` (canonical) or `.xls`. pyxform also accepts `.csv` (sheets stacked in one CSV, each started by a row whose first cell is the sheet name) and markdown tables in its test suite. Spec: xlsform.org/en/ (reference) and docs.getodk.org/xlsform/. pyxform compiles XLSForm to XForms.
- **Sheets**: `survey` (required), `choices`, `settings`, and `entities` (ODK entities).

`survey`:

| type | name | label::English (en) | label::Español (es) | required | relevant | appearance | hint::English (en) |
|---|---|---|---|---|---|---|---|
| begin_group | p1 | Page 1 | Página 1 | | | field-list | |
| select_one yn | q1 | Do you own a car? | ¿Tiene coche? | yes | | | |
| select_multiple brands | q2 | Which brands have you owned? | ¿Qué marcas…? | | selected(${q1}, '1') | | |
| text | q3 | Any comments? | ¿Comentarios? | | | multiline | |
| end_group | | | | | | | |
| begin_group | q4 | Rate these | Califique | | | table-list | |
| select_one agree | r1 | Price | Precio | | | | |
| select_one agree | r2 | Quality | Calidad | | | | |
| end_group | | | | | | | |

`choices`:

| list_name | name | label::English (en) | label::Español (es) |
|---|---|---|---|
| yn | 1 | Yes | Sí |
| yn | 2 | No | No |
| brands | A | A | A |
| brands | B | B | B |
| brands | C | C | C |
| agree | 1 | Disagree | En desacuerdo |
| agree | 2 | Neutral | Neutral |
| agree | 3 | Agree | De acuerdo |

`settings`: `form_title`, `form_id`, `version`, `default_language`, `instance_name`, `style` (`pages` or `theme-grid` for Enketo), `public_key`, `submission_url`, `allow_choice_duplicates`.

- **Types**: `integer`, `decimal`, `range` (parameters `start= end= step=`), `text`, `select_one L`, `select_multiple L`, `select_one_from_file f.csv`, `select_multiple_from_file`, `rank L`, `note`, `geopoint`, `geotrace`, `geoshape`, `date`, `time`, `dateTime`, `image`, `audio`, `background-audio`, `video`, `file`, `barcode`, `calculate`, `acknowledge`, `hidden`, `xml-external`, `csv-external`, `begin_group`/`end_group` (also written `begin group`), `begin_repeat`/`end_repeat`, and metadata types (`start`, `end`, `today`, `deviceid`, `username`, `phonenumber`, `email`, `audit`, `start-geopoint`).
- **Other survey columns**: `hint`, `guidance_hint`, `constraint`, `constraint_message`, `required_message`, `default`, `read_only`, `calculation`, `trigger`, `choice_filter`, `parameters` (e.g. `randomize=true seed=${x}`), `repeat_count`, `media::image`, `media::audio`, `media::video`, `bind::…` and `body::…` passthrough columns (a built-in raw escape), `instance::…`.
- **Choices**: `list_name`, `name` (the stored value, no spaces), `label[::lang]`, `media::image`, plus arbitrary extra columns that `choice_filter` can use (`filter=${country}`).
- **Logic**: ODK XPath 1.0 with `${name}` references. Examples: `selected(${q2}, 'A')`, `count-selected()`, `${q1} = '1'`, `string-length()`, `if()`, `coalesce()`, `regex()`, `jr:choice-name()`, `indexed-repeat()`, `position(..)`, `once()`, `today()`, `decimal-date-time()`, `pulldata()`. `.` refers to the current value in `constraint`. Only display logic exists (`relevant`); there is no jump. `relevant` on a group applies to all its children.
- **Pages**: a `begin_group` with appearance `field-list` makes a page in Collect. Enketo with `style=pages` treats each field-list group as a page. Everything else is one question per screen (Collect) or one long form (Enketo default). Matrix: a `table-list` group of `select_one` sharing a list, or the older `label` / `list-nolabel` appearance trick. Likert styling uses the `likert` appearance on a single select_one.
- **i18n**: suffix any text column with `::Language Name (code)`. The recommended ODK form is `label::English (en)`, and `default_language` goes in settings.
- **Verdict**: the lingua franca of humanitarian, field and academic data collection, and very expressive. The CSV variant is trivially pure. For xlsx, Haskell's `xlsx` package (Codec.Xlsx) reads and writes it on top of `zip-archive`, so it is still easy. Implement early.

---

## 5. ODK XForms (XML)

- `.xml`, spec at getodk.github.io/xforms-spec/ (ODK XForms: a W3C XForms 1.0 subset plus JavaRosa extensions). Enketo, ODK Collect, KoboToolbox, SurveyCTO and CommCare all consume it.

```xml
<h:html xmlns="http://www.w3.org/2002/xforms" xmlns:h="http://www.w3.org/1999/xhtml"
        xmlns:jr="http://openrosa.org/javarosa" xmlns:odk="http://www.opendatakit.org/xforms">
 <h:head>
  <h:title>Cars</h:title>
  <model odk:xforms-version="1.0.0">
   <itext>
    <translation lang="English (en)" default="true()">
     <text id="/data/q1:label"><value>Do you own a car?</value></text>
     <text id="/data/q1/1:label"><value>Yes</value></text> …
    </translation>
    <translation lang="Español (es)"> … </translation>
   </itext>
   <instance>
    <data id="cars" version="2026092701">
     <q1/><q2/><q3/><q4><r1/><r2/></q4>
     <meta><instanceID/></meta>
    </data>
   </instance>
   <bind nodeset="/data/q1" type="string" required="true()"/>
   <bind nodeset="/data/q2" type="string" relevant="selected( /data/q1 , '1')"/>
   <bind nodeset="/data/q3" type="string"/>
   <bind nodeset="/data/q4/r1" type="string"/><bind nodeset="/data/q4/r2" type="string"/>
   <bind nodeset="/data/meta/instanceID" type="string" readonly="true()" jr:preload="uid"/>
  </model>
 </h:head>
 <h:body>
  <select1 ref="/data/q1"><label ref="jr:itext('/data/q1:label')"/>
    <item><label ref="jr:itext('/data/q1/1:label')"/><value>1</value></item>
    <item><label>No</label><value>2</value></item></select1>
  <select ref="/data/q2"><label>Which brands have you owned?</label>
    <item><label>A</label><value>A</value></item><item><label>B</label><value>B</value></item><item><label>C</label><value>C</value></item></select>
  <input ref="/data/q3" appearance="multiline"><label>Any comments?</label></input>
  <group ref="/data/q4" appearance="field-list"><label>Rate these</label>
    <select1 ref="/data/q4/r1" appearance="label"> …items… </select1>
    <select1 ref="/data/q4/r1" appearance="list-nolabel"><label>Price</label> …items… </select1>
    …
  </group>
 </h:body>
</h:html>
```

- **Controls**: `input`, `select1`, `select`, `odk:rank`, `upload` (mediatype image/audio/video/*), `trigger` (acknowledge), `range`, `group`, `repeat`. **Bind types**: `string`, `int`, `decimal`, `date`, `time`, `dateTime`, `geopoint`, `geotrace`, `geoshape`, `binary`, `barcode`, `boolean`. Bind attributes: `relevant`, `constraint`, `jr:constraintMsg`, `required`, `jr:requiredMsg`, `readonly`, `calculate`, `jr:preload`. Choice lists can be `<itemset nodeset="instance('brands')/root/item[filter]">` for secondary instances.
- **Logic**: XPath 1.0 over absolute or relative paths plus ODK functions. It is the same language as XLSForm with `${x}` resolved to paths.
- **i18n**: `<itext>` translations, referenced by `jr:itext('id')`. Media forms: `<value form="image">jr://images/x.png</value>`.
- **Verdict**: reading XForms lets the converter import anything already deployed (Kobo/ODK servers serve XForms, not XLSForms). Writing XForms directly duplicates pyxform. Only write it if the tool must deploy without pyxform, and otherwise emit XLSForm.

---

## 6. REDCap

### 6a. Data dictionary CSV

- `.csv` (UTF-8, comma), exactly these 18 columns in this order:

```
Variable / Field Name,Form Name,Section Header,Field Type,Field Label,"Choices, Calculations, OR Slider Labels",Field Note,Text Validation Type OR Show Slider Number,Text Validation Min,Text Validation Max,Identifier?,Branching Logic (Show field only if...),Required Field?,Custom Alignment,Question Number (surveys only),Matrix Group Name,Matrix Ranking?,Field Annotation
record_id,cars,,text,Record ID,,,,,,,,,,,,,
q1,cars,,radio,Do you own a car?,"1, Yes | 2, No",,,,,,,y,,,,,
q2,cars,,checkbox,Which brands have you owned?,"1, A | 2, B | 3, C",,,,,,[q1] = '1',,,,,,
q3,cars,,notes,Any comments?,,,,,,,,,,,,,
q4_r1,cars,Rate these,radio,Price,"1, Disagree | 2, Neutral | 3, Agree",,,,,,,,,,q4,,
q4_r2,cars,,radio,Quality,"1, Disagree | 2, Neutral | 3, Agree",,,,,,,,,,q4,,
```

- **Field types**: `text`, `notes`, `dropdown`, `radio`, `checkbox`, `yesno`, `truefalse`, `calc`, `file` (validation `signature` makes a signature field), `slider`, `descriptive`, `sql`. There is no separate date or number type. Those are `text` plus a Text Validation Type: `date_ymd`, `date_mdy`, `date_dmy`, `datetime_*`, `datetime_seconds_*`, `time`, `time_hh_mm_ss`, `integer`, `number`, `number_1dp`, `number_comma_decimal`, `email`, `phone`, `zipcode`, `alpha_only`, and others.
- **Choices**: `code, label | code, label`. Codes are integers or alphanumeric strings without spaces, and the first comma separates code from label. Slider "choices" are `left | middle | right` labels. `calc` puts its formula here.
- **Matrix**: consecutive rows with the same `Matrix Group Name` and identical choices render as a grid. The Section Header on the first row serves as the matrix title, and `Matrix Ranking? = y` prevents two rows sharing a column.
- **Branching logic**: `[q1] = '1'`, checkbox `[q2(1)] = '1'`, `<>`, `<`, `>=`, `and`, `or`, `not`, parentheses, and functions (`datediff([d1],[d2],"y")`, `sum()`, `if(c,a,b)`, `isblankormissingcode()`, `contains()`, `rounddown()`). Longitudinal projects use `[event_1_arm_1][q1]`, repeating instruments use `[q1][current-instance]`, and smart variables look like `[user-name]`. Only show-if exists, with no jumps. Survey-level stop actions are separate settings.
- **Pages/sections**: an instrument (`Form Name`) is the unit. In survey mode, a Section Header starts a new page when "display questions on separate pages" is enabled.
- **Field Annotation** holds Action Tags such as `@HIDDEN`, `@DEFAULT='1'`, `@CALCTEXT(...)`, `@NONEOFTHEABOVE='99'`, `@RANDOMORDER`, `@READONLY` and `@IF(cond, @TAG, @TAG)`. They carry real semantics and must be preserved.
- **i18n**: none in the dictionary. Multi-Language Management exports separate JSON/CSV/INI translation files.
- **Constraint**: the first field of the project must be the record ID.

### 6b. REDCap Project XML

CDISC ODM 1.3.1 with `redcap:` namespaced attributes (`redcap:Variable`, `redcap:FieldType`, `redcap:BranchingLogic`, `redcap:FieldAnnotation`, `redcap:MatrixGroupName`…). It holds the full project, including events and arms. Read this if full fidelity is needed; for most work, stick to the CSV.

- **Verdict**: the CSV is trivially pure, and REDCap has a very large academic and clinical base. Implement early.

---

## 7. Google Forms API (`forms.get` resource)

- JSON REST resource at developers.google.com/workspace/forms/api/reference/rest/v1/forms. There is no file format; "export" means calling the API.

```json
{
  "formId": "…",
  "info": { "title": "Cars", "documentTitle": "Cars", "description": "" },
  "settings": { "quizSettings": { "isQuiz": false }, "emailCollectionType": "DO_NOT_COLLECT" },
  "items": [
    { "itemId": "a1", "title": "Do you own a car?",
      "questionItem": { "question": { "questionId": "q1", "required": true,
        "choiceQuestion": { "type": "RADIO", "options": [
          { "value": "Yes" },
          { "value": "No", "goToSectionId": "s2" } ] } } } },
    { "itemId": "a2", "title": "Which brands have you owned?",
      "questionItem": { "question": { "choiceQuestion": { "type": "CHECKBOX",
        "options": [ {"value":"A"}, {"value":"B"}, {"value":"C"}, {"isOther": true} ] } } } },
    { "itemId": "s2", "title": "More", "pageBreakItem": {} },
    { "itemId": "a3", "title": "Any comments?",
      "questionItem": { "question": { "textQuestion": { "paragraph": true } } } },
    { "itemId": "a4", "title": "Rate these",
      "questionGroupItem": {
        "questions": [ { "rowQuestion": { "title": "Price" } }, { "rowQuestion": { "title": "Quality" } } ],
        "grid": { "columns": { "type": "RADIO",
          "options": [ {"value":"Disagree"}, {"value":"Neutral"}, {"value":"Agree"} ] } } } }
  ],
  "revisionId": "…", "responderUri": "…", "linkedSheetId": "…", "publishSettings": { … }
}
```

- **Item kinds**: `questionItem`, `questionGroupItem` (grid: RADIO for a multiple-choice grid, CHECKBOX for a checkbox grid), `pageBreakItem`, `textItem`, `imageItem`, `videoItem`. **Question kinds**: `choiceQuestion` (RADIO, CHECKBOX, DROP_DOWN; `shuffle`), `textQuestion` (`paragraph`), `scaleQuestion` (`low`, `high`, `lowLabel`, `highLabel`), `dateQuestion` (`includeTime`, `includeYear`), `timeQuestion` (`duration`), `fileUploadQuestion` (read-only in the API), `rowQuestion`, `ratingQuestion` (`ratingScaleLevel`, `iconType`).
- **Choices**: the label is the value (`value` string). There are no separate codes, which is lossy for research. `isOther`, `image`.
- **Logic**: only per-option branching on RADIO/DROP_DOWN options: `goToAction` (`NEXT_SECTION`, `RESTART_FORM`, `SUBMIT_FORM`) or `goToSectionId` (the itemId of a pageBreakItem). Page-level "after section, go to" is also exposed as `pageBreakItem`'s navigation, which can be limited through the API, so test it. There are no boolean expressions and no display logic.
- **Pages**: `pageBreakItem` starts a section. Items are a flat list.
- **i18n**: none.
- **Writing**: `forms.create` accepts only `info.title` (and `documentTitle`). All items must then be added with `forms.batchUpdate` `createItem` requests, whose `location.index` must be set. A writer therefore emits a request list rather than a document.
- **Quiz**: `grading` (`pointValue`, `correctAnswers`, feedback) on questions.
- **Verdict**: a low-information format with huge demand. The reader is easy. The writer targets `batchUpdate` JSON and needs a "lowering" pass that turns display logic into section jumps where possible.

---

## 8. Typeform Create API

- JSON, spec at typeform.com/developers/create/reference/create-form/.

```json
{
  "title": "Cars",
  "settings": { "language": "en", "progress_bar": "proportion", "show_progress_bar": true },
  "fields": [
    { "ref": "q1", "title": "Do you own a car?", "type": "multiple_choice",
      "properties": { "allow_multiple_selection": false, "randomize": false,
        "choices": [ { "ref": "yes", "label": "Yes" }, { "ref": "no", "label": "No" } ] },
      "validations": { "required": true } },
    { "ref": "q2", "title": "Which brands have you owned?", "type": "multiple_choice",
      "properties": { "allow_multiple_selection": true, "allow_other_choice": false,
        "choices": [ {"ref":"a","label":"A"}, {"ref":"b","label":"B"}, {"ref":"c","label":"C"} ] } },
    { "ref": "q3", "title": "Any comments?", "type": "long_text" },
    { "ref": "q4", "title": "Rate these", "type": "matrix",
      "properties": { "fields": [
        { "ref": "q4_r1", "title": "Price", "type": "multiple_choice",
          "properties": { "choices": [ {"ref":"c1","label":"Disagree"}, {"ref":"c2","label":"Neutral"}, {"ref":"c3","label":"Agree"} ] } },
        { "ref": "q4_r2", "title": "Quality", "type": "multiple_choice",
          "properties": { "choices": [ {"ref":"c1","label":"Disagree"}, {"ref":"c2","label":"Neutral"}, {"ref":"c3","label":"Agree"} ] } } ] } }
  ],
  "logic": [
    { "type": "field", "ref": "q1", "actions": [
      { "action": "jump", "details": { "to": { "type": "field", "value": "q3" } },
        "condition": { "op": "is", "vars": [ { "type": "field", "value": "q1" }, { "type": "choice", "value": "no" } ] } } ] }
  ],
  "thankyou_screens": [ { "ref": "end", "title": "Thanks!", "properties": { "show_button": false } } ],
  "hidden": [ "source" ],
  "variables": { "score": 0 }
}
```

(Matrix subfield shape per the reference: a matrix's `properties.fields` is an array of `multiple_choice` fields sharing choices. Confirm the exact required properties against a GET of a real matrix form.)

- **Types**: `short_text`, `long_text`, `multiple_choice`, `dropdown`, `picture_choice`, `yes_no`, `legal`, `rating`, `opinion_scale`, `nps`, `ranking`, `matrix`, `number`, `date`, `email`, `phone_number`, `website`, `file_upload`, `payment`, `statement`, `group`, `contact_info`, `signature`, `calendly`/`google_calendar`/`calendar`, `checkbox`, `multi_format` (video/audio answers). The set is open.
- **Logic**: `logic[]` per field (`type: "field"`) or for hidden fields (`type: "hidden"`). Each action is `jump` (to field/thankyou/outcome) or arithmetic on variables (`add`, `subtract`, `multiply`, `divide`, `set`). Conditions are an `op` tree: `and`/`or` nodes with `vars: [conditions]`, and leaves with ops `is`, `is_not`, `equal`, `not_equal`, `lower_than`, `lower_equal_than`, `greater_than`, `greater_equal_than`, `begins_with`, `ends_with`, `contains`, `not_contains`, `earlier_than`, `earlier_than_or_on`, `later_than`, `later_than_or_on`, `on`, `not_on`, `always`. Operands are `{type: field|hidden|variable|constant|choice, value}`. The model is jump-based, with no display-if.
- **Pages**: one question per screen. `group` fields nest (`properties.fields`), and `statement` fields serve as section text.
- **Piping**: `{{field:q1}}`, `{{hidden:source}}`, `{{var:score}}`.
- **i18n**: one language per form (`settings.language`). Translations are managed by a separate feature and API, so treat them as out of band. (Unverified: check whether the translation endpoints are public in your plan tier.)
- **Verdict**: medium-high information and a clean JSON. Its logic is jump-based, so it needs the jump/relevance conversion pass.

---

## 9. SurveyMonkey API v3 (survey details)

- JSON. `GET /v3/surveys/{id}/details` returns `pages[].questions[]`. Docs: api.surveymonkey.com/v3/docs and github.com/SurveyMonkey/public_api_docs (`includes/_questions.md`).

```json
{ "title": "Cars", "language": "en",
  "pages": [ { "title": "", "position": 1, "questions": [
    { "headings": [ { "heading": "Do you own a car?" } ], "position": 1,
      "family": "single_choice", "subtype": "vertical",
      "required": { "text": "This question requires an answer.", "type": "all", "amount": "0" },
      "answers": { "choices": [ { "text": "Yes", "position": 1 }, { "text": "No", "position": 2 } ] } },
    { "headings": [ { "heading": "Which brands have you owned?" } ],
      "family": "multiple_choice", "subtype": "vertical",
      "answers": { "choices": [ {"text":"A"}, {"text":"B"}, {"text":"C"} ],
                   "other": { "text": "Other (please specify)", "num_lines": 1, "num_chars": 50, "is_answer_choice": true } } },
    { "headings": [ { "heading": "Any comments?" } ], "family": "open_ended", "subtype": "essay" },
    { "headings": [ { "heading": "Rate these" } ], "family": "matrix", "subtype": "rating",
      "answers": { "rows": [ {"text":"Price"}, {"text":"Quality"} ],
                   "choices": [ {"text":"Disagree","weight":1}, {"text":"Neutral","weight":2}, {"text":"Agree","weight":3} ] } }
  ] } ] }
```

- **Families/subtypes**: `single_choice` (vertical, horiz, menu), `multiple_choice` (vertical, horiz), `matrix` (single, rating, ranking, menu, multi), `open_ended` (single, multi, numerical, essay), `demographic` (international, us), `datetime` (date_only, time_only, both), `presentation` (descriptive_text, image, video). `display_options.display_type` adds variants: `slider`, `file_upload`, `image_choice`, `emoji`, `star` (star rating), and `display_subtype`.
- **Answers**: `choices[]` (text, position, weight, visible, quiz_options), `rows[]` (matrix rows, or the fields of open_ended multi/numerical), `cols[]` (matrix menu: columns with nested choices), `other`. Returned objects carry numeric string `id`s. There are no user-set codes. `weight` is the closest thing to a code value.
- **Validation/required**: `required{type: all|at_least|at_most|exactly|range, amount}`, `validation{type: integer|decimal|email|regex|text_length|date_*, min, max, sum}`, `forced_ranking`, `sorting{type: default|textasc|textdesc|resp_count_*|random|flip, ignore_last}`.
- **Logic**: page and question skip logic exist in the product, but v3 details do not expose them usefully for round-tripping, and they cannot be set on create. Treat logic as lossy.
- **i18n**: one language per survey, with multilingual variants managed separately.
- **Verdict**: reader priority medium (the install base is large), writer low.

---

## 10. Triple-S XML

- `.sss` or `.xml` (the metadata) plus a data file (`.asc` fixed-width or `.csv`). Spec: triple-s.org (Triple-S XML 3.0 PDF, examples PDF). It is a market-research interchange standard for variables and labels, not routing.

```xml
<?xml version="1.0" encoding="UTF-8"?>
<sss version="3.0" languages="en es">
 <date>27 September 2026</date><time>12:00</time><origin>converter</origin>
 <survey>
  <name>cars</name>
  <title><text xml:lang="en">Cars</text><text xml:lang="es">Coches</text></title>
  <record ident="A" format="fixed">
   <variable ident="1" type="single">
    <name>q1</name>
    <label><text xml:lang="en">Do you own a car?</text><text xml:lang="es">¿Tiene coche?</text></label>
    <position start="1" finish="1"/>
    <values><value code="1">Yes</value><value code="2">No</value></values>
   </variable>
   <variable ident="2" type="multiple">
    <name>q2</name><label>Which brands have you owned?</label>
    <position start="2" finish="4"/>  <!-- bit-string form: one column per code -->
    <values><value code="1">A</value><value code="2">B</value><value code="3">C</value></values>
   </variable>
   <variable ident="3" type="character"><name>q3</name><label>Any comments?</label><position start="5" finish="504"/></variable>
   <variable ident="4" type="single"><name>q4_r1</name><label>Rate these: Price</label><position start="505" finish="505"/>
    <values><value code="1">Disagree</value><value code="2">Neutral</value><value code="3">Agree</value></values></variable>
   <variable ident="5" type="single"><name>q4_r2</name><label>Rate these: Quality</label><position start="506" finish="506"/>…</variable>
  </record>
 </survey>
</sss>
```

- **Types**: `single`, `multiple` (as a bit-string or with `<spread subfields="n" width="w"/>` for code lists), `quantity` (with `<values><range from to/></values>`), `character`, `logical`, `date`, `time`. Other attributes and elements: `use="serial"|"weight"`, `<size>` (CSV format), `<filter>` (a free-text description, not executable), and hierarchical records in 2.0+ (`<hierarchy>`).
- **Logic**: none executable. **Pages**: none. There is no matrix concept: a grid is flattened to one variable per row.
- **i18n**: `languages` attribute plus `<text xml:lang>` in labels (2.0+).
- **Verdict**: a writer is cheap and useful for market-research hand-off, but it is output only. A reader yields only a codebook-grade IR.

---

## 11. DDI (Codebook 2.5 and Lifecycle 3.x)

Spec: ddialliance.org (DDI-C 2.5 XSD and field-level documentation; DDI-L 3.3). Archives use DDI: ICPSR, UKDS, IPUMS, the NADA/World Bank microdata library, and Nesstar. Colectica and Questasy author DDI-L.

**Codebook 2.5** (variable-centric; question text hangs off variables):

```xml
<codeBook xmlns="ddi:codebook:2_5" version="2.5">
 <stdyDscr><citation><titlStmt><titl>Cars</titl></titlStmt></citation></stdyDscr>
 <dataDscr>
  <var ID="V1" name="q1">
   <labl>Owns car</labl>
   <qstn responseDomainType="category"><qstnLit>Do you own a car?</qstnLit></qstn>
   <catgry><catValu>1</catValu><labl>Yes</labl></catgry>
   <catgry><catValu>2</catValu><labl>No</labl></catgry>
  </var>
  <var ID="V2" name="q2_A"><labl>Brand A</labl>
   <qstn responseDomainType="multiple"><preQTxt>Asked if q1 = 1</preQTxt><qstnLit>Which brands have you owned? A</qstnLit></qstn>
   <universe>Car owners (q1 = 1)</universe>
   <catgry><catValu>0</catValu><labl>Not selected</labl></catgry><catgry><catValu>1</catValu><labl>Selected</labl></catgry></var>
  <!-- q2_B, q2_C … -->
  <var ID="V5" name="q3"><qstn responseDomainType="text"><qstnLit>Any comments?</qstnLit></qstn></var>
  <var ID="V6" name="q4_r1"><qstn><preQTxt>Rate these</preQTxt><qstnLit>Price</qstnLit></qstn> …catgry…</var>
  <varGrp ID="G1" type="multipleResp" var="V2 V3 V4"><labl>Brands owned</labl></varGrp>
  <varGrp ID="G2" type="grid" var="V6 V7"><labl>Rate these</labl></varGrp>
 </dataDscr>
</codeBook>
```

- `qstn` children: `preQTxt`, `qstnLit`, `postQTxt`, `forward`/`backward` (skip references as IDREFs, `<forward qstn="Q5">`), `ivuInstr`. `responseDomainType`: text, numeric, code, category, datetime, geographic, multiple, other. `universe` is descriptive text, not executable logic. Every element accepts `xml:lang`, and elements may be repeated per language.
- **DDI-Lifecycle 3.3** (instrument-centric) has `QuestionItem` (with `QuestionText/LiteralText`, `CodeDomain` referencing a `CodeList` of `Code` → `Category`, `TextDomain`, `NumericDomain`, `DateTimeDomain`), `QuestionGrid` (with `GridDimension rank="1|2"` and code domains, which is a real matrix), `StatementItem`, and a `ControlConstruct` scheme with `Sequence`, `IfThenElse` (`IfCondition/Command/CommandCode` with a `ProgramLanguage` attribute, e.g. an executable condition in any language), `Loop`, `RepeatUntil`, `RepeatWhile`, `QuestionConstruct` (placing a question in the flow), `ComputationItem`. Every element is identified by URN (agency:id:version) and heavily referenced.
- **Verdict**: DDI-C writer = medium priority for archival deposit (the flattened codebook). DDI-L = high information but high implementation cost. Treat it as a later reader/writer.

---

## 12. Moodle GIFT and Aiken

**GIFT** (`.gift`/`.txt`, docs.moodle.org/en/GIFT_format):

```
// Cars survey (GIFT is quiz-oriented; "survey" semantics via all-correct or weighted answers)
$CATEGORY: cars

::q1:: Do you own a car? {
  =Yes
  ~No
}

::q2:: Which brands have you owned? {
  ~%33.33333%A
  ~%33.33333%B
  ~%33.33333%C
}

::q3:: Any comments? {}

::q4:: Rate these (match each item to a level) {
  =Price -> Neutral
  =Quality -> Agree
}
```

- **Types**: multiple choice (`=` right, `~` wrong, `~%n%` weighted, which makes it multi-answer when using weights), true/false (`{T}`/`{F}`), short answer (all `=`), matching (`=a -> b`), missing word (braces mid-sentence), numerical (`{#3:0.5}`, `{#1..5}`), essay (`{}`), description (no braces). Feedback is `#text` and general feedback is `####`. Markup: `[html]`, `[markdown]`, `[moodle]`, `[plain]`. Escape the special characters `~ = # { } :` with a backslash.
- A survey maps poorly: there is no logic, no pages (apart from `$CATEGORY`), and no real Likert matrix (matching requires a correct answer). The format is useful for exporting items to LMS quizzes.

**Aiken** (`.txt`):

```
Do you own a car?
A. Yes
B. No
ANSWER: A
```

- Single-answer MC only, and `ANSWER:` is mandatory. Options are `A.` or `A)`. Everything else is lost. Low value: a writer is a 20-line exercise, and the reader should only exist for completeness.

**Moodle XML** (`<quiz><question type="multichoice">…`) and the **Moodle Feedback/Questionnaire** plugins' XML exports are better survey targets than GIFT if LMS support matters.

---

## 13. IMS QTI 2.1 / 3.0

- XML items (`assessmentItem`; in 3.0, kebab-case `qti-assessment-item`) inside an IMS Content Package zip with `imsmanifest.xml`. Spec: 1edtech.org/standards/qti.

```xml
<qti-assessment-item identifier="q1" title="q1" adaptive="false" time-dependent="false"
   xmlns="http://www.imsglobal.org/xsd/imsqtiasi_v3p0">
 <qti-response-declaration identifier="RESPONSE" cardinality="single" base-type="identifier"/>
 <qti-item-body>
  <qti-choice-interaction response-identifier="RESPONSE" max-choices="1">
   <qti-prompt>Do you own a car?</qti-prompt>
   <qti-simple-choice identifier="yes">Yes</qti-simple-choice>
   <qti-simple-choice identifier="no">No</qti-simple-choice>
  </qti-choice-interaction>
 </qti-item-body>
</qti-assessment-item>
```

- Multi-select is `cardinality="multiple"` with `max-choices="0"`, free text is `qti-extended-text-interaction`, and a matrix is `qti-match-interaction` (two `qti-simple-match-set`s, `cardinality="multiple"`, `base-type="directedPair"`) or a `qti-choice-interaction` per row. Tests (`qti-assessment-test` → `qti-test-part` → `qti-assessment-section`) carry `qti-pre-condition` and `qti-branch-rule` with QTI expression XML (`<qti-match><qti-variable identifier="q1.RESPONSE"/><qti-base-value base-type="identifier">yes</qti-base-value></qti-match>`). QTI 3 has shared-vocabulary CSS classes for Likert display.
- **Verdict**: not worth supporting early. It is assessment-first and verbose, and packaging requires a zip. Consider it only if an education customer asks. A single-item writer is easy if it is ever needed.

---

## 14. HL7 FHIR Questionnaire (worth adding: health and clinical)

- `application/fhir+json` or `+xml`. Spec: hl7.org/fhir/questionnaire.html (R4 and R5), plus Structured Data Capture (SDC) IG hl7.org/fhir/uv/sdc for rendering, calculated values and adaptive forms.

```json
{ "resourceType": "Questionnaire", "status": "draft", "title": "Cars", "language": "en",
  "item": [
    { "linkId": "q1", "text": "Do you own a car?", "type": "choice", "required": true,
      "answerOption": [ { "valueCoding": { "code": "1", "display": "Yes" } }, { "valueCoding": { "code": "2", "display": "No" } } ] },
    { "linkId": "q2", "text": "Which brands have you owned?", "type": "choice", "repeats": true,
      "enableWhen": [ { "question": "q1", "operator": "=", "answerCoding": { "code": "1" } } ],
      "answerOption": [ { "valueCoding": { "code": "A", "display": "A" } }, { "valueCoding": { "code": "B", "display": "B" } }, { "valueCoding": { "code": "C", "display": "C" } } ] },
    { "linkId": "q3", "text": "Any comments?", "type": "text" },
    { "linkId": "q4", "text": "Rate these", "type": "group",
      "extension": [ { "url": "http://hl7.org/fhir/StructureDefinition/questionnaire-itemControl",
        "valueCodeableConcept": { "coding": [ { "system": "http://hl7.org/fhir/questionnaire-item-control", "code": "gtable" } ] } } ],
      "item": [
        { "linkId": "q4.r1", "text": "Price", "type": "choice", "answerValueSet": "#agree3" },
        { "linkId": "q4.r2", "text": "Quality", "type": "choice", "answerValueSet": "#agree3" } ] } ] }
```

- **Types (R4)**: group, display, boolean, decimal, integer, date, dateTime, time, string, text, url, choice, open-choice, attachment, reference, quantity. R5 renames `choice` to `coding` and adds `answerConstraint` (optionsOnly/optionsOrType/optionsOrString) in place of open-choice.
- **Logic**: `enableWhen[]` (`question`, `operator` exists/=/!=/>/</>=/<=, `answer[x]`) with `enableBehavior` all/any. SDC adds `enableWhenExpression`, `calculatedExpression` and `answerExpression` in FHIRPath or CQL, plus `variable` extensions.
- **i18n**: the `translation` extension on each string's `_text` primitive element.
- **Verdict**: a clean, strongly specified, lossless-ish target, and the obvious choice for health research and EHR integration. Medium priority.

## 15. CDISC ODM 1.3 / 2.0 (clinical)

- XML, cdisc.org/standards/data-exchange/odm. `Study/MetaDataVersion` holds `FormDef` → `ItemGroupRef` → `ItemGroupDef` → `ItemRef` (with `Mandatory`, `CollectionExceptionConditionOID`) → `ItemDef` (`DataType`, `<Question><TranslatedText xml:lang>`, `CodeListRef`) plus `CodeList/CodeListItem CodedValue/Decode`. `ConditionDef`/`MethodDef` carry `FormalExpression Context="…"`. REDCap, OpenClinica and most EDCs exchange instruments this way. The format suits a writer once the IR is stable.

---

## 16. Microsoft Forms

- **No public API or documented file format for form definitions.** Microsoft Graph has no Forms endpoints. The web app uses an undocumented internal API (`forms.office.com/formapi/api/...`), which is unstable and requires tenant auth. The only exports are responses to Excel. Import runs through "Quick Import" from Word/PDF (AI-parsed), so the practical way in is to write a well-structured `.docx` or plain text.
- **Verdict**: skip as a reader and writer. At most, emit a clean DOCX or TXT layout optimized for Quick Import.
- **Update (2026-09-27): implemented as `msforms`** against the de-facto formapi JSON, verified from three real captured form definitions and two independent sets of captured request bodies (provenance in `examples/msforms/SOURCE.md`; the three third-party captures were later replaced in `examples/msforms/` with synthetic forms in the same shape). The Forms UI still cannot import it; tools that call the formapi endpoints can.

## 17. Tally

- JSON via the Tally API (developers.tally.so, `POST /forms`, `GET /forms/{id}`). A form is a flat, Notion-like `blocks[]`, each block `{uuid, type, groupUuid, groupType, payload}`. A question is a run of blocks sharing a `groupUuid`: typically a `TITLE` block (the question text), then input blocks. A radio question is several `MULTIPLE_CHOICE_OPTION` blocks with `groupType: "MULTIPLE_CHOICE"` and `payload: {index, text, isFirst, isLast}`. Block types: FORM_TITLE, TITLE, TEXT, LABEL, HEADING_1/2/3, IMAGE, EMBED, EMBED_VIDEO, EMBED_AUDIO, DIVIDER, PAGE_BREAK, INPUT_TEXT, TEXTAREA, INPUT_NUMBER, INPUT_EMAIL, INPUT_LINK, INPUT_PHONE_NUMBER, INPUT_DATE, INPUT_TIME, MULTIPLE_CHOICE_OPTION, CHECKBOX, DROPDOWN_OPTION, RANKING_OPTION, MULTI_SELECT_OPTION, LINEAR_SCALE, RATING, MATRIX, MATRIX_ROW, MATRIX_COLUMN, FILE_UPLOAD, SIGNATURE, PAYMENT, HIDDEN_FIELDS, CONDITIONAL_LOGIC, CALCULATED_FIELDS, CAPTCHA, RESPONDENT_COUNTRY. Logic is itself a `CONDITIONAL_LOGIC` block whose payload holds conditionals and actions (show/hide/jump/require/calculate). Pages are delimited by `PAGE_BREAK` blocks. There is no i18n.
- **Verdict**: a medium-information, awkward (block-run) encoding. Low priority. Build it by adapting the Google Forms reader and writer approach.

## 18. Jotform

- JSON via the REST API. `GET /form/{id}/questions` returns `content: { "<qid>": {qid, type, text, order, name, required: "Yes"|"No", options: "A|B|C", ...} }`. Every value is a string, and options are pipe-separated. Types are `control_*`: `control_head`, `control_text` (paragraph), `control_textbox`, `control_textarea`, `control_radio`, `control_checkbox`, `control_dropdown`, `control_matrix` (`mrows: "Price|Quality"`, `mcolumns: "Disagree|Neutral|Agree"`, `inputType: "Radio Button"`), `control_scale`, `control_rating`, `control_datetime`, `control_number`, `control_email`, `control_phone`, `control_fullname`, `control_address`, `control_fileupload`, `control_pagebreak`, `control_widget`, `control_button`, and others. Conditions come from `GET /form/{id}/properties` → `conditions[]`, whose `terms` and `action` fields are **JSON-encoded strings** (`{"type":"field","link":"Any","terms":"[{\"field\":\"3\",\"operator\":\"equals\",\"value\":\"Yes\"}]","action":"[{\"visibility\":\"Show\",\"field\":\"5\"}]"}`). Pages are `control_pagebreak`. Translations are a separate feature.
- **Verdict**: low priority. Stringly-typed, and choice labels double as values. (Hand-built from observed API output. Verify before implementing.)

## 19. oTree

- Python (`__init__.py` in oTree 5 "no self" format). Fields are declared on `Player`, and pages list `form_fields`. Write-only makes sense, since reading requires parsing Python.

```python
from otree.api import *

class Player(BasePlayer):
    q1 = models.IntegerField(label="Do you own a car?", choices=[[1, "Yes"], [2, "No"]],
                             widget=widgets.RadioSelect)
    q2_A = models.BooleanField(label="A", blank=True, widget=widgets.CheckboxInput)
    q2_B = models.BooleanField(label="B", blank=True, widget=widgets.CheckboxInput)
    q2_C = models.BooleanField(label="C", blank=True, widget=widgets.CheckboxInput)
    q3 = models.LongStringField(label="Any comments?", blank=True)
    q4_r1 = models.IntegerField(label="Price", choices=[[1,"Disagree"],[2,"Neutral"],[3,"Agree"]],
                                widget=widgets.RadioSelectHorizontal)
    q4_r2 = models.IntegerField(label="Quality", choices=[[1,"Disagree"],[2,"Neutral"],[3,"Agree"]],
                                widget=widgets.RadioSelectHorizontal)

class P1(Page):
    form_model = "player"
    form_fields = ["q1"]

class P2(Page):
    form_model = "player"
    form_fields = ["q2_A", "q2_B", "q2_C"]
    @staticmethod
    def is_displayed(player): return player.q1 == 1

class P3(Page):
    form_model = "player"
    form_fields = ["q3", "q4_r1", "q4_r2"]

page_sequence = [P1, P2, P3]
```

- Field types: `IntegerField`, `FloatField`, `CurrencyField`, `BooleanField`, `StringField`, `LongStringField`. Options: `choices`, `min`, `max`, `blank`, `initial`, `widget` (RadioSelect, RadioSelectHorizontal, CheckboxInput). Multi-select needs one Boolean per option. Logic is page-level (`is_displayed`) Python, plus `error_message` validation. i18n uses Django gettext. A matrix needs a custom template.
- **Verdict**: a low-priority writer for behavioral-economics users.

## 20. jsPsych

- **Legacy plugins** (`plugin-survey-multi-choice`, `-multi-select`, `-text`, `-likert`, `-html-form`) take JS objects:

```js
const q1 = { type: jsPsychSurveyMultiChoice, questions: [
  { prompt: "Do you own a car?", name: "q1", options: ["Yes", "No"], required: true } ] };
const q2 = { timeline: [{ type: jsPsychSurveyMultiSelect, questions: [
  { prompt: "Which brands have you owned?", name: "q2", options: ["A","B","C"] } ] }],
  conditional_function: () => jsPsych.data.get().last(1).values()[0].response.q1 === "Yes" };
const q3 = { type: jsPsychSurveyText, questions: [{ prompt: "Any comments?", name: "q3", rows: 5 }] };
const q4 = { type: jsPsychSurveyLikert, preamble: "Rate these", questions: [
  { prompt: "Price", name: "r1", labels: ["Disagree","Neutral","Agree"] },
  { prompt: "Quality", name: "r2", labels: ["Disagree","Neutral","Agree"] } ] };
```

- **The current `@jspsych/plugin-survey`** is `{ type: jsPsychSurvey, survey_json: <SurveyJS JSON> }`, optionally with `survey_function`. Once a SurveyJS writer exists, a jsPsych writer is nearly free.
- **Verdict**: a thin writer wrapping SurveyJS output.

## 21. SPSS syntax and `.sav` metadata

- **Syntax** (`.sps`, text; IBM Command Syntax Reference):

```
VARIABLE LABELS q1 'Do you own a car?' /q2_A 'Which brands: A' /q2_B 'Which brands: B' /q2_C 'Which brands: C'
  /q3 'Any comments?' /q4_r1 'Rate these: Price' /q4_r2 'Rate these: Quality'.
VALUE LABELS q1 1 'Yes' 2 'No'
  /q2_A q2_B q2_C 0 'Not selected' 1 'Selected'
  /q4_r1 q4_r2 1 'Disagree' 2 'Neutral' 3 'Agree'.
MISSING VALUES q1 q4_r1 q4_r2 (99).
VARIABLE LEVEL q1 (NOMINAL) /q4_r1 q4_r2 (ORDINAL).
MRSETS /MDGROUP NAME=$q2 LABEL='Which brands have you owned?' VARIABLES=q2_A q2_B q2_C VALUE=1
  CATEGORYLABELS=VARLABELS.
```

- **`.sav`**: binary records. Type 2 is a variable (width, print/write formats, 8-byte short name), type 3+4 are value labels, type 6 holds documents, and type 7 subtypes hold extensions: 3 (machine int info), 11 (measure/width/alignment), 13 (long variable names), 14 (very long strings), 7/19 (MR sets), 20 (encoding), 21/22 (long-string value labels and missing values). GNU PSPP documents the layout (www.gnu.org/software/pspp/pspp-dev/html_node/System-File-Format.html). Writing is straightforward with `Data.ByteString.Builder`, but it is binary. The same metadata can be emitted as syntax, R `haven` labels, or Stata `label define`.
- **Verdict**: writer for syntax (cheap), `.sav` later. These are codebook targets with no questionnaire logic.

## 22. Plain-text / markdown-ish formats

- **surveydown** (R/Quarto; pkg.surveydown.org; PLOS One 2025). The `survey.qmd` pages are Quarto divs (`::: {.sd_page id=p1} … :::`), and questions are R calls `sd_question(id, type, label, option = c("Yes" = "1", "No" = "2"), row = c(...))`, with an optional `questions.yml`. Types: `select`, `mc`, `mc_multiple`, `mc_buttons`, `mc_multiple_buttons`, `mc_image`, `mc_multiple_image`, `text`, `textarea`, `numeric`, `slider`, `slider_numeric`, `date`, `daterange`, `matrix`, `matrix_multiple`. Logic is R code in `app.R`: `sd_show_if(input$q1 == "1" ~ "q2")` and `sd_skip_if(... ~ "page_id")`, where the target is a page or question id. Note that `option` is `label = value` (names are labels), the reverse of most formats.

```
::: {.sd_page id=p1}
```{r}
sd_question(id = "q1", type = "mc", label = "Do you own a car?", option = c("Yes" = "1", "No" = "2"))
sd_question(id = "q2", type = "mc_multiple", label = "Which brands have you owned?", option = c("A"="A","B"="B","C"="C"))
sd_question(id = "q3", type = "textarea", label = "Any comments?")
sd_question(id = "q4", type = "matrix", label = "Rate these",
            row = c("Price" = "r1", "Quality" = "r2"),
            option = c("Disagree" = "1", "Neutral" = "2", "Agree" = "3"))
sd_next()
```
:::
```

- **formr** (formr.org) uses an XLSForm-like item table (Google Sheet/xlsx). Survey columns are `type`, `name`, `label`, `optional`, `showif`, `value`, `class`, `block_order`, `item_order`, and a `choices` sheet has `list_name`/`name`/`label`. Types include `note`, `text`, `textarea`, `number`, `mc`, `mc_multiple`, `mc_button`, `mc_multiple_button`, `rating_button`, `check`, `select_one`, `select_multiple`, `date`, `email`, `range`, `calculate`, `submit` (the page break). `showif` and `value` are **R expressions** (`q1 == 1`). A reader/writer can share most code with XLSForm.
- **Qualtrics TXT Advanced**, section 1b above, is the most widely used text-based survey format.
- **IBM SPSS Dimensions mrScriptMetadata** (the MDM text language behind `.mdd`; Unicom Intelligence). Mature, very expressive, and plain text:

```
Metadata(en-US, Question, label)
    q1 "Do you own a car?" categorical [1..1] { yes "Yes", no "No" };
    q2 "Which brands have you owned?" categorical [1..] { A "A", B "B", C "C" };
    q3 "Any comments?" text;
    q4 "Rate these" loop { r1 "Price", r2 "Quality" }
        fields ( rating "" categorical [1..1] { disagree "Disagree", neutral "Neutral", agree "Agree" }; )
        expand grid;
End Metadata
Routing(Web)
    q1.Ask()
    If q1.ContainsAny({yes}) Then q2.Ask()
    q3.Ask()
    q4.Ask()
End Routing
```

- **Forsta (Decipher) XML**, dominant in market research. `<radio label="q1"><title>…</title><row label="r1" value="1">Yes</row>…</radio>`, `<checkbox label="q2" atleast="1" cond="q1.r1">`, `<textarea label="q3">`, and `<radio label="q4" type="rating"><row …/><col …/></radio>` for grids. `<suspend/>` is a page break, `<block>`/`<loop>` handle structure, and `cond=` takes a Python expression. Docs are login-gated, but the syntax is widely known.
- **Blaise 4/5** (Statistics Netherlands; used by national statistics offices for CAPI/CATI): `FIELDS q1 "Do you own a car?" : (Yes "Yes", No "No")`, `q2 : SET OF (A, B, C)`, `q3 : STRING[500]`, and a `RULES` section with `q1  IF q1 = Yes THEN q2 ENDIF`. Multi-language through `LANGUAGES =` and parallel texts. Reader value is high in official statistics, but the grammar is large.
- Also: **Formbricks** (open-source survey JSON: `questions[]` of `openText`, `multipleChoiceSingle`, `multipleChoiceMulti`, `rating`, `nps`, `matrix`, `ranking`, `cta`, `consent`…, i18n `{default, de}`, jump `logic[]`), **Form.io** JSON components, the World Bank's **Survey Solutions** questionnaire JSON (C#-expression logic, rosters), **CSPro** dictionaries (`.dcf`), **SoSci Survey** XML, **PsychoPy/Pavlovia Forms** (xlsx/csv item lists: `itemText`, `type`, `options`, `ticks`, `tickLabels`), **Gorilla**, and **Inquisit**. None is essential. Formbricks and Survey Solutions are the most plausible next-tier readers.

---

## 23. Information richness and write-purity rankings

**Most information (best round-trip sources):** QSF ≈ LSS/LimeSurvey TSV ≈ SurveyJS ≈ Forsta XML ≈ Dimensions MDD > XForms ≈ XLSForm ≈ FHIR+SDC ≈ DDI-L > Typeform > REDCap dictionary > CDISC ODM > SurveyMonkey > Google Forms > Tally ≈ Jotform > DDI-C ≈ Triple-S ≈ SPSS (codebook only) > Qualtrics TXT > GIFT > Aiken.

**Pure to write (no zip/binary)**: every JSON/XML/CSV/TSV/text format above. The exceptions:
- XLSForm `.xlsx` needs a zip plus SpreadsheetML. Use the `xlsx` package, or emit the pyxform CSV variant.
- QTI packages and LimeSurvey `.lsa` need zips.
- REDCap instrument zips (`instrument.csv` + `OriginID.txt`) need zips.
- SPSS `.sav` and Stata `.dta` are binary.
- Google Forms, Typeform and SurveyMonkey writers target API payloads rather than files, and Google additionally needs a two-phase create/batchUpdate.

**Logic families** (this determines lossiness):
1. *Show-if / relevance expressions* (text DSLs): XLSForm/XForms (XPath), LimeSurvey (ExpressionScript), SurveyJS, REDCap, formr (R), FHIR-SDC (FHIRPath), CDISC ODM (FormalExpression), Forsta (Python), oTree (Python).
2. *Structured condition trees*: QSF (DNF of atoms with locators), Typeform (op/vars tree), FHIR `enableWhen`, Jotform, Tally.
3. *Jump / goto*: Google Forms (option → section), Typeform `jump`, SurveyMonkey skip logic, QSF `SkipLogic`, SurveyJS `triggers: skip`, Blaise/Dimensions routing (procedural).
4. *None*: Triple-S, DDI-C (descriptive only), SPSS, GIFT, Aiken, Qualtrics TXT.

---

## 24. Recommended intermediate representation (Haskell)

Design goals: (a) lossless for the union of common features; (b) every node carries an `Attr` so format-specific ids and settings survive round-trips; (c) raw escape hatches at block, question, option, inline and expression level; (d) logic as a real typed AST that each writer "lowers" to its dialect, with warnings when lowering loses information (pandoc's `PandocMonad` + `report` pattern).

```haskell
{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}
module Text.Survey.Definition where

import Data.Text (Text)
import Data.Map.Strict (Map)
import Data.List.NonEmpty (NonEmpty)

------------------------------------------------------------------ top level
data Survey = Survey
  { surveyMeta   :: Meta
  , surveyLangs  :: Languages            -- default + others; drives I18n completeness checks
  , surveyLists  :: Map ListId [Option]  -- shared choice lists (XLSForm lists, FHIR ValueSets, LS label sets, Qualtrics RS)
  , surveyVars   :: [VarDecl]            -- embedded data / hidden fields / URL params / Typeform variables
  , surveyBody   :: [Node]               -- ordered flow; structure and routing interleaved
  } deriving (Eq, Show, Generic, Data)

data Languages = Languages { defaultLang :: Lang, otherLangs :: [Lang] }
newtype Lang = Lang Text                 -- BCP-47 ("en", "es-MX"); readers normalize "English (en)", "EN", "es"

newtype Meta = Meta (Map Text MetaValue) -- title, description, version, form_id, authors, DDI citation, welcome/end text…
data MetaValue = MText I18nText | MRich I18nRich | MBool Bool | MList [MetaValue] | MMap (Map Text MetaValue)
               | MRaw Format Text

------------------------------------------------------------------ attrs & raw
data Attr = Attr { attrId :: Text, attrClasses :: [Text], attrKVs :: [(Text, Text)] }
-- conventions: kv "qualtrics:QID"="QID4", "limesurvey:qid"="17", "xlsform:appearance"="likert",
--              "redcap:annotation"="@HIDDEN", "surveyjs:renderAs"="…"; classes for display hints.

newtype Format = Format Text             -- "qsf", "xlsform", "surveyjs", "html", "xpath", …

------------------------------------------------------------------ text
type I18n a    = Map Lang a              -- total over surveyLangs ideally; missing = fallback to default
type I18nText  = I18n Text
type I18nRich  = I18n [Inline]

data Inline                              -- small subset; bridge to pandoc-types Inline for HTML/MD I/O
  = Str Text | Space | SoftBreak | LineBreak
  | Emph [Inline] | Strong [Inline] | Underline [Inline] | Code Text
  | Link [Inline] Text | Image Text Text
  | Pipe VarRef (Maybe PipeFormat)       -- ${q1}, {q1}, [q1], {{field:q1}}, ${q://QID1/ChoiceGroup/SelectedChoices}
  | RawInline Format Text                -- unparsed HTML, Qualtrics piped-text with no equivalent, etc.

------------------------------------------------------------------ flow / structure
data Node
  = Section   Attr SectionKind (Maybe I18nRich) SectionOpts [Node]
      -- SectionKind = Page | Block | Group | Panel ; Page is a hard page break boundary
  | Question  Question
  | Display   Attr I18nRich (Maybe Expr)                -- note/descriptive text/statement, with relevance
  | Media     Attr MediaKind Text (Maybe I18nRich)      -- image/video/audio embed
  | Loop      Attr LoopSpec [Node]                      -- repeat/roster/loop-and-merge/paneldynamic
  | Randomize Attr RandSpec [Node]                      -- shuffle children / pick n / balanced
  | Branch    Attr Expr [Node] [Node]                   -- flow-level if/else (QSF Branch, DDI IfThenElse, Blaise IF)
  | Jump      Attr (Maybe Expr) JumpTarget              -- skip-to (Typeform/GForms/SkipLogic/SM); Nothing = unconditional
  | Compute   Attr VarName Expr                         -- calculate/embedded-data set/Typeform add/set
  | Terminate Attr (Maybe Expr) EndKind                 -- end survey / screen-out / redirect
  | RawNode   Format Value                              -- whole unknown element (QSF WebService, Tally CAPTCHA…)

data SectionOpts = SectionOpts
  { secRelevant  :: Maybe Expr
  , secRandomize :: Maybe RandSpec                      -- randomize question order within
  , secLayout    :: [Text] }                            -- "field-list", "table-list", "one-per-page"

data JumpTarget = ToNode Text | ToEndOfSection | ToEnd EndKind | ToUrl Text
data EndKind    = Complete | ScreenOut | QuotaFull | Redirect Text | EndScreen Text
data LoopSpec   = LoopCount Expr | LoopOver ListId | LoopOverSelected VarRef | LoopUntil Expr | LoopDynamic (Maybe Int) (Maybe Int)
data RandSpec   = RandSpec { randSubset :: Maybe Int, randEvenly :: Bool, randKeepLast :: Int }

------------------------------------------------------------------ questions
data Question = Question
  { qAttr       :: Attr
  , qName       :: VarName                -- stable export name (DataExportTag, name, title, Variable/Field Name, linkId)
  , qLabel      :: I18nRich               -- question text
  , qHint       :: Maybe I18nRich
  , qKind       :: QKind
  , qRequired   :: Required               -- NotRequired | Required (Maybe I18nText) | RequiredIf Expr | SoftRequired
  , qRelevant   :: Maybe Expr             -- display-if (the canonical form of logic)
  , qReadOnly   :: Maybe Expr
  , qDefault    :: Maybe Expr
  , qConstraints:: [Constraint]           -- Constraint Expr (Maybe I18nText); plus typed ones below
  , qScoring    :: Maybe Scoring          -- quiz / assessment values
  , qExtra      :: [(Format, Value)]      -- per-format raw payloads we could not map (verbatim JSON/XML fragment)
  }

data QKind
  = Choice   ChoiceSpec
  | Matrix   MatrixSpec
  | TextQ    TextSpec                     -- single/multi-line, input type, length, regex, placeholder
  | Number   NumSpec                      -- integer/decimal, min/max/step, unit, prefix/suffix
  | Scale    ScaleSpec                    -- slider, rating stars, NPS, opinion scale, linear scale (low/high/labels/step/icon)
  | Boolean  (Maybe (I18nRich, I18nRich)) -- yes/no, truefalse, acknowledge/legal
  | DateTime DTSpec                       -- date | time | datetime | daterange | duration
  | Ranking  OptionsRef (Maybe Int)       -- rank all / top-n
  | ConstantSum OptionsRef (Maybe Double) -- allocate total
  | MultiField [SubField]                 -- multipletext / TE Form / contact_info / address / fullname
  | Upload   UploadSpec                   -- file/image/audio/video/signature capture
  | Geo      GeoKind                      -- point/trace/shape
  | Barcode
  | Hidden                                -- hidden input carried with the response
  | RawQuestion Format Value              -- entirely unknown kind

data ChoiceSpec = ChoiceSpec
  { chMin, chMax    :: Maybe Int          -- (1,1)=single; (0,Nothing)=multi; supports "at least 2"
  , chOptions       :: OptionsRef
  , chOther         :: Maybe OtherSpec    -- "Other, specify" with its own var name
  , chWidget        :: Widget             -- Radio | Checkbox | Dropdown | Listbox | Buttons | ImagePicker | Autocomplete | Tagbox
  , chOrder         :: OptionOrder }      -- AsIs | Shuffle | ShuffleKeepLast Int | Alphabetical | Flip

data OptionsRef
  = Inline   [Option]
  | ListRef  ListId (Maybe Expr)          -- shared list + choice_filter
  | External Text                         -- select_one_from_file, choicesByUrl
  | FromAnswers VarRef CarryMode          -- carry-forward (Qualtrics, SurveyJS choicesFromQuestion)

data Option = Option
  { optAttr      :: Attr
  , optValue     :: Value                 -- the CODE stored in data (1, "A", "yes"); required
  , optLabel     :: I18nRich
  , optExclusive :: Bool                  -- "None of the above"
  , optRelevant  :: Maybe Expr            -- per-option display logic
  , optImage     :: Maybe Text
  , optScore     :: Maybe Double          -- weight / assessment value / points
  , optJump      :: Maybe JumpTarget }    -- per-option branching (GForms, SM, QSF SkipLogic) — kept for fidelity

data MatrixSpec = MatrixSpec
  { mxRows     :: [Option]                -- statements; optValue = row code, qName<>"_"<>code = var
  , mxCols     :: MatrixCols
  , mxRowOrder :: OptionOrder
  , mxTranspose:: Bool                    -- LS "array by column"
  , mxDynamic  :: Maybe (Int, Maybe Int) }-- matrixdynamic: user-added rows

data MatrixCols
  = SharedScale ChoiceSpec                -- classic Likert grid (single or multi per row)
  | DualScale ChoiceSpec ChoiceSpec       -- LS dual-scale, QSF bipolar
  | PerColumn [SubField]                  -- matrixdropdown, QSF SBS, LS array numbers/texts: each column its own kind

data SubField = SubField { sfName :: VarName, sfLabel :: I18nRich, sfKind :: QKind, sfRequired :: Required }

------------------------------------------------------------------ values & logic
newtype VarName = VarName Text
data VarRef = VarRef { vrName :: VarName, vrPath :: [Text] }   -- q4.r1 ; q2 . "A" ; repeat-relative handled via Current
data Value = VNull | VBool Bool | VInt Integer | VDec Scientific | VText Text | VDate Day | VTime TimeOfDay | VList [Value]

data Expr
  = Lit Value
  | Var VarRef
  | Current                                 -- "." in XPath constraint, {row.x} in SurveyJS
  | Not Expr | And [Expr] | Or [Expr]
  | Cmp CmpOp Expr Expr                     -- Eq | Neq | Lt | Le | Gt | Ge
  | Arith ArithOp Expr Expr | Neg Expr
  | Selected VarRef Value                   -- multi or single contains code
  | AnySelected VarRef [Value] | AllSelected VarRef [Value]
  | CountSelected VarRef
  | Answered VarRef                         -- not empty / exists
  | Displayed VarRef                        -- QSF "Displayed", LS is_empty on irrelevant
  | TextOp TextOp Expr Expr                 -- contains, starts, ends, regex
  | If Expr Expr Expr
  | Call Text [Expr]                        -- named function, with a small standard library (today, age, sum, len, round…)
  | RawExpr Format Text                     -- verbatim when parsing failed or function unknown

data Required  = NotRequired | Required (Maybe I18nText) | RequiredIf Expr | SoftRequired
data Constraint = Constraint Expr (Maybe I18nText) | LengthBetween (Maybe Int) (Maybe Int)
                | Pattern Text | RangeC (Maybe Value) (Maybe Value) | SumEquals Double
data Scoring = Scoring { correct :: [Value], points :: Maybe Double, feedback :: Map Value I18nRich }
```

**Key design decisions and why:**

1. **One ordered tree with flow constructors** (`Branch`, `Jump`, `Randomize`, `Loop`, `Compute`, `Terminate`) inside `surveyBody`, rather than Qualtrics' split into block definitions plus a flow. Readers for QSF inline blocks at their flow positions. If a QSF block appears twice in the flow (rare), the reader keeps the second occurrence as a `RawNode "qsf-blockref"`.
2. **Two logic forms, with passes between them.** Keep display-if (`qRelevant`, `secRelevant`, `optRelevant`) and jump (`Jump`, `optJump`) both in the AST, then provide transforms in a `Text.Survey.Transform` module:
   - `jumpsToRelevance`: for forward-only jumps in a DAG, every node strictly between a jump source and its target gains `relevant &&= not cond`. This is always possible for forward jumps.
   - `relevanceToJumps`: possible when the relevant nodes form contiguous runs whose condition depends only on earlier single-choice answers. Otherwise it emits a warning and drops the logic, which is what a Google Forms writer must do.
   - `branchToRelevance`: pushes a `Branch` condition down to its children.
   - Writers declare a capability record (`supportsRelevance`, `supportsJumps`, `supportsBranchNodes`, `exprDialect`, `supportsI18n`, `supportsMatrixPerColumn`…), and the driver runs the needed lowering passes before calling the writer. This mirrors pandoc's extension flags.
3. **Codes are mandatory on options** (`optValue`). Formats without codes (Google Forms, SurveyMonkey, Jotform, GIFT) get synthesized codes: by position, or the label itself when the writer targets a label-as-value format. The IR records this with `Attr` class `code-synthesized` so a round trip does not invent spurious codes.
4. **Variable naming is explicit**: `qName` plus a deterministic subvariable naming policy (`q2_A`, `q4_r1`). The policy is configurable per writer, because SPSS, REDCap and Stata have length and character limits and a `sanitizeNames` pass is needed.
5. **Matrix is first-class** with three column models (`SharedScale`, `DualScale`, `PerColumn`) because that is exactly where formats differ. The `flattenMatrix` pass turns a matrix into a `Section` of single questions sharing an `Attr` group id. REDCap, Typeform, XLSForm, Triple-S, SPSS and oTree need it. `groupMatrix` is its inverse, keyed on `Matrix Group Name`, a `table-list` group, a `varGrp type=grid`, and so on.
6. **Expression parsing per dialect, with a raw fallback.** Parse XPath/ODK, SurveyJS, REDCap, LimeSurvey ExpressionScript, QSF logic trees, Typeform op trees, FHIR enableWhen, and (later) FHIRPath into `Expr`. Anything unparsed becomes `RawExpr dialect text`. When a writer's dialect equals the raw dialect, it passes the text through verbatim, which gives lossless same-format round trips even for exotic functions.
7. **Rich text is a small `Inline` type with a `Pipe` constructor**, bridged to `pandoc-types` so HTML (QSF, LSS) and Markdown (XLSForm allows markdown) can be parsed and rendered with pandoc itself. Pipes are the one construct pandoc lacks.
8. **i18n is `Map Lang`** at every text leaf, not parallel document copies. The LimeSurvey TSV and XForms itext readers merge their per-language copies by id. Writers for monolingual formats take a `--lang` option.
9. **Escape hatches at every level**: `RawNode`, `RawQuestion`, `RawInline`, `RawExpr`, `qExtra`, `MRaw`, plus `Attr` key-values. A same-format round trip (QSF → IR → QSF) should reproduce unknown settings from `qExtra` and `Attr` by merging them back into the emitted JSON.
10. **Native serialization**: derive Aeson instances and publish the JSON form as the tool's native format (like `pandoc -t json`). Test fixtures, filters and LLM tooling can then target it.

---

## 25. Prioritized implementation plan

**Phase 0: core**
1. The IR plus a native JSON reader/writer (Aeson) and a pretty "survey markdown" writer for debugging, with golden tests.
2. Lowering passes: `flattenMatrix`/`groupMatrix`, `jumpsToRelevance`/`relevanceToJumps`, `branchToRelevance`, `synthesizeCodes`, `sanitizeNames`, `selectLang`. Add a warning/log monad from day one.

**Phase 1: highest value per effort (readers and writers)**
3. **XLSForm** reader+writer (CSV first, then xlsx via the `xlsx` package), plus an ODK XPath expression parser/printer. The open ecosystem is huge (ODK, Kobo, SurveyCTO), and the format stresses i18n, groups, repeats and expressions.
4. **SurveyJS JSON** reader+writer and the SurveyJS expression parser/printer. With it, the **jsPsych** writer comes almost free.
5. **Qualtrics QSF reader** (the biggest demand; parse the logic trees and flow), then the **Qualtrics TXT Advanced writer** (trivial; the no-API route into Qualtrics). Add a QSF writer after the reader has been tested against many real exports.
6. **REDCap data dictionary CSV** reader+writer and the REDCap logic parser/printer.
7. **LimeSurvey TSV** reader+writer and the ExpressionScript parser/printer, then the **LSS reader** (same model).

**Phase 2: API platforms (mostly JSON, logic via lowering)**
8. **Google Forms**: a reader from `forms.get`, and a writer that emits `forms.create` plus `batchUpdate` request JSON.
9. **Typeform** reader+writer (jump logic and variables).
10. **SurveyMonkey** reader, and a writer if needed.
11. **ODK XForms reader** (import already-deployed forms). The XForms writer comes last, or is done by shelling out to pyxform.

**Phase 3: standards and codebook targets (mostly writers)**
12. **FHIR Questionnaire** (R4 + R5) reader+writer, plus the enableWhen mapping. FHIRPath comes later as RawExpr.
13. **DDI Codebook 2.5** writer (and reader), **Triple-S** writer, **SPSS syntax** writer (with MRSETS), CDISC **ODM** writer/reader (which also gives REDCap project XML read access).

**Phase 4: long tail, on demand**
14. Qualtrics TXT Simple reader, GIFT/Aiken/Moodle XML writers, surveydown writer, formr (XLSForm variant), oTree writer, Formbricks, Tally, Jotform, Survey Solutions, Forsta XML, Dimensions mrScript, Blaise, DDI-L, QTI 3, and a `.sav` binary writer.

**Skip:** Microsoft Forms (no definition API or format). For it, emit DOCX or TXT for its Quick Import.

---

## Sources

- Qualtrics import/export (TXT Simple/Advanced, QSF note): https://www.qualtrics.com/support/survey-platform/survey-module/survey-tools/import-and-export-surveys/
- LimeSurvey TSV structure: https://www.limesurvey.org/manual/Tab_Separated_Value_survey_structure ; question types: https://www.limesurvey.org/manual/Question_types ; ExpressionScript: https://manual.limesurvey.org/Expression_Manager/en
- XLSForm: https://xlsform.org/en/ , https://docs.getodk.org/xlsform/ ; ODK XForms spec: https://getodk.github.io/xforms-spec/
- SurveyJS: https://surveyjs.io/form-library/documentation
- REDCap: data dictionary help inside REDCap (Project Setup → Data Dictionary → "Help with the data dictionary"); CDISC ODM: https://www.cdisc.org/standards/data-exchange/odm-xml
- Google Forms API: https://developers.google.com/workspace/forms/api/reference/rest/v1/forms
- Typeform Create API: https://www.typeform.com/developers/create/reference/create-form/
- SurveyMonkey: https://api.surveymonkey.com/v3/docs , https://github.com/SurveyMonkey/public_api_docs/blob/main/includes/_questions.md
- Triple-S: http://www.triple-s.org/wp-content/uploads/Triple-S-XML-3.0.pdf , examples: http://www.triple-s.org/wp-content/uploads/Triple-S-XML-3.0-Examples.pdf
- DDI: https://ddialliance.org/Specification/
- FHIR Questionnaire: https://hl7.org/fhir/questionnaire.html ; SDC: https://hl7.org/fhir/uv/sdc/
- Moodle GIFT: https://docs.moodle.org/en/GIFT_format ; Aiken: https://docs.moodle.org/en/Aiken_format
- QTI: https://www.1edtech.org/standards/qti
- Tally API: https://developers.tally.so/api-reference/endpoint/forms/post
- Jotform API: https://api.jotform.com/docs/
- surveydown: https://pkg.surveydown.org/reference/sd_question , https://surveydown.org/docs/question-types
- SPSS `.sav` layout (PSPP): https://www.gnu.org/software/pspp/pspp-dev/html_node/System-File-Format.html
- oTree: https://otree.readthedocs.io/ ; jsPsych: https://www.jspsych.org/
