# panform

Pandoc for forms. `panform` reads a survey or form in one format and writes it in another: Qualtrics to XLSForm, REDCap to FHIR, Google Forms to SurveyJS, and so on, across 35 formats. Everything goes through one shared definition of a survey, and every reader and writer is a pure function.

panform is an independent project. It is not affiliated with, endorsed by or maintained by pandoc or its authors. Its design is inspired by their work: one shared document model, with a pure reader and writer per format.

```sh
panform survey.qsf -o survey.xlsx              # formats guessed from the file names
panform -f redcap -t fhir dictionary.csv       # or named explicitly
panform form.typeform.json -t questionpunk     # to a human-readable questionnaire
panform --strict in.lss -o out.gform.json      # refuse if anything would be lost
```

Every conversion reports what it could not carry across, so nothing disappears silently:

The cases panform cannot convert exactly are listed in [docs/known-limitations.md](docs/known-limitations.md); each is reported when it happens, and `--strict` refuses to write the output.

```
3 thing(s) did not convert exactly:
  [features] exclusive choice written as a plain choice
  [sat] matrix expanded into a field-list group of one select per row
  [nps] scale end labels dropped
```

## Formats

| Family | Formats |
|---|---|
| Survey platforms | Qualtrics QSF and TXT, LimeSurvey `.lss` and tab-separated, SurveyMonkey, Microsoft Forms, Google Forms (`forms.get` and `batchUpdate`), Typeform, Jotform, Tally, Formbricks, SurveyJS |
| Field data collection | XLSForm (`.xlsx` and CSV), ODK XForms |
| Research and clinical | REDCap data dictionary, CDISC ODM (including REDCap project XML), HL7 FHIR Questionnaire, DDI Codebook, Triple-S, SPSS syntax |
| Market research | Forsta (Decipher) XML, IBM Dimensions mrScript, Blaise |
| Education and experiments | IMS QTI, Moodle GIFT and Aiken, jsPsych, surveydown |
| Text | QuestionPunk Markdown (round-trips everything panform reads), printable Markdown and HTML (write only), native JSON |

All of them read and write except printable Markdown and HTML. `panform --list-formats` prints the full list with file extensions. Microsoft Forms has no public definition format; `msforms` is the JSON its editor exchanges with its internal API, reconstructed from real captures (sources in `examples/msforms/SOURCE.md`).

## What survives a conversion

`panform --audit` round-trips about forty one-feature surveys through every format and prints a feature × format table: ✓ kept exactly, ~ changed with a warning, ✗ changed silently. The test suite fails on any ✗, so the table in [docs/audit.md](docs/audit.md) is also a guarantee: every loss shown there is reported when it happens.

## Visual designs and integration catalog

`panform --catalog` emits the versioned `panform.catalog/1` JSON catalog for all
35 formats: file extensions, read/write support, and the native visual properties
this converter preserves. Its mode is `offline_conversion`: this is not a list
of live publishing integrations or a promise of all features in each vendor UI.

Jotform colors, fonts, spacing and custom CSS; Formbricks styling; SurveyJS
logos, backgrounds and layout; and Microsoft Forms' stored theme data now
survive a same-format conversion. Typeform keeps its theme reference and
conversational UI settings. Native JSON and QuestionPunk Markdown can carry
these settings as metadata between conversions.

Moving designs to another vendor reports each unmapped visual property.
`--strict` refuses to write the result. The [design guide](docs/visual-design.md)
explains what is preserved, including image and hosted-theme limitations.
The [audit](docs/audit.md) includes visual design rows; design metadata is no
longer discarded when comparing those results.

## QuestionPunk Markdown

A questionnaire written the way it looks on paper. It is valid Markdown, it round-trips exactly (see the known limitations for the two exceptions), and people and language models can write it by hand.

```markdown
---
title: Customer feedback
languages: en, fr
---

# About you {#p1}

1. Have you used our product? {#used *}
   @fr Avez-vous utilisé notre produit ?
   ( ) Yes
   ( ) No

2. Which features do you use? {#features}
   if used = Yes
   [ ] Reports
   [ ] Other {#other other}
   [ ] None of these {#none only}

3. How likely are you to recommend us? {#nps}
   [0] Not likely .. [10] Very likely
```

`( )` is pick one, `[ ]` pick many, `-` lines are matrix rows (or a ranking on their own), `@fr` translates the line above, and conditions are plain words: `used = Yes and features has api`, `nps <= 6`, `why is answered`. A `#` heading opens a section, and a `#` alone on a line ends it. The full grammar is in the header of `src/Panform/Format/QuestionPunk.hs`.

## How it works

- **One AST** (`src/Panform/AST.hs`): questions with coded choices, matrices, scales, rankings, validation, translations on every text, pages, sections and repeats, plus extension attributes and raw pass-through, in the spirit of pandoc's `RawBlock`.
- **A pure conversion monad** (`src/Panform/Conv.hs`): a reader or writer can fail or warn, and cannot do I/O. The same input always gives byte-identical output.
- **Logic dialects** (`src/Panform/Expr.hs`): show-if conditions parse into one expression type and print in each format's own syntax: XLSForm `${q}`, REDCap `[q(code)]`, SurveyJS `{q} contains`, LimeSurvey ExpressionScript, XPath, Decipher, mrScript, Blaise and R. Jump-to logic (Google Forms, Typeform, Tally, Formbricks, Microsoft Forms) is converted to show-if and back where that is exact, and conditions derived that way are simplified.

- **Bounded input:** a small hostile file ends in an error or a warning, never a crash or runaway memory. An `.xlsx` is refused past column XFD (16,384), past 5,000,000 cells, or when its XML parts inflate past 32 MiB each or 96 MiB together. ODM 2.0 group references may expand to 200,000 items and groups, and QTI branch rules to 1,000,000 condition terms. A scale written out point by point lists at most 10,000 points, with a warning. A REDCap dictionary that is not UTF-8, names a column twice or has a value past its last named column is refused.

## Tests

`cabal test` runs about 560 tests:

- **Property tests:** logic parses back to itself in every dialect.
- **No silent loss:** the audit above.
- **Fixed points:** every example file in `examples/`, including real exports from Qualtrics, LimeSurvey, pyxform, HL7, Google Forms, Typeform and Microsoft Forms, is written in every format and read back, and the second write must be byte-identical to the first.
- **Corpus:** `cabal test corpus` runs 274 real public survey files through every format and writes a report. The files carry mixed licences, so the corpus is kept in a separate repository that is not published; the suite reads `$PANFORM_CORPUS`, defaults to `../panform-corpus`, and skips if neither exists. Every other test runs from this repository alone.

## Building

GHC 9.10 and cabal (`ghcup install ghc recommended && ghcup install cabal recommended`), then:

```sh
cabal build
cabal run panform -- --list-formats
cabal test
```

The code and the hand-written examples are MIT. Examples copied from other projects keep their own licences, recorded per file in `examples/SOURCES.md` with the notices beside them (`*.LICENSE`). The files in `examples/gpl/` are GPL-2.0-or-later; they are test data shipped alongside panform, not part of the MIT-licensed work (see `examples/gpl/README.md`).
