# Known limitations

panform's rule is that nothing changes without a warning. The cases below are the ones where a conversion cannot be exact. Each is reported by a warning when it happens, and `--strict` refuses to write the output.

## Every format

- **Large jump logic.** Jump-style logic (Formbricks, Tally, Typeform, Google Forms, Microsoft Forms) becomes show-if conditions on the questions it skips. When the exact condition for a question would be enormous (every screen branching on its own answer), that question gets no condition and is shown whenever it is reached. Formbricks keeps the original jumps, so Formbricks → Formbricks stays exact.
- **Text in logic.** Some logic syntaxes cannot write every character in a quoted value:
  - XPath and XLSForm cannot write a value that contains both `'` and `"`.
  - SurveyJS, REDCap, LimeSurvey, mrScript and Blaise cannot write a value that ends in a backslash.
  - Where a choice code is written as part of a name (REDCap, LimeSurvey, Decipher, mrScript, Blaise), a code that is not a plain word is refused. That covers a LimeSurvey code with `_`, and a Decipher choice named `any`, `val`, `ival` or `unsafe_val`.

  In each case the condition is dropped.
- **Input limits.** A spreadsheet is read up to column XFD and 5 million cells. Each zipped part may inflate to at most 32 MiB, and 96 MiB in all. Repeated XML groups expand to at most 200,000 items. A scale lists at most 10,000 points. Beyond these, panform stops with an error rather than running out of memory.

## By format

**Qualtrics (QSF)**
- Question ids are renumbered on writing. Piped text such as `${q://QID7/…}`, conditions kept in Qualtrics form, and questions kept whole may then point at a different question.
- Dynamic (carried-forward) choices become a question with only its written choices.
- Choice-count and custom validation on non-text questions, and default answers on ordinary questions, are not carried.
- A survey password is not copied into the output.

**Qualtrics TXT**
- Trailing spaces, blank lines around text and carriage returns are not kept.
- A line of text that looks like a tag is written as `[ [tag]]`.

**XForm**
- If two questions in different groups share a name, logic that refers to that name points at the first one.
- A default value is trimmed, and a default that is only spaces is dropped.

**XForm → XLSForm:** a required condition written in XPath is dropped. XLSForm → XForm keeps it.

**REDCap**
- A length limit on a text answer is dropped.
- A repeating group is written as an ordinary section, because repeating instruments are set in the REDCap project, not in the data dictionary.
- A section title over a field that has its own header is merged into one header.

**FHIR Questionnaire**
- Several `initial` answers, and answer options on a number or text item, are kept only when writing FHIR. Other formats get the first answer, or no options.
- Options listed from a value set with exclusions, filters or whole code systems may be incomplete.

**ODM:** a GT or LT range check on a decimal answer is read as inclusive (at least, at most). Whole numbers convert exactly.

**Typeform**
- A statement's description is joined to its text.
- Jumps after one panform cannot convert (backward, unknown target, unparsed condition) are kept only for Typeform.

**Formbricks:** validation rules joined by "or", or that repeat a rule type, are kept only for Formbricks → Formbricks. Other formats get no validation for that question.

**Tally:** when large jump logic is approximated (see above), the original jumps are not kept.

**Google Forms:** a text item with a description but no title, or with a blank line in its title, does not keep its title and description apart.

**Decipher:** question elements other than title, comment, row, col and choice (for example `<validate>`, or rows on a text or number question) are dropped.

**mrScript and Blaise**
- A question asked in several places becomes one question, shown when any of the asks applies.
- A field assigned several times keeps only the last assignment.

**Aiken:** text with line breaks or extra spaces is written on one line. A question that starts like an option (`A. `) or like `ANSWER:` is written as it is. Aiken has no way to escape either.

**surveydown:** hints and page titles are written on one line.

**SurveyJS:** a survey-level property called `design` is dropped, because that name is where panform keeps the visual design.

**QuestionPunk Markdown and app JSON**
- Sections nested deeper than six levels are written at level six, so they and the items after them read back under another section.
- A ranking or matrix with nothing listed has no spelling that reads back.
- In the app JSON, a branch back to an earlier question has no display-condition form and is not carried.

## Microsoft Forms importer

`importers/msforms/import.js` cannot resume an import: running it again adds every item again.
