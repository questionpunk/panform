# Microsoft Forms fixtures: provenance

Microsoft does not document a definition format for Forms, and Microsoft Graph has
no Forms endpoints (see https://learn.microsoft.com/en-us/answers/questions/1520732/graph-api-support-for-microsoft-form).
The `msforms` format is the JSON the Forms web editor exchanges with its internal API:

    GET https://forms.office.com/formapi/api/{tenantId}/users/{userId}/forms('{formId}')?$expand=questions($expand=choices)

| File | What it is | Source |
|---|---|---|
| `basic-types.msforms.json` | Synthetic form in the full wire shape of a 2022 formapi GET: an untitled first section, long text, choice, star rating and date questions, and a titled section with a subtitle at the end. Two questions share a title that ends in a space, with the editor's `&nbsp;` rich-text copy. | Hand-written for panform, MIT |
| `sections.msforms.json` | Synthetic form in the full wire shape of a 2026 formapi GET: a section with a title and subtitle, one with an empty subtitle and one with no title, a text question with a subtitle, a choice and a date, plus a stored theme (`otherInfo`, `themeV2`). | Hand-written for panform, MIT |
| `branching.msforms.json` | Synthetic trimmed form (only the fields the reader needs): a `ToTheEnd` jump and a jump to a section. | Hand-written for panform, MIT |
| `all-types.msforms.json` | Hand-built here. Every question type, using the `questionInfo` bodies captured in the sources below. | see below |
| `panform-capture-2026-09.msforms.json` | The owner's own live editor capture. | see "Live editor capture" below |

In the three synthetic files every title, question text, choice and description is
invented. The keys, their order, the encoded `questionInfo`, `settings`, `localeInfo`,
`otherInfo` and `themeV2` strings and the built-in theme constants are the API's. Ids are
placeholders: zero GUIDs for the owner, tenant and principal, `r` plus a zero-padded number
for questions and sections, `example-form-id-…` for the form, and `owner@example.com` for
the creator. No tenant, user or email value is real.

They replace three captured forms from third-party repositories that carried no licence,
which were used to work out the wire shape and are no longer in the tree:

- https://github.com/4orms/renderer (`google_form_example`, a 2022 capture)
- https://github.com/hilmimusyafa/undergraduateinformatics-website
  (`tests/Fixtures/msforms/form-definition.raw.json` and `form-definition-branching.raw.json`)

Sources for the shapes in `all-types` (Rating, NPS, FileUpload, Ranking and Likert
were not in those captures):

- Captured POST bodies for every question type, including the separate `choices`
  entities of Ranking and Likert (`Question.MatrixChoice` rows with `groupId`):
  https://github.com/Dutchy365/BlogBytes/blob/master/content/page/MSFormAPI.md (2023)
- `ChoiceType` 2 = multi-select, `Validation.rule` 11 = email, `Question.MatrixChoiceGroup`
  header with `choices` columns, `Question.FileUpload` info:
  https://github.com/Maxim-Mazurok/msforms-api/blob/main/tests/unit/form-fixture.ts
- Field lists cross-checked against importers: https://github.com/nextcloud/formvox
  (`lib/Service/MicrosoftFormsApiClient.php`), https://github.com/Citolab/qti-convert
  (`microsoft-form/parser.ts`), and a JSON schema from a Power Automate flow:
  https://github.com/wyattdave/Power-Platform (`Exam Marker/ms forms questions schema.json`).
- The formapi endpoint and question POST, captured over CDP:
  https://github.com/dmvevents/clawx-pilot/blob/main/docs/MSFORMS_INTERNAL_API.md

No populated `localeList` (form translations) was found in any sample, so the reader
warns and ignores it.

## Live editor capture

`panform-capture-2026-09.msforms.json` is the formapi GET result
(`forms('{id}')?$expand=questions($expand=choices),descriptiveQuestions`) for a form built by
hand in the Forms web editor (forms.cloud.microsoft) on 2026-09-27, from a personal Microsoft
account. It has an implicit untitled first section, a required choice question with "Other",
a "No" -> end jump, a "Yes" -> section "Drivers" jump, and a text question. The owner,
tenant, creator, OData context, form id and table id are redacted; question and section ids
are the editor's random `r`+hex ids and are kept.

## Live import check (2026-09-27)

`importers/msforms/import.js` imported `all-types` into a real form (personal account),
and the form was read back. Confirmed live: sections, required, "Other", `ToTheEnd` and
jump-to-section branching, Rating, NPS, Ranking options via `POST .../choices`, Likert
as a `Question.MatrixChoiceGroup` header whose `choices` are the columns plus
`Question.MatrixChoice` rows linked by `groupId`, and `Validation.rule` 11 = email.
`Question.FileUpload` is refused on personal accounts (500, AADSTS70011).
