# Microsoft Forms importer

Microsoft Forms cannot import a form definition, so this script builds the form
through the editor's own internal API from inside a logged-in browser tab. It is
plain JavaScript that lives outside the Haskell library; panform itself only reads
and writes the `msforms` JSON.

## Use

1. Convert: `panform -t msforms -o form.msforms.json input.surveyjs.json`
2. In Microsoft Forms, create a new blank form and open it in the editor. The page
   URL must contain `&id=...`.
3. Open DevTools > Console and paste all of `import.js`. From that moment it watches
   for the editor's own formapi requests.
4. Make any small edit in the editor (e.g. retype a letter of the title), so the
   editor sends one request, then run:

   ```js
   await panformImport(/* contents of form.msforms.json, as an object or a string */)
   ```

   If you call it before making the edit, it logs a prompt and waits (with no time
   limit) until you do.
5. Reload the editor to see the result. The call returns
   `{formId, created, patched, errors, warnings, idMap, stopped}`. Every failed call
   is listed in `errors` with its HTTP status, and the import carries on with the
   next step, with these exceptions:
   - If the form's current content cannot be read, the call fails before anything
     is changed.
   - If the form has sections and "Shuffle questions" cannot be turned off, no
     section or question is created; `stopped` says so.
   - A jump is set only when its target was created. A jump to an item that failed
     (or to an id that is not in the import) is left out and listed in `errors`. A
     Likert row whose header failed is not created.

Only one import runs at a time: a second call while one is running is refused.
Pasting the script again does nothing. If the form already has content, the new
items go after it and `warnings` says so.

**An import cannot be resumed or undone.** Running it again, for example after some
calls failed, adds every item again with new ids. After a partial failure, fix the
form by hand in the editor, or start again with a new blank form.

## What it does

1. It finds the API root (`https://<host>/formapi/api/{tenant}/users/{user}`) among
   the requests the page has already made, and the form id in the page URL. Only
   requests to the page's own origin count: a formapi-shaped URL on another host is
   ignored.
2. It gets auth from the headers the editor sets on its own formapi requests
   (`authorization`, `__requestverificationtoken`, `x-ms-form-request-*`), which it
   reads when that request is sent. Session cookies alone are refused (403). The
   headers are taken only from an https request to the page's own origin, and are
   sent back only to the API root that request went to. They stay inside the
   script's closure and are never logged, and the hook is removed once they have
   been captured.
3. It gives every question and section a new id (`r` + 32 random hex) and rewrites
   `TargetQuestionId` and `groupId` to match.
4. On personal accounts (the `x-ms-form-request-ring: msa` header, or Microsoft's
   consumer tenant `9188040d-6c67-4c5b-b112-36a304b66dad`), it skips file upload
   questions and records a warning. Forms refuses them there: the POST returns 500,
   AADSTS70011. A jump to a skipped question goes to the item after it.
5. It sends the calls in the editor's order:
   - `PATCH forms('{id}')` with the title and description.
   - `PATCH forms('{id}')` with `settings` when there are sections. This sets
     `ShuffleQuestionOrder` to false and changes no other setting.
   - `POST forms('{id}')/descriptiveQuestions` for each section. When questions come
     before the first section, it first adds the untitled first section, as the
     editor does.
   - `POST forms('{id}')/questions` for each question, without any jumps. Ranking
     and Likert options follow as `POST .../questions('{qid}')/choices`.
   - `PATCH .../questions('{qid}')` (or `descriptiveQuestions`) with the full
     `questionInfo`, including `BranchInfo`, once every jump target exists.

## How well each step is verified

A live import into the Forms editor (personal account, `forms.cloud.microsoft`,
2026-09-27) created 18 items and patched 2. Reading the form back confirmed each
item marked "live" below.

| Step | Evidence |
|---|---|
| Title and description, settings, sections (including the implicit first one), required, "Other", choice and text questions, email validation (`Validation.rule` 11), jumps to a section and to the end | Verified live |
| Rating and NPS | Verified live |
| Ranking options via `POST .../choices` | Verified live |
| Likert: `MatrixChoiceGroup` header with `choices`, `MatrixChoice` rows with `groupId` | Verified live |
| Date question | Created without error in the live import; the read-back check did not cover it |
| File upload | **Refused on personal accounts** (skipped, with a warning); not verified on work or school accounts |
| Work and school (tenant) accounts, `forms.office.com` | Same API per published sources, **not verified live** |
| Translations (`localeList`) | Unknown; this script does not send them |

Microsoft does not document this API and may change it at any time.

## Tests

`node --test importers/msforms/import.test.mjs` runs the script against a mocked
editor page. It checks the header wait, the single-run guard, the file-upload skip
and jump retargeting, id remapping, that jumps are patched last, that the API root
and the headers come only from the page's own origin, and what happens when the
first read, the settings change or an item's creation fails.
