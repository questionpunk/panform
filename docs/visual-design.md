# Visual design conversion

Panform converts files. It does not authenticate with vendors, publish forms,
upload images, download themes, execute CSS, or preview the respondent UI.
`panform --catalog` exposes all 35 formats with their implemented design support
so an integrating app can explain those limits before export.

## What stays intact

| Format | Same-format visual settings |
|---|---|
| Jotform | `properties.font`, `fontsize`, `fontcolor`, `background`, `styles`, `formWidth`, `labelWidth`, `alignment`, `lineSpacing`, `highlightLine`, `injectCSS` |
| Formbricks | Complete `styling` object, including nested light/dark colors and future vendor properties |
| SurveyJS | Logo URL/size/fit/position, background image/opacity, width/mode, question-per-page mode, progress display/location/type, question numbering, title/description location |
| Microsoft Forms | Stored `otherInfo`, `background`, `header`, `logo` values from editor exports |
| Typeform | Existing `theme` reference and `cui_settings`; the existing converter also preserves unmodeled field properties, attachments and layouts |
| Native JSON / QuestionPunk Markdown | All of the above as extension metadata, without rendering or cross-vendor translation |
| Other formats | No survey-wide visual design mapping is claimed. Existing question structure and appearance mappings remain separate. |

The catalog's `design.fields` names the exact survey-wide fields covered.
`same_format` means a native setting survives writing back to that format;
`metadata` means the values are stored for a later conversion. `not_mapped`
means the converter has no survey-wide visual design mapping, not that the
vendor itself lacks branding tools.

Jotform fields are stored under `jotform:design` in `Meta.metaAttrs`, Formbricks
under `formbricks:design`, SurveyJS under `surveyjs:design`, and Microsoft Forms
under `msforms:design`. Typeform retains its existing `typeform:theme` and
`typeform:cui_settings` keys. The readers keep values exactly, including null,
empty strings, numbers, nested objects and CSS whitespace. These attributes are
additive; the AST shape and QuestionPunk syntax are unchanged.

Only the listed native properties are emitted from design metadata. A property
such as a webhook, access restriction or form ID cannot be injected through a
design attribute. Unknown nested Formbricks styling properties are retained as
opaque data; Panform does not claim to validate a particular server's schema.

## Moving between platforms

A font, layout, hosted theme or vendor CSS cannot safely be guessed into another
vendor's design system. Registry writers report each unmapped design property
at `design`, even when other conversion warnings also exist. `--strict` refuses
the conversion before writing or overwriting the output file.

Native JSON and QuestionPunk Markdown are lossless intermediate containers for
these settings. An app reading them must not assume that stored settings are
rendered. It must decide whether to offer a destination-native design editor or
ask the author to finish branding at the destination.

Image URLs and Typeform theme references are retained, not the remote files.
Moving a form between accounts may require access to those resources. SurveyJS
Theme Editor themes are separate from its survey definition; this change covers
survey JSON properties, not `applyTheme()` or an invented embedded theme field.
Microsoft Forms remains an undocumented editor format, not a public API.

## Verification

The spec suite checks five distinct Jotform designs and five Formbricks designs,
additional SurveyJS/Microsoft Forms designs, every registered visual field with
null/empty/false/zero/object values, lossless intermediate formats and explicit
loss warnings for all other destinations. Each scenario reads native JSON,
writes it, reads it again and checks byte stability on the second write.

The audit includes five visual-design rows and compares design metadata instead
of treating it as incidental metadata. The private corpus remains separate;
no corpus files or remote account data are included in these tests. These are
file-conversion checks, not authenticated vendor publishing tests.

## Sources

Checked 2026-09-28:

- [Jotform form properties](https://api.jotform.com/docs/properties/?field=form)
- [Formbricks survey resource](https://formbricks.com/docs/api-reference/management-api-%3E-survey/get-all-surveys)
- [SurveyJS survey JSON properties](https://surveyjs.io/form-library/documentation/api-reference/survey-data-model)
- [SurveyJS separate theme interface](https://surveyjs.io/form-library/documentation/api-reference/itheme)
- [Typeform form resource](https://www.typeform.com/developers/create/reference/retrieve-form/)
- Microsoft Forms: existing repository capture provenance in `examples/msforms/SOURCE.md`.
