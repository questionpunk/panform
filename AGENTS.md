# Working on panform

panform is a Haskell "pandoc for forms": 35 survey and form formats are read into one AST (`src/Panform/AST.hs`, type `Form`) and written back out. Read `README.md` first.

## Rules

- **Every reader and writer is pure.** They run in `Conv` (`src/Panform/Conv.hs`) and never do I/O. The same input always gives byte-identical output.
- **Never lose anything silently.** Keep the data, or `warn` saying what changed and where. A reader that finds nothing already warns (via the registry in `src/Panform/Formats.hs`).
- **Round trips are exact.** A writer's output must be readable by its own reader. Writing it again must give identical bytes, and reading it back in the same format must give the same `Form` unless something warned.
  - Keep respondent-facing text exactly: never trim on read without trimming the same way on write.
  - Make id and name sanitising idempotent.
- **Logic** is the `Expr` type. Each text syntax is a `Dialect` in `src/Panform/Expr.hs`; QuestionPunk's is in `src/Panform/Expr/QP.hs`. Jump-style logic becomes show-if (`src/Panform/Jump.hs`, `src/Panform/Bdd.hs`).
- **New format:** add a module under `src/Panform/Format/`, register it in `Formats.hs`, add it to `panform.cabal`, add an example under `examples/` with provenance in `examples/SOURCES.md`, and add a test in `test/Spec.hs`.

## Checks before any PR

```sh
cabal build all --ghc-options=-Werror   # zero warnings
cabal test spec                          # ~560 tests: audit, fixed points, own-format losslessness
cabal run -v0 panform -- --audit > docs/audit.md   # CI fails if this file is stale
PANFORM_CORPUS=../panform-corpus PANFORM_REPORT=/tmp/report.md cabal test corpus   # maintainers only
```

- The corpus is 274 real survey files with mixed licences, kept outside this repository; the suite skips when it is absent. Every row of its report should read `274 | 0 | 0 | 0 | 0 | 0 | 274 | 0`. Never copy corpus files into this repository.
- A conversion that cannot be exact must warn, and belongs in `docs/known-limitations.md`.

## Keeping up with the QuestionPunk app

`src/Panform/Format/QuestionPunkJson.hs` restates three lists from the app, and the app adds to them often:

| List here | The app's source |
|---|---|
| `answerTypes` | the Prisma `AnswerType` enum |
| `authorable` | `chatEditAnswerTypes` (`app/prompts/surveyQuestion/surveyItem/common.ts`) |
| `detailKeys` | every key in `ITEM_DETAIL_KEYS_BY_ANSWER_TYPE`, `TEXT_FIELD_ITEM_DETAIL_KEYS_BY_ANSWER_TYPE` and `CHAT_EDITABLE_ITEM_DETAIL_KEYS` |

A type or key missing here is never lost: it is kept by name with a warning, or written with the `questionpunk:` prefix. It is still drift, and these lists decide the bytes both writers produce.

- The app's `detailKeysMatchApp.test.ts` fails when the app gains a type or key its port lacks. The developer records it in `PENDING_IN_PANFORM` (`qpMarkdown/detailKeys.ts`). That list is this repo's to-do.
- **To sync:** add the pending entries to the lists here (`detailKeys` stays in ASCII order), run the checks below, and merge. Then, in the app, regenerate the golden files with the new commit, update `PANFORM_COMMIT`, move the pending entries into the port's lists and empty `PENDING_IN_PANFORM`. The app's `docs/agents/modules/17-survey-file-formats.md` has the commands.
- Sync before every release of panform, and whenever the pending list is not empty.

## Conventions

- **Coding agents commit as the configured git user.** No `Co-Authored-By` or `Claude-Session` trailers, and no "Generated with" lines in PR bodies.
- **Branch and open a PR to `main`.** Never push to `main` directly.
- **Releases are the maintainer's call.** Do not publish to Hackage or tag a release without the maintainer's approval.
- **Every example needs a row in `examples/SOURCES.md`** with its source and licence. Write new fixtures yourself (MIT); a third-party file goes in only with a licence that allows it, and GPL files only under `examples/gpl/`.
- **QuestionPunk Markdown (`.qp.md`) and `.qp.json` are also implemented in TypeScript** in the QuestionPunk app (`questionpunk/app/utilities/survey/formats/qpMarkdown/`), which pins a panform commit and checks it byte for byte. Change their syntax only with a matching app change and regenerated golden files.
- `importers/msforms/import.js` is a browser script replaying Microsoft Forms' internal API. It runs only in a logged-in Forms editor tab and has a Node test (`node --test importers/msforms/import.test.mjs`).
