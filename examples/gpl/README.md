# GPL-licensed example files

The files under this directory are **not** part of the MIT-licensed panform
work. They are third-party test data, shipped next to panform as a mere
aggregation, and they keep their own licence. The MIT licence in the
repository root does not apply to them.

Both files are © LimeSurvey GmbH and contributors and are licensed under the
GNU General Public License, version 2 or (at your option) any later version
(GPL-2.0-or-later). The full text of version 2 is in [`COPYING`](COPYING).

| File | Upstream file | Upstream commit | Changes |
|---|---|---|---|
| `limesurvey/ls2-group-relevance.lss` | [LimeSurvey/LimeSurvey](https://github.com/LimeSurvey/LimeSurvey) `docs/demosurveys/ls205_group_relevance.lss` | `0c137af5fb` (2014-02-07) | None. The bytes are unmodified; only the file name differs. |
| `limesurvey/ls4-answer-options-multilingual.lss` | [LimeSurvey/LimeSurvey](https://github.com/LimeSurvey/LimeSurvey) `tests/data/surveys/limesurvey_survey_expressionAnswerOptionsTest.lss` | `750282eae4` | None. The bytes are unmodified; only the file name differs. |

panform's test suite reads these two files as input (`test/Spec.hs`). Nothing
in the panform library or executable is derived from them or includes them.

If you redistribute this directory, keep `COPYING` and this notice with it.
The rest of `examples/` is covered by [`../SOURCES.md`](../SOURCES.md).
