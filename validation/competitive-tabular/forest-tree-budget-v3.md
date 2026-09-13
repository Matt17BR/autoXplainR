# Automatic forest tree budget, version 3

Declared on 13 September 2026 before the version 3 tree-budget candidate was
frozen or fitted and before any locked acceptance outcomes were opened. This
amendment supersedes the automatic tree counts in
[forest-validation-budget-v2.md](forest-validation-budget-v2.md). Existing
frozen candidates and their recovery runs retain their original settings and
records. Earlier 500-tree-final requirements describe those earlier candidates;
the proposed automatic final model below has 256 trees in the heavy tier.

## Decision and scope

The input work proxy remains the number of outer-training rows multiplied by
the number of raw predictors. Only package-generated adaptive forest settings
use the following policy, identified as `forest-tree-budget-v3`:

| Input work | Screening trees | Complete CV trees | Final all-training trees |
|---|---:|---:|---:|
| Below 1,000,000 | 128 | 500 | 500 |
| 1,000,000 to below 4,000,000 | 128 | 256 | 500 |
| At least 4,000,000 | 128 | 128 | 256 |

The automatic final count is part of each planned configuration before its
parameter key, description, complexity proxy and deterministic fitting seed are
created. A heavy-tier final fit requests 256 trees and fits 256 trees. Its
screening and CV records disclose their actual counts separately. This is a
computation default, not a convergence test or a claim of optimal tree count.

Grid search, explicit grids and exact family-budget searches retain their
requested tree counts. The existing input-subset and split-node rules are
unchanged. Every requested CV fold and every row in each fold's training
partition remain included. Every final fit receives the complete declared
training pool and retains native OOB diagnostics. Neither evaluation features
nor evaluation outcomes determine the tree budget. Explanation samples remain
at the existing 5,000-row limit, with five screening permutations and twenty
detailed permutations; this amendment reduces neither rows nor repeats.

## Evidence and expected work

The prior CV-only reduction left final fitting, stored forest size and report
prediction costs unchanged. The saved 500-tree Covertype development forest's
54-input screening pass required 270 predictions on 5,000 rows and took
493.131 seconds at two native threads. Subsequent individual predictions were
consistent with repeated prediction dominating that cost. Those measurements
describe a 50,000-training-row forest, not the full Covertype forest; see
[report-cost-assessment.md](explanation-work/report-cost-assessment.md).

For one forest finalist and five equal folds, normalized CV-plus-final
row-tree work changes from `5 * .8n * 128 + n * 500 = 1012n` to `768n`, a
24.11% reduction. Screening is unchanged. For the default eighteen-setting
tabular portfolio, six forest screens of approximately 16,000 rows and 128
trees add 12.288 million row-trees. Under those assumptions the full Year
forest work changes from 481.57 million to 368.42 million row-trees, and full
Covertype from 482.68 million to 369.26 million, approximately 23.5% reductions.
These proxies omit split costs, node counts, sampling fractions, OOB work and
other model families. They are not elapsed-time estimates.

Each reported model still requires `5p + 20U` permutation predictions, where
`p` is the input count and `U` is the actual union across reported models. A
256-tree final forest has 48.8% fewer trees to traverse than a 500-tree forest.
The actual smaller forest must be measured; a prediction prefix of a stored
500-tree model does not establish the same model-loading or prediction cost.
Neither proxy establishes completion within a resource gate.

Existing same-seed development prefixes give these loss differences from 500:

| Development forest | 256-tree difference | 128-tree difference |
|---|---:|---:|
| Year, square-root mtry, node size 5, RMSE | +0.015162 | +0.043579 |
| Year, mtry 30, node size 20, RMSE | -0.000718 | +0.008514 |
| Year, Extra Trees, RMSE | +0.012205 | +0.031722 |
| Bank, log loss | +0.000199 | +0.001682 |
| Covertype, log loss | +0.000095 | +0.001944 |

Sources are [regression prefixes](forest-policy/prefix-results.json) and
[classification prefixes](forest-policy/classification-prefix-results.json).
The small aggregate differences support testing 256 final trees, but they do
not establish final-model equivalence. Covertype cottonwood/willow correct
classifications fell from 74/94 at 500 to 71/94 at 256. Covertype has one zero
observed-class probability at every tested count; finite log loss uses the
unchanged `1e-15` floor. Prefixes overlap and their row-bootstrap intervals do
not measure fitting-seed or training-sample uncertainty. Changing the planned
tree count changes the existing parameter-derived fitting seed. The seed rule
is preserved; cached prefixes do not replace the actual proposed seeded fit.

This evidence does not justify 128 final trees. Keeping 5,000 explanation rows
also avoids a separate unsupported approximation: the earlier 1,000-row Bank
probe retained only five of the eight leading inputs and reversed the sign of
`poutcome` importance. Fewer model trees can still change importance and the
cross-model feature union even when all reference rows are retained.

## Checks and remaining release gates

Focused tests must verify both work boundaries, exact-path preservation,
planned keys and seeds, native regression/binary/multiclass final structures
and predictions, retained final OOB calculations, and actual attempted settings
when fitting fails. Backend-stub orchestration must retain every requested
fold and final training row. Such mocks are not scale or quality measurements.

Before acceptance freeze, record bounded development comparisons of fixed Year
and Covertype configurations fitted with the actual proposed parameter-derived
seeds. Retain complete predictions, primary and secondary losses, class counts
and recalls, probability zeros, fit/prediction times and saved model sizes.
Compare influential and borderline explanation effects on the same 5,000
reference rows with unchanged repeat counts. Covertype's 50,000-row development
pool has only 2.7 million input work, so an explicit 256-tree diagnostic there
must not be described as the ordinary heavy-tier default or a full-data fit.

The original 1,200-second development and 7,200-second full-workflow limits,
24-GiB address-space ceiling, held-out partitions, selected-model thresholds,
[forest-family quality gates](forest-family-acceptance.md), independent metrics,
complete saved prediction replay and offline report tasks remain unchanged.
The full public one-call run includes search, final fits, default explanations,
HTML and saved results in its existing total limit. A strong boosting result
cannot waive forest quality or full-row forest completion. A failed native
reference still supplies no automatic pass.

This amendment does not change the native 500-tree reference workflow. Any
separately declared reference using development-selected settings must retain
the original interruption record, its own provenance and its own unchanged
resource limits. Include this amendment and all other protocol amendments in
the final acceptance freeze manifest. No results produced by this amendment
are locked acceptance evidence until that freeze and the required scoring.
