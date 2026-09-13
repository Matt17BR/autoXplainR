# Classification screening row diagnostic

Declared on 2026-09-12 before running this diagnostic. This extends the Year
Prediction probe to the saved Bank Marketing and Covertype native forests. All
assessment targets have already been used for development. Acceptance targets
remain unopened.

The question is whether a 1,000-row feature screening pass merits further study
for expensive forests. Detailed importance and effect calculations would still
use the existing 5,000-row limit. No default changes are authorized by this probe.

For each existing 500-tree forest, use its saved preprocessing blueprint and
compare nested uniform samples of 1,000 and 5,000 assessment rows. Use sampling
seed 80831 and permutation seed 80841, five repeats, every input, and log loss.
Keep the complete repeat matrices, both ordered row samples, all feature means,
shuffle standard errors and intervals, signs, ranks, and sampled class counts.
Use two native prediction threads and load only one forest at a time. Do not
refit or choose model parameters. Record source, model, data and script hashes.

The following checks screen out a smaller pilot as a candidate policy:

- At least seven of the eight leading inputs must agree between row limits.
- At least twelve of the sixteen leading inputs must agree.
- Every input in the 5,000-row top eight with a positive lower shuffle interval
  must appear in the 1,000-row top sixteen with positive mean importance.
- A class represented by at least 20 rows in the larger sample must also occur
  in the smaller sample. Record counts for every class, including zeros.

These checks are necessary diagnostics, not an approval threshold. One paired
sample per dataset cannot establish stable feature selection. In particular,
the union of leading inputs from several models may change with the screening
sample. Report any failed check and any large magnitude changes. Shuffle
intervals do not measure uncertainty from sampling rows.

Measure elapsed importance time and prediction calls on each row limit. Runtime
estimates for a complete report must retain its detailed 20-repeat pass at
5,000 rows and the actual union of selected inputs. Do not present a single
forest's top eight as the union across all reported models.
