# Independent audit of grouped classification folds

Published 0.6.1 balanced only group row counts. With 32 groups of 20 rows and a
binary outcome concentrated in 10 groups, 14 of 30 fixed seeds produced an inner
fold without the positive class. Every seed left at least five training groups
of each class, so a valid five-fold allocation existed. At seed 1 the public
`autoxplain()` call stopped before fitting. The repaired allocator covers both
classes for all 30 seeds and the public call completes with finite probabilities.
The two coverage CSV files preserve every seed.

Run `Rscript validation/stress-grouped/run.R installed-library output-directory`
against each installation to repeat that published-package comparison.

The proposed class-balancing allocator was tested against partitions known to be
feasible, rather than against its own chosen assignments. This review found and
repaired a false failure before accepting the new allocator.

## What failed

The first implementation retried 16 times, but always sorted groups by the same
rarity and priority scores. Randomness affected only ties. When scores differed,
retries could repeat the same unsuccessful placement.

The smallest counterexample found by reducing a generated case has six groups and
three outcome classes:

| Group | A | B | C | One valid fold |
| --- | ---: | ---: | ---: | ---: |
| 1 | 1 | 1 | 0 | 1 |
| 2 | 0 | 0 | 1 | 1 |
| 3 | 1 | 1 | 0 | 2 |
| 4 | 1 | 0 | 2 | 3 |
| 5 | 0 | 1 | 2 | 2 |
| 6 | 0 | 1 | 2 | 3 |

Every fold in that literal witness contains A, B and C. The initial allocator
nevertheless returned an error at seed 7. This is a counterexample to that search,
not an infeasible study design. The regression test checks the witness separately
from the allocator's result.

## Retry comparison

The comparison generated 2,500 matrices with a valid partition constructed first.
Cases vary from 4 to 12 groups, 3 to 5 classes and 2 to 4 folds. The first 1,500
cases have denser class coverage; the remaining 1,000 have sparser coverage.
Class counts vary within groups. All policies use the same seeds and 16 attempts.

| Retry order after the first greedy attempt | False failures | Invalid successful allocations |
| --- | ---: | ---: |
| Original rarity and priority order, random ties only | 30 / 2,500 | 0 |
| Uniform random group permutations | 0 / 2,500 | 0 |
| Random permutations weighted by group rarity | 0 / 2,500 | 0 |
| Rarity multiplied by a random factor between 0.5 and 1.5 | 1 / 2,500 | 0 |

The accepted change keeps the first greedy attempt and uses
`sample.int(length(labels), prob = rarity)` on subsequent attempts. Rarity is the
sum, over classes represented in a group, of the reciprocal number of groups
containing that class. This changes the processing order even when the original
scores have no ties, while continuing to favor groups containing scarce classes.
The class-coverage and imbalance criteria for choosing a fold remain the same.

`retry-comparison.json` records the measured results and failed case indices.
`minimal-false-failure.json` records the reduced counterexample and witness.

## Independent acceptance checks

The accepted allocator passed both parts of `verify-grouped.R`:

- All 2,500 generated, known-feasible cases returned valid allocations.
- All 2,940 tested support multisets agreed with an exhaustive set-partition
  oracle: 1,364 feasible and 1,576 infeasible. These enumerate three outcome
  classes, three through six groups, and two or three folds. Equivalent fold
  label permutations are identified by the oracle; it does not use the greedy
  score or processing order.
- Every returned allocation kept groups intact and put every observed class in
  every fold. Every call preserved the caller's random-number state. Another 25
  checks renamed outcome classes and obtained identical fold assignments.

The exhaustive check covers every multiset of nonempty group class supports in
that range, ordered by support pattern. It does not cover every ordering of those
groups, every row multiplicity, or all larger designs. The random cases add
unequal class counts but remain a finite sample. This is still a bounded search;
failure does not prove that a valid partition is impossible. The error text says
so, and the documentation does not promise optimal class balance.

Separate public workflow tests change only outer-test labels and require identical
training folds, selected configurations, fitted predictions and preprocessing.
The class-balancing inputs are restricted to outer-training outcomes.

## Reproduce

From the package root:

```sh
AXR_GROUPED_OUTPUT=/path/to/grouped-oracle.json \
  Rscript validation/stress-grouped/verify-grouped.R
```

Set `AXR_GROUPED_LIBRARY` to an installed package library to test an artifact;
otherwise the script loads the working source. The generator seed is 51312 and
the allocator seed is 7. `final-oracle.json` is the accepted local run. Full
generated matrices and exploratory comparison scripts are retained locally under
`~/.cache/autoxplain-stress-0.6.2/adapters/grouped-oracle/` and its parent directory.
