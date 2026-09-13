# Explanation row-cap diagnostic

Declared on 12 September 2026 before computing this comparison. This probe does
not change package defaults, repeat counts or the acceptance protocol.

Use the retained Year development forest fitted on 40,000 rows with 500 trees,
`mtry=30`, split-node size 20 and seed 80711. Evaluate importance on the separate
20,000-row development assessment set. These are development targets, not
locked acceptance targets. Freeze the actual package source before loading it.

Compare 1,000 and 5,000 uniformly sampled assessment rows using the same sample
seed 80831 and permutation seed 80841. Verify that the smaller sample is nested
inside the larger. Both calls screen all 90 inputs with five repeats, RMSE and
the existing 95% shuffle intervals. Those intervals describe repeated shuffling;
they do not include uncertainty from selecting reference rows.

Retain complete repeat losses, row identities, timing and source hashes. Compare
the overlap of the leading 8 and 16 inputs, signs, effect magnitudes, repeat
standard errors and whether inputs with clearly positive 5,000-row importance
remain detectable. Ranking among near-zero inputs is not an accuracy standard.
Record the complete top-input comparison rather than using a single rank
correlation to declare success.

This is one development model and one nested sample pair. It can identify
failure or support further testing, but cannot alone establish a universal
1,000-row default. No automated row-budget change is authorized by these
results. A complete one-call timing comparison and classification checks remain
necessary before adopting any broader policy.
