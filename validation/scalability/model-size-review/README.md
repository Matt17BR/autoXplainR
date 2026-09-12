# What the report's R object size measures

The final million-row regression report showed an intercept-only baseline much
larger than the fitted boosting and regularized models. A read-only inventory
of that saved result reproduced every displayed size:

| Retained model | R object size |
| --- | ---: |
| Boosting | 55.78 KiB |
| Regularized regression | 62.93 KiB |
| Intercept-only baseline | 234,386.05 KiB |

The baseline retains a full `lm` object. Its QR decomposition, residuals and
fitted values each account for approximately 72 MB under `object.size()`;
effects add 16 MB and the retained outcome adds 8 MB. The coefficient itself
occupies 288 bytes. Each residual or fitted-value vector contains 8 MB of
numeric values but also 64 MB of row names. Shared names can be counted
repeatedly by this measurement.

The boosting wrapper is different. Its native fit contributes 4,424 measured
bytes, including a 64-byte pointer to the C++ model. The native allocation behind
that pointer is excluded. Most of its measured R size is the saved input recipe.
Consequently, using the same R function does not yield a comparable deployment
memory or saved-file footprint across these model representations.

The chart help, measurement disclosure and `model_tradeoffs()` documentation now
explain those limits. The H2O engine-reported size remains explicitly separate
from its native baseline's R measurement. Numeric values, model retention and
computations are unchanged.

[findings.json](findings.json) records all component sizes, the original report
hash and inspection scope. This audit used an existing result and did not
measure native heap usage or prediction-only storage. Full-process peak memory
and complete-result file sizes remain separate measurements in the
[million-row evidence](../million/final-workflows.json).

A separate review HTML was rendered from the same saved result using the final
wording. Its complete bytes differ only in the two intended help passages;
all three JSON payloads are identical. The original HTML and its 81.055-second
one-call measurement remain untouched. [derivative-report.json](derivative-report.json)
records both hashes and the separate review artifact.
