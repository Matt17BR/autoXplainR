# Audit reproductions

These scripts record counterexamples against source commit
`2e6920354fe7ec7550f9c2688eff5f54034c28bd` (AutoXplainR 0.3.0).
Read [the audit](../audit-0.3.0.md) for expected behavior and interpretation.
They deliberately expose incorrect behavior and are not passing regression
tests. Outputs were obtained on R 4.5.2, Ubuntu 26.04.1.

Run from the repository root with the package's development dependencies:

```sh
Rscript validation/audit-evidence/statistical-reproductions.R
Rscript validation/audit-evidence/adapter-reproductions.R
Rscript validation/audit-evidence/audit-reuse.R
Rscript validation/audit-evidence/ordered-neural.R
Rscript validation/audit-evidence/formula-capture.R
Rscript validation/audit-evidence/additional-reproductions.R
```

The `.txt` files retain observed results. The first adapter example uses a
separated binomial fit to make the complement error obvious; the corrective test
should also cover ordinary nondegenerate data. Timing is not used as a product
benchmark in the audit. Formula serialization sizes are environment-specific;
the retained bindings and unchanged predictions establish the underlying issue.

For browser measurements, install Python Playwright 1.58.0 in a disposable
environment, use a Chrome executable, and install axe-core 4.13.0 separately.
Set `CHROME_PATH` to the browser and `AXE_PATH` to `axe.min.js`, then run:

```sh
python validation/audit-evidence/browser-audit.py
```

The browser script reads the committed public synthetic report, measures desktop
and mobile layouts, and writes `browser-audit.json`. A detected issue is a
starting point for review: Chrome's manual keyboard test passed for the five
table scroll regions flagged by axe. Other-browser behavior and incomplete
contrast checks remain unverified. This is not a full accessibility audit.

All examples use built-in or synthetic data. No script starts H2O or calls a
hosted narrative provider. H2O preprocessing was inspected separately; no live
ranking-effect claim is made. The eight existing targeted test files passed
201 assertions with zero failures, warnings or skips; see
`existing-targeted-tests.txt`. The entire suite was not rerun during this review.
