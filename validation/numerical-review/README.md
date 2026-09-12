# Independent numerical review

This check supplements the numerical regressions in
`tests/testthat/test-adapter-robustness.R`. It uses the source before the portable
mean and standard-deviation repair, commit
`813d9b0a18397028ff1c6caaa951b8e46ffa03bb`, as its ordinary-input reference.

The 48 ordinary cases compare complete matrix blueprints and baked input
matrices, including reordered rows. They cover four row counts, three input
scales, all centering/scaling combinations, a constant and a categorical input.
They do **not** fit models or compare full model predictions.

The 35 standard-deviation cases cover positive and negative adjacent values and
clusters near the largest double, at `1e308`, `1e-160` and `1e-200`, plus
representable and unrepresentable boundaries and a wide-spread example. Python
converts the actual binary64 input values to `Decimal` and computes sample
variance with 1,000 decimal digits of precision. The reference does not call R's
`sd()` or copy the package's normalization algorithm. A JSON null paired with
`unrepresentable: true` means that the true sample SD exceeds the double range.

The recorded run passed all 48 ordinary cases and all 35 SD cases. The largest
relative SD error was `2.22e-16`, below the `1e-12` comparison tolerance. See
[the verdict](verdict.json) and [individual SD results](results.json).

These checks ran on x86 Linux. The shipped tests separately preserve the
original macOS failure case and emulate ordinary double-precision accumulation
on every host. Actual macOS execution is recorded in release CI, not claimed by
this independent checker. A stored center is still a rounded double; exact
midpoints between adjacent extreme values need not be representable.

## Reproduce

From the repository root, with Python 3.9 or newer and the package's R development
dependencies installed:

```sh
python3 validation/numerical-review/generate-oracles.py
Rscript validation/numerical-review/check.R
```

Both commands use `~/.cache/autoxplain-numerical-review` for output. Set
`AXR_NUMERICAL_OUTPUT` to use another directory. The generator is deterministic;
the checker reads its `oracles.json`, verifies that the package source stays
unchanged during execution, and writes `results.json` and `verdict.json` with
the source fingerprint and platform. The pinned Git commit must be available in
the checkout. The scripts do not change packaged source or installed packages.
