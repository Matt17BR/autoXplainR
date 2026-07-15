# Contributing to AutoXplainR

Thanks for helping improve explanation reliability in R. Contributions are
welcome as issues, design discussions, documentation, tests, or code.

## Before opening code

For a material API or statistical-method change, open an issue first. Describe:

- the user claim the method is intended to support;
- the estimand and assumptions;
- failure modes under dependence, finite samples, or model multiplicity;
- the proposed object contract and backward-compatibility impact;
- validation evidence or a simulation plan.

Features are not accepted solely because they produce an attractive chart.
Changes must also preserve the beginner-first workflow and progressive
disclosure rules in [PRODUCT.md](PRODUCT.md).

## Local setup

```r
pak::pak(c("devtools", "testthat", "roxygen2", "lintr"))
devtools::load_all()
devtools::test()
devtools::check()
```

The ordinary suite must not start Java, access the internet, open a browser, or
require credentials. H2O tests are isolated and opt-in:

```sh
AUTOXPLAIN_RUN_H2O=true Rscript -e 'devtools::test(filter = "h2o")'
```

## Pull request checklist

- Add unit tests for happy paths, invalid inputs, and statistical edge cases.
- Add a deterministic simulation when a numerical method is introduced.
- Preserve the caller's random-number state for stochastic functions.
- Do not silently drop columns, rows, or factor levels.
- State whether intervals are Monte Carlo, bootstrap, or inferential.
- Add NEWS and user documentation for visible changes.
- Run `devtools::document()`, `devtools::test()`, and `devtools::check()`.
- Keep measured statement coverage at or above the enforced 80% project floor;
  new statistical branches should be covered substantially more deeply.
- Keep optional packages behind `requireNamespace()` checks.

All contributions are licensed under the project MIT license. Participation is
subject to the [Code of Conduct](CODE_OF_CONDUCT.md).
