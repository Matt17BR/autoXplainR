# Install optional model backends

Installs the CRAN packages required by an AutoXplainR model portfolio.
This helper only changes the user's library when called explicitly; use
`dry_run = TRUE` to inspect the missing packages without installing
them.

## Usage

``` r
install_model_engines(
  portfolio = c("recommended", "extended"),
  task = c("regression", "binary", "multiclass"),
  dry_run = FALSE,
  ...
)
```

## Arguments

- portfolio:

  One of `"recommended"` or `"extended"`.

- task:

  Task used to omit inapplicable learner backends. Defaults to
  regression, whose recommended portfolio also covers binary
  classification.

- dry_run:

  Return missing package names without installing them.

- ...:

  Additional arguments passed to
  [`utils::install.packages()`](https://rdrr.io/r/utils/install.packages.html).

## Value

The missing package names, invisibly. With `dry_run = FALSE`, these are
the packages whose installation was requested.
