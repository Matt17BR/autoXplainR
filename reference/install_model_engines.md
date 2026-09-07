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

  Print and visibly return the missing package plan without installing
  anything.

- ...:

  Additional arguments passed to
  [`utils::install.packages()`](https://rdrr.io/r/utils/install.packages.html).

## Value

With `dry_run = TRUE`, the missing package names visibly. With
`dry_run = FALSE`, the package names whose installation was requested,
invisibly.

## Examples

``` r
install_model_engines("recommended", task = "regression", dry_run = TRUE)
#> Dry run only; no packages were installed. All `recommended` portfolio backends for `regression` are available.
#> character(0)
```
