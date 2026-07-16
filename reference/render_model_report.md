# Render a beginner-first model report

Creates a standalone HTML report from an
[`autoxplain()`](https://matt17br.github.io/autoXplainR/reference/autoxplain.md)
result. The report leads with the prediction question, evaluation role,
simple-baseline comparison, and plain metric definitions. Feature
reliance, fitted effects, and an explanation evidence audit follow with
progressively more detail.

## Usage

``` r
render_model_report(
  result,
  output_file = "autoxplain-report.html",
  title = NULL,
  audit = NULL,
  effects = NULL,
  narrative = NULL,
  subgroup = NULL,
  open = FALSE,
  top_features = 8L,
  n_repeats = 20L,
  max_models = 5L
)
```

## Arguments

- result:

  An `autoxplain_result`.

- output_file:

  Destination `.html` path.

- title:

  Optional report title. `NULL` uses the target name.

- audit:

  Optional precomputed `autoxplain_audit`.

- effects:

  Optional named list of feature-effect objects.

- narrative:

  Optional narrative returned by
  [`generate_natural_language_report()`](https://matt17br.github.io/autoXplainR/reference/generate_natural_language_report.md).

- subgroup:

  Optional name of one categorical or low-cardinality column. When
  supplied, the report includes an explicit evaluation-set subgroup
  performance check. See
  [`subgroup_performance()`](https://matt17br.github.io/autoXplainR/reference/subgroup_performance.md).

- open:

  Open the report in a browser after writing it.

- top_features:

  Maximum number of features audited when `audit` is not supplied.

- n_repeats:

  Permutation repeats when `audit` is not supplied.

- max_models:

  Maximum models audited when `audit` is not supplied.

## Value

The normalized output path, invisibly.

## Examples

``` r
result <- autoxplain(mtcars, "mpg", seed = 2026)
path <- tempfile(fileext = ".html")
render_model_report(result, path, n_repeats = 3)
unlink(path)
```
