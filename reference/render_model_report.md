# Render an interactive model comparison

Creates a standalone HTML report from an
[`autoxplain()`](https://matt17br.github.io/autoXplainR/reference/autoxplain.md)
result. The report opens with model scores, effective settings and
measured costs. Focused tabs show feature importance, class-specific
fitted effects, input relationships, prediction errors and checks. Model
details expose the retained fit and its preprocessing; background
explanations use optional help and expandable details.

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
  max_models = 5L,
  uncertainty = FALSE,
  target_units = NULL
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

  Maximum displayed features per model when `audit` is not supplied. The
  audit uses their union; multiclass curves cover each outcome class.

- n_repeats:

  Permutation repeats when `audit` is not supplied.

- max_models:

  Maximum models audited when `audit` is not supplied.

- uncertainty:

  Include
  [`performance_uncertainty()`](https://matt17br.github.io/autoXplainR/reference/performance_uncertainty.md)
  using its default paired bootstrap. Off by default; temporal
  evaluation is not supported.

- target_units:

  Optional unit label for a numeric target. Used in the analysis brief
  and effect captions; no units are inferred.

## Value

The normalized output path, invisibly. Its `diagnostic_status` attribute
records optional checks performed for this report. The input result is
not changed by rendering.

## Examples

``` r
result <- autoxplain(mtcars, "mpg", seed = 2026)
path <- tempfile(fileext = ".html")
render_model_report(result, path, n_repeats = 3)
unlink(path)
```
