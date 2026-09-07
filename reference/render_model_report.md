# Render an interactive model comparison

Creates a standalone HTML report from an
[`autoxplain()`](https://matt17br.github.io/autoXplainR/reference/autoxplain.md)
or
[`evaluate_models()`](https://matt17br.github.io/autoXplainR/reference/evaluate_models.md)
result. It opens with model scores, effective settings and available
measured costs. Focused tabs show recorded model selection, supplied
data, feature importance, class-specific fitted effects, prediction
errors and checks. Model details expose the retained fit and recorded
preprocessing; supporting explanations use optional help and expandable
details.

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
  uncertainty = "auto",
  target_units = NULL,
  report_data = "summary",
  benchmark = NULL
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

  `"auto"` (default) includes the primary-versus-reference paired
  bootstrap from
  [`performance_uncertainty()`](https://matt17br.github.io/autoXplainR/reference/performance_uncertainty.md)
  when supported and records why it is unavailable otherwise. `TRUE`
  requires it; `FALSE` omits it. Intervals condition on the fitted
  models. Temporal evaluation is unsupported. Guided workflows use the
  intercept-only baseline as their reference; supplied-model results
  need an explicit reference.

- target_units:

  Optional unit label for a numeric target. Used in the analysis brief
  and effect captions; no units are inferred.

- report_data:

  Data included in HTML: `"summary"` for aggregate views, `"rows"` to
  include individual observations and predictions, or `"none"` to omit
  data exploration and case-level displays.
  [`report_data_control()`](https://matt17br.github.io/autoXplainR/reference/report_data_control.md)
  selects exported columns and the maximum number of rows. Hidden rows
  and panels remain accessible to anyone receiving the file.

- benchmark:

  Optional result of
  [`benchmark_predictions()`](https://matt17br.github.io/autoXplainR/reference/benchmark_predictions.md)
  made from the same unchanged models and evaluation data. Adds repeated
  prediction costs and their measurement protocol; rendering does not
  run a benchmark.

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
