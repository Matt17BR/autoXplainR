# Measure repeated prediction cost on one common evaluation batch

Benchmarks direct prediction through each retained explainer on the same
retained evaluation predictor rows. Model fitting, the guided workflow's
saved raw-data recipe and report caches are outside the timed call.
Transformations inside a custom prediction function are included. These
measurements describe this R process and backend, not deployment latency
or a general speed ranking.

## Usage

``` r
benchmark_predictions(
  result,
  models = NULL,
  batch_size = 256L,
  n_repeats = 7L,
  min_duration = 0.05,
  max_iterations = 1000L,
  max_seconds = 15,
  seed = 123L
)
```

## Arguments

- result:

  An
  [`autoxplain()`](https://matt17br.github.io/autoXplainR/reference/autoxplain.md)
  or
  [`evaluate_models()`](https://matt17br.github.io/autoXplainR/reference/evaluate_models.md)
  result.

- models:

  Optional retained model IDs or indices; NULL uses all models.

- batch_size:

  Maximum number of evaluation rows sampled without replacement.

- n_repeats:

  Number of measured repetitions, at least two.

- min_duration:

  Target elapsed seconds for an inner block of repeated predictions.
  Calibration also targets at least 20 observed clock steps.

- max_iterations:

  Maximum prediction calls in any inner block.

- max_seconds:

  Soft budget for warmup, calibration and measurement. A running backend
  call or inner block can exceed the remaining budget.

- seed:

  Seed for the common batch and interleaved model order. The caller's
  RNG state is preserved. Timings themselves are not reproducible
  numbers.

## Value

An `autoxplain_prediction_benchmark` containing `summary`, raw
`measurements`, `protocol` and model/evaluation identity fingerprints.
Attach it explicitly when rendering or store it as
`result$prediction_benchmark`.

## Details

Each model receives two warmup calls. Calibration chooses a block size,
then models are measured in a shuffled order in each repetition. The
smallest positive step observed in bounded clock polling is recorded; it
is an empirical clock observation, not a certified hardware resolution.
The 20-step target is an engineering guard against resolution-scale
timing, not a guarantee of stable measurements. Quartiles summarize
repeated measurements and are not confidence intervals.

Raw warmup, calibration and measurement records remain available even if
the budget expires, prediction fails, or elapsed time is not
sufficiently resolved. Summary costs are omitted when fewer than two
usable repetitions completed, clock resolution was not observed, any
measured block is resolution limited, or final identity could not be
verified. No model is refitted. A captured payload fingerprint detects
accidental edits to summaries, raw timings or protocol before
attachment; this is not a digital signature.

## Examples

``` r
result <- autoxplain(mtcars, "mpg", model_set = "quick", explain = FALSE)
bench <- benchmark_predictions(result, n_repeats = 2, min_duration = 0.02)
bench$summary
#>          model_id   status reason batch_rows repetitions requested_repetitions
#> 1      main_model computed                 6           2                     2
#> 2 simple_baseline computed                 6           2                     2
#>   min_ms_per_batch p25_ms_per_batch median_ms_per_batch p75_ms_per_batch
#> 1        0.4363636        0.4545455           0.4727273        0.4909091
#> 2        0.1803279        0.1864754           0.1926230        0.1987705
#>   max_ms_per_batch median_ms_per_row warning
#> 1        0.5090909        0.07878788        
#> 2        0.2049180        0.03210383        
```
