# Compare what retained models do differently

`compare_model_behavior()` turns a multi-model
[`autoxplain()`](https://matt17br.github.io/autoXplainR/reference/autoxplain.md)
result into a common, model-agnostic comparison. It keeps two kinds of
evidence separate:

## Usage

``` r
compare_model_behavior(
  result,
  models = NULL,
  performance_tolerance = NULL,
  explanation_audit = NULL
)
```

## Arguments

- result:

  An `autoxplain_result` containing at least two comparable retained
  models.

- models:

  Model IDs, indices, or `NULL`. `NULL` compares every retained model
  not labeled as a baseline.

- performance_tolerance:

  Optional non-negative relative gap from the best selected evaluation
  score. `NULL` keeps all selected models.

- explanation_audit:

  Optional `autoxplain_audit` made from the same result, selected model
  IDs, and evaluation rows.

## Value

An `autoxplain_model_behavior` object. Its `models` table combines
performance, a clearly labeled trade-off proxy, and behavior cards;
`prediction_pairs` contains paired disagreement; `ambiguity` retains
case-level disagreement; `feature_evidence` contains optional
top-feature ranks and pairwise explanation-rank agreement; and
`findings` contains plain-language comparison statements.

## Details

- learner behavior cards describe what each model family can represent;
  they are reviewed prior knowledge, not findings from the supplied
  data;

- performance and paired prediction differences are computed from the
  result's common evaluation rows; serialized size or runtime, when
  shown, is an operational resource proxy rather than structural model
  complexity.

Optionally, pass an audit created with
`audit_explanations(as_explainers(result, models = ...))`. The
comparison then adds compact repeated-permutation-importance ranks and
pairwise rank agreement. Permutation importance measures model reliance
on the supplied evaluation data; it is not a causal effect or
population-level inference.

Effect-shape comparisons are deliberately not generated automatically.
Marginal curves need feature-specific grids and dependence checks, so a
generic shape claim would be easy to overstate. Use
[`explain_effect()`](https://matt17br.github.io/autoXplainR/reference/explain_effect.md)
for a named feature after reviewing the audit's dependence diagnostics.

## Examples

``` r
fit <- autoxplain(mtcars, "mpg", model_set = "comparison", seed = 2026)
compare_model_behavior(fit)
#> <AutoXplainR model behavior comparison>
#>   models:      3 (flexible_tree, small_tree, main_model)
#>   evidence:    6 held-out test rows
#>   performance: rmse (lower is better)
#>   trade-off:   model_size_kb (resource proxy)
#>   distance:    absolute difference in predicted target units
#>   feature check: not computed
#> 
#> Models at a glance
#>            model family backend  rmse relative_gap model_size_kb
#>  flexible_tree *   tree   rpart 1.825         0.0%         38.35
#>       small_tree   tree   rpart 3.032        66.1%         34.20
#>       main_model linear   stats 3.553        94.7%         44.44
#>   * best supplied evaluation score; rankings remain descriptive
#> 
#> What differs
#>   - flexible_tree has the best supplied rmse score (1.825).
#>   - flexible_tree and main_model differ most on average (2.62 using absolute difference in predicted target units).
#>   - Before considering this dataset, flexible_tree allows stepwise with automatic along tree paths; main_model allows none unless encoded in features with none unless specified in features.
#> 
#> Evidence key
#>   behavior cards = prior knowledge about model capacity
#>   metrics, prediction gaps, and optional permutation importance = computed evidence
#>   caution: descriptive comparison, not causal or uncertainty coverage
```
