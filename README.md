# AutoXplainR

[![R-CMD-check](https://github.com/Matt17BR/autoXplainR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Matt17BR/autoXplainR/actions/workflows/R-CMD-check.yaml)
[![coverage](https://github.com/Matt17BR/autoXplainR/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/Matt17BR/autoXplainR/actions/workflows/test-coverage.yaml)
[![pkgdown](https://github.com/Matt17BR/autoXplainR/actions/workflows/pkgdown.yaml/badge.svg)](https://matt17br.github.io/autoXplainR/)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![License: MIT](https://img.shields.io/badge/license-MIT-2f7d5b.svg)](https://github.com/Matt17BR/autoXplainR/blob/main/LICENSE.md)

**Fit. Check. Compare. Understand. Explain.**

AutoXplainR helps someone fitting one of their first tabular models move from a
data frame to a responsible, shareable explanation. It creates a held-out test
set, compares an understandable model with a simple baseline, defines every
metric, investigates fitted patterns, and tells the user what the evidence does
and does not support.

The default path is local and needs no Java, cloud account, or LLM. One explicit
mode runs a leak-safe tournament across genuinely different model families—from
regularized and additive models to forests, boosting, kernels, neighbors, and
neural networks—without peeking at the final holdout. Advanced users can choose
families and search budgets, bring any fitted model, use H2O AutoML, or opt into
local or hosted language models.

**[Get started](https://matt17br.github.io/autoXplainR/articles/autoxplainr-introduction.html)** ·
**[Function reference](https://matt17br.github.io/autoXplainR/reference/index.html)** ·
**[Product contract](https://matt17br.github.io/autoXplainR/PRODUCT.html)** ·
**[Roadmap](https://matt17br.github.io/autoXplainR/ROADMAP.html)**

<p align="center">
  <img src="man/figures/guided-overview.png" alt="AutoXplainR report showing the modeling question, held-out baseline comparison, and four plain-language score cards" width="100%">
  <br><sub>A complete first screen: the question, unseen-data result, baseline, and evaluation size. No scrolling inside the image.</sub>
</p>

## Start with one call

```r
library(AutoXplainR)

result <- autoxplain(mtcars, target_column = "mpg", seed = 2026)
result
render_model_report(result, "my-model-report.html")
```

The default call fits two deliberately understandable references:

- a linear, logistic, or multinomial logistic primary model; and
- an intercept-only baseline that ignores every input.

Both are evaluated on rows kept out of fitting. The printed result answers the
first useful question—*did the model improve on a simple guess?*—before the
package asks anyone to interpret feature importance.

```text
<AutoXplainR guided result>
  question:   predict `mpg` (regression)
  engine:     base
  models:     2 (primary + baseline)
  result:     primary model has rmse = ...
  baseline:   ...% improvement in rmse
```

## Tune supervised models without tuning on the test set

Automatic tuning is a first-class local workflow, not an H2O-only escape hatch:

```r
# One-time installation of the recommended native engines
install_model_engines("recommended", task = "multiclass", dry_run = TRUE)
install_model_engines("recommended", task = "multiclass")

tuned <- autoxplain(
  iris,
  target_column = "Species",
  model_set = "tuned",
  nfolds = 5,
  seed = 2026
)

tuning_results(tuned)
render_model_report(tuned, "tuned-model-report.html")
```

The three presets keep the first choice understandable:

| Portfolio | Families | When to use it |
|---|---|---|
| `core` | linear, tree, neural | dependency-light teaching, examples, and a fast first tournament |
| `recommended` | linear, regularized, additive, tree, forest, boosting | the default serious tabular comparison after one explicit engine installation |
| `extended` | all ten families, adding neural, radial-kernel, nearest-neighbor, and MARS models | a broader sensitivity analysis when runtime and optional dependencies are acceptable |

The package itself supports R 4.1 and the dependency-light `core` portfolio.
The current CRAN release of XGBoost requires R 4.3 or newer; on older R,
`install_model_engines(..., dry_run = TRUE)` reports that constraint before
anything is installed.

Additive and MARS models support regression and binary outcomes, so they are
omitted from a multiclass tournament by the declared task contract—not by the
packages that happen to be installed. Missing engines produce one explicit
installation instruction; AutoXplainR never silently changes the requested
tournament.

Preprocessing is relearned inside each resampling fold. Automatic budgets are
large enough to vary the important controls in each family (15 core, 30
recommended, or 40 extended), while an explicit `max_models` is honored without
a hidden cap. The default one-standard-error rule first uses a documented,
reviewed family priority and then the least-flexible setting *within that
family*. It never compares arbitrary “complexity” numbers between unlike model
families.

Only the outer training rows take part in that search. The chosen configuration
is refitted on all training rows and then scored once on the held-out rows. The
report keeps these two numbers separate: the resampled score selects the model;
the held-out score evaluates the selected model.

The complete candidate grid, fold-level scores, paired out-of-fold predictions,
omitted-row accounting, warnings, failures, elapsed times, selected
hyperparameters, and data boundary remain available in `tuning_results(tuned)`.
Each family's best fitted model is retained so disagreement can be inspected,
not merely reported as a leaderboard number:

```r
behavior <- compare_model_behavior(tuned)
behavior

# Compare one fitted feature shape on aligned rows and common support
effects <- compare_model_effects(
  tuned,
  feature = "Petal.Length",
  method = "ale",
  class = "virginica"
)
plot(effects)
```

The two comparison functions answer different questions:

- `compare_model_behavior()` is the portfolio overview: family capacities,
  held-out scores, and same-row prediction disagreement, with optional
  permutation-importance rank evidence.
- `compare_model_effects()` asks how the retained fits respond to one named
  input on aligned evaluation rows and a common, support-checked grid.

Behavior cards are reviewed prior knowledge about representational capacity;
scores, prediction gaps, importance, and fitted curves are computed evidence.
Keeping those labels separate prevents “this model *can* learn interactions”
from becoming the false claim that it *did* use an interaction in this fit.
Neither an ALE nor a PDP curve estimates the causal effect of changing an
input.

Most users should stop at the presets. The advanced escape hatch is a validated
control object rather than another set of loose arguments:

```r
control <- tuning_control(
  grids = list(tree = data.frame(
    maxdepth = c(2L, 5L),
    cp = c(0.02, 0.002),
    minsplit = c(12L, 6L)
  )),
  family_budgets = c(linear = 1L, tree = 2L),
  metric = "mae",
  retain_oof = FALSE
)

advanced <- autoxplain(
  mtcars,
  "mpg",
  model_set = "tuned",
  learners = c("linear", "tree"),
  tuning_control = control,
  seed = 2026
)
```

`tuning_control()` can also accept ordinary supplied V-fold IDs and a strict
failure policy. Supplied IDs require an explicit outer `test_data`; they do not
silently turn random V-fold resampling into grouped or forward-chaining
validation. H2O AutoML remains optional when a Java-backed search or stacked
ensemble is appropriate.

## Compare models without hiding the trade-off

Comparison is not out of scope. It is a first-class, opt-in extension of the
same beginner workflow:

```r
comparison <- autoxplain(
  iris,
  target_column = "Sepal.Length",
  model_set = "comparison",
  test_fraction = 0.4,
  seed = 2026
)

model_tradeoffs(comparison)
plot_model_comparison(comparison)  # optional interactive Plotly view
render_model_report(comparison, "comparison-report.html")
```

The local comparison set contains the pre-specified statistical model, a small
decision tree, a more flexible tree, and the simple baseline. AutoXplainR shows
held-out performance, fit and prediction time, approximate R object size,
family-specific flexibility proxies, and the candidate-relative Pareto
frontier.

It does **not** collapse those dimensions into a mysterious score. It also does
not silently replace the primary model with whichever candidate happened to
win on the holdout: doing that and then reporting the same holdout score as
final performance would be optimistic.

Comparison mode also asks where the supplied non-baseline models give different
answers for the same held-out row:

```r
ambiguity <- prediction_ambiguity(comparison)
head(ambiguity$rows)
```

For regression this is a range in target units; for classification it is hard-
class disagreement plus a probability-distribution distance. These are model-
specification sensitivity checks, not confidence intervals. Candidate scores
remain beside the result because a poorly performing model can create large,
uninformative disagreement.

<p align="center">
  <img src="man/figures/model-comparison.png" alt="Complete AutoXplainR model-comparison section with score cards, a labeled Pareto frontier, interpretation guidance, and candidate table" width="100%">
  <br><sub>Performance versus an approximate model-object-size resource proxy, with the interpretation rule and full candidate table kept in the frame.</sub>
</p>

## One report, from result to responsible language

| Question | What AutoXplainR provides |
|---|---|
| What am I predicting? | detected task, target, split, preprocessing, and fitted family |
| Does the model work? | held-out metrics, plain definitions, baseline comparison, probability calibration, missingness shift, optional subgroup checks, and failure warnings |
| How was it tuned? | training-only folds, fold-specific preprocessing, candidate settings, one-standard-error selection, and an untouched final holdout |
| Are other models competitive? | explicit candidates, prediction comparisons, and Pareto trade-offs |
| What patterns does it use? | repeated permutation reliance plus ALE or diagnosed PDP effects |
| How much should I trust the explanation? | dependence, repeat stability, supplied-model disagreement, and next actions |
| How do I communicate it? | a deterministic local narrative, optional LLM adapter, and standalone HTML |

<table>
  <tr>
    <td width="50%" valign="top">
      <img src="man/figures/model-patterns.png" alt="AutoXplainR feature-reliance table and fitted-effect cards" width="100%"><br>
      <strong>Explain fitted patterns</strong><br>
      <sub>Reliance and effect direction, with causal boundaries adjacent to the result.</sub>
    </td>
    <td width="50%" valign="top">
      <img src="man/figures/explanation-reliability.png" alt="AutoXplainR explanation-reliability grade, diagnostics, warnings, and next actions" width="100%"><br>
      <strong>Stress-test the explanation</strong><br>
      <sub>A diagnostic grade, evidence, and concrete next actions—not a certification.</sub>
    </td>
  </tr>
</table>

Every screenshot above is a crop of the package's dependency-free HTML report,
not a separate design mock-up. The report remains readable after it is shared;
it does not require R, H2O, Plotly, or an LLM at viewing time.

## Open the analysis when you need more control

The guided result converts into ordinary model-agnostic explainers:

```r
explainer <- as_explainers(result, models = "main_model")$main_model

importance <- calculate_permutation_importance(
  explainer,
  n_repeats = 30,
  seed = 2026
)

effect <- explain_effect(explainer, feature = "wt")
```

Permutation intervals describe variation across random shuffles; they are not
population confidence intervals. ALE and PDP describe the fitted prediction
function; they do not establish the effect of intervening on an input.

For a multi-model reliability review:

```r
audit <- audit_explanations(
  as_explainers(comparison),
  n_repeats = 30,
  performance_tolerance = 0.05,
  seed = 2026
)

audit
render_explanation_report(audit, "technical-evidence.html")
```

The audit retains repeat-level importance, feature-dependence diagnostics,
near-optimal supplied models, prediction agreement, explanation disagreement,
and explicit permitted-claim labels.

## A complete explanation never requires an LLM

The deterministic local narrative is always the default—even if an API key is
present:

```r
memo <- generate_natural_language_report(result)
render_model_report(result, "report.html", narrative = memo)
```

Remote generation requires an explicit provider. Only aggregate metrics,
feature names, warnings, and provenance enter the prompt; raw rows, fitted
objects, case-level predictions, and secrets do not.

```r
narrative_providers()

memo <- generate_natural_language_report(result, provider = "gemini")
memo <- generate_natural_language_report(result, provider = "groq")
memo <- generate_natural_language_report(result, provider = "cloudflare")
memo <- generate_natural_language_report(result, provider = "ollama")
memo <- generate_natural_language_report(result, provider = "openrouter")
```

### What does the generated output look like?

The repository includes [complete narrative snapshots](https://matt17br.github.io/autoXplainR/LLM_EXAMPLES.html): a
deterministic memo refreshed from the current reproducible `iris` analysis and
an explicitly versioned live `gemini-3.5-flash` transport capture. The latter
is a communication example, not a numerical regression test. The live request
used validated structured output, `store = false`, and no fallback.

Here is a short excerpt from that Gemini response:

> **Held-out performance.** The generated memo separates the training-resampled
> selection score from the final score on different evaluation rows, then keeps
> causal, fairness, safety, and deployment claims outside the model's remit.

The full page shows the exact reproduction code, complete outputs, provider
provenance, what Gemini generated, and which interpretation boundaries the
package appended locally. Generated wording can vary; the computed evidence is
authoritative.

The [narrative provider guide](https://matt17br.github.io/autoXplainR/LLM_PROVIDERS.html) compares current free access,
privacy, structured-output support, reproducibility, and setup for Gemini,
Groq, Cloudflare Workers AI, Ollama, and OpenRouter. Provider, resolved model,
fallback, and prompt provenance are retained. Schema-capable providers return
five validated fields that AutoXplainR renders locally with fixed interpretation
boundaries; a schema controls format, not factual correctness.

## Classification, existing models, and H2O use the same contract

```r
# Multiclass classification
flowers <- autoxplain(iris, target_column = "Species", seed = 2026)

# Any existing model or framework
existing <- explain_model(
  fitted_object,
  data = held_out_data,
  y = "outcome",
  task = "binary",
  positive = "yes",
  predict_function = function(model, newdata) {
    # Return P(outcome == "yes") as a numeric vector.
  }
)

# Broader optional AutoML candidate set; requires Java and h2o
h2o_result <- autoxplain(
  training_data,
  target_column = "outcome",
  test_data = held_out_data,
  engine = "h2o",
  max_models = 10,
  max_runtime_secs = 0,              # let the fixed model budget stop the run
  exclude_algos = "DeepLearning",   # H2O's reproducibility recommendation
  evaluation_role = "test",         # assert only when study provenance supports it
  seed = 2026
)

# H2O's own selection evidence stays separate from common-row evaluation
h2o_result$engine_leaderboard
h2o_result$leaderboard
```

The first table records H2O's model-selection evidence. The second evaluates
that already selected primary model, alternative fitted models, and an
intercept-only baseline on aligned outer rows; outer ranks do not silently
select a new winner. H2O documents that wall-clock-constrained AutoML and Deep
Learning weaken reproducibility, so the returned provenance records both
caveats. See the official [H2O AutoML reference](https://docs.h2o.ai/h2o/latest-stable/h2o-r/docs/reference/h2o.automl.html).

Supplied `test_data` is deliberately called neutral `evaluation` data unless
you explicitly set `evaluation_role = "test"` or `"validation"`. Exact
duplicate-valued records across training and evaluation data produce a
possible-leakage warning because they may be the same observational unit—or
legitimate coincident records. Use `overlap_action = "error"` for a strict
pipeline, or `"ignore"` only after checking the data provenance.

Regression predictions are numeric vectors. Binary predictions are
positive-class probabilities. Multiclass predictions are named probability
matrices. The package validates that contract before an expensive explanation
begins.

For binary decisions, the report also shows what changes when the conventional
0.5 probability cutoff moves. The same values are available directly:

```r
binary_data <- transform(mtcars, am = factor(am, labels = c("automatic", "manual")))
binary_result <- autoxplain(binary_data, "am", seed = 2026)
threshold_diagnostics(binary_result, thresholds = c(0.3, 0.5, 0.7))
```

This is a false-positive/false-negative sensitivity check, not permission to
optimize a cutoff on the holdout and call that performance independently
validated.

## Installation

Install the current development version from GitHub. After a CRAN release is
available, the standard `install.packages("AutoXplainR")` path will be supported
as well.

```r
# install.packages("pak")
pak::pak("Matt17BR/autoXplainR")
```

## Interpretation boundaries

- Held-out performance describes the supplied evaluation data, not every future
  population or time period.
- A Pareto frontier covers only the supplied models and displayed objectives.
- Permutation importance estimates fitted-model reliance and is sensitive to
  dependent predictors.
- ALE reduces direct marginal extrapolation but remains associational.
- Generated prose can be wrong; the numerical report remains authoritative.
- AutoXplainR does not automate causal claims, fairness certification, safety
  approval, or deployment decisions.

## Where it fits

AutoXplainR complements modeling and explanation frameworks rather than hiding
them. [DALEX](https://jmlr.org/papers/v19/18-416.html), ingredients, and iml
provide broad explanation ecosystems; [xplainfi](https://mlr-org.github.io/xplainfi/articles/inference.html)
provides formal importance-inference tools; tidymodels, mlr3, and H2O provide
larger modeling systems. AutoXplainR's role is the approachable contract joining
fit, unseen-data evaluation, comparison, explanation reliability, and careful
communication.

The methods are grounded in Fisher, Rudin, and Dominici's
[model-class-reliance work](https://www.jmlr.org/papers/v20/18-760.html), Apley
and Zhu's [ALE method](https://arxiv.org/abs/1612.08468), and Biecek's
[DALEX framework](https://jmlr.org/papers/v19/18-416.html).

## Development and governance

```r
devtools::document()
devtools::test()
devtools::check()
```

The ordinary suite is dependency-light. Real H2O integration is isolated and
opt-in:

```sh
AUTOXPLAIN_RUN_H2O=true Rscript -e 'devtools::test(filter = "h2o")'
```

See the [contribution guide](https://matt17br.github.io/autoXplainR/CONTRIBUTING.html),
[security policy](https://matt17br.github.io/autoXplainR/SECURITY.html),
[governance](https://matt17br.github.io/autoXplainR/GOVERNANCE.html),
[release checklist](https://matt17br.github.io/autoXplainR/RELEASE_CHECKLIST.html),
and [release notes](https://matt17br.github.io/autoXplainR/news/index.html).
Questions and early ideas belong in
[GitHub Discussions](https://github.com/Matt17BR/autoXplainR/discussions).

## License

[MIT](https://github.com/Matt17BR/autoXplainR/blob/main/LICENSE.md) © 2025–2026 Matteo Mazzarelli.
