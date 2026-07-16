# Package index

## Guided workflow

- [`autoxplain()`](https://matt17br.github.io/autoXplainR/reference/autoxplain.md)
  : Fit and evaluate a model through a guided workflow
- [`tuning_results()`](https://matt17br.github.io/autoXplainR/reference/tuning_results.md)
  : Inspect automatic tuning evidence
- [`tuning_control()`](https://matt17br.github.io/autoXplainR/reference/tuning_control.md)
  : Advanced controls for local model tuning
- [`as_explainers()`](https://matt17br.github.io/autoXplainR/reference/as_explainers.md)
  : Convert an AutoML result to model-agnostic explainers
- [`calibration_diagnostics()`](https://matt17br.github.io/autoXplainR/reference/calibration_diagnostics.md)
  : Check whether held-out class probabilities behave literally
- [`threshold_diagnostics()`](https://matt17br.github.io/autoXplainR/reference/threshold_diagnostics.md)
  : Explore binary decision-threshold trade-offs
- [`missingness_shift()`](https://matt17br.github.io/autoXplainR/reference/missingness_shift.md)
  : Compare raw missing-value rates between training and evaluation data
- [`subgroup_performance()`](https://matt17br.github.io/autoXplainR/reference/subgroup_performance.md)
  : Compare held-out performance across an explicitly chosen group
- [`render_model_report()`](https://matt17br.github.io/autoXplainR/reference/render_model_report.md)
  : Render a beginner-first model report
- [`generate_natural_language_report()`](https://matt17br.github.io/autoXplainR/reference/generate_natural_language_report.md)
  : Generate an evidence-constrained narrative
- [`narrative_providers()`](https://matt17br.github.io/autoXplainR/reference/narrative_providers.md)
  : List supported narrative providers
- [`prediction_ambiguity()`](https://matt17br.github.io/autoXplainR/reference/prediction_ambiguity.md)
  : Find held-out rows where supplied models disagree
- [`compare_model_behavior()`](https://matt17br.github.io/autoXplainR/reference/compare_model_behavior.md)
  : Compare what retained models do differently
- [`compare_model_effects()`](https://matt17br.github.io/autoXplainR/reference/compare_model_effects.md)
  : Compare one fitted feature effect across retained models

## Model-agnostic evidence workflow

- [`explain_model()`](https://matt17br.github.io/autoXplainR/reference/explain_model.md)
  : Create a model-agnostic explainer
- [`predict(`*`<autoxplain_explainer>`*`)`](https://matt17br.github.io/autoXplainR/reference/predict.autoxplain_explainer.md)
  : Predict with an AutoXplainR explainer
- [`audit_explanations()`](https://matt17br.github.io/autoXplainR/reference/audit_explanations.md)
  : Stress-test the evidence behind model explanations
- [`summary(`*`<autoxplain_audit>`*`)`](https://matt17br.github.io/autoXplainR/reference/summary.autoxplain_audit.md)
  : Summarize an explanation audit
- [`render_explanation_report()`](https://matt17br.github.io/autoXplainR/reference/render_explanation_report.md)
  : Render a standalone explanation evidence report
- [`explain_effect()`](https://matt17br.github.io/autoXplainR/reference/explain_effect.md)
  : Estimate a model feature effect

## Explanation estimators

- [`calculate_permutation_importance()`](https://matt17br.github.io/autoXplainR/reference/calculate_permutation_importance.md)
  : Repeated permutation feature importance with uncertainty diagnostics
- [`calculate_accumulated_local_effects()`](https://matt17br.github.io/autoXplainR/reference/calculate_accumulated_local_effects.md)
  : Calculate accumulated local effects data
- [`calculate_partial_dependence()`](https://matt17br.github.io/autoXplainR/reference/calculate_partial_dependence.md)
  : Calculate partial dependence data
- [`calculate_partial_dependence_multi()`](https://matt17br.github.io/autoXplainR/reference/calculate_partial_dependence_multi.md)
  : Calculate PDPs for multiple features

## Fitting and preprocessing

- [`learner_catalog()`](https://matt17br.github.io/autoXplainR/reference/learner_catalog.md)
  : Inspect AutoXplainR learner families
- [`install_model_engines()`](https://matt17br.github.io/autoXplainR/reference/install_model_engines.md)
  : Install optional model backends
- [`preprocess_data()`](https://matt17br.github.io/autoXplainR/reference/preprocess_data.md)
  : Prepare modeling data with an auditable recipe

## Visualization and metadata

- [`plot_model_comparison()`](https://matt17br.github.io/autoXplainR/reference/plot_model_comparison.md)
  : Plot model performance and resource trade-offs
- [`plot_model_correlations()`](https://matt17br.github.io/autoXplainR/reference/plot_model_correlations.md)
  : Plot prediction agreement among retained AutoML models
- [`plot_partial_dependence()`](https://matt17br.github.io/autoXplainR/reference/plot_partial_dependence.md)
  : Plot a PDP or ALE feature effect
- [`plot_partial_dependence_multi()`](https://matt17br.github.io/autoXplainR/reference/plot_partial_dependence_multi.md)
  : Plot multiple feature effects
- [`plot_permutation_importance()`](https://matt17br.github.io/autoXplainR/reference/plot_permutation_importance.md)
  : Plot permutation importance with uncertainty
- [`plot(`*`<autoxplain_model_effects>`*`)`](https://matt17br.github.io/autoXplainR/reference/plot.autoxplain_model_effects.md)
  : Plot aligned cross-model effects
- [`model_tradeoffs()`](https://matt17br.github.io/autoXplainR/reference/model_tradeoffs.md)
  : Compare supplied models without hiding trade-offs
- [`extract_model_characteristics()`](https://matt17br.github.io/autoXplainR/reference/extract_model_characteristics.md)
  : Extract comparable model metadata

## Compatibility helpers

Legacy entry points retained for existing code; new analyses should use
the guided evidence workflow above.

- [`generate_dashboard()`](https://matt17br.github.io/autoXplainR/reference/generate_dashboard.md)
  : Generate a guided model report
- [`create_simple_dashboard()`](https://matt17br.github.io/autoXplainR/reference/create_simple_dashboard.md)
  : Create a lightweight AutoXplainR dashboard
- [`create_model_comparison_report()`](https://matt17br.github.io/autoXplainR/reference/create_model_comparison_report.md)
  : Create a compact model metadata report
- [`calculate_weighted_efficiency()`](https://matt17br.github.io/autoXplainR/reference/calculate_weighted_efficiency.md)
  : Calculate a relative weighted model score
- [`preprocess_for_h2o()`](https://matt17br.github.io/autoXplainR/reference/preprocess_for_h2o.md)
  : Prepare a data frame for the H2O adapter
