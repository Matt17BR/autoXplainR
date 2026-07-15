#' AutoXplainR: Evidence audits for model explanations
#'
#' AutoXplainR stress-tests explanatory claims for tabular predictive models.
#' Its model-agnostic core separates fitting from explanation, measures repeated
#' permutation variability, diagnoses feature dependence, compares explanations
#' across near-optimal supplied models, and renders a provenance-rich evidence
#' report. H2O AutoML, Plotly, and remote narrative generation are optional
#' integrations rather than package startup requirements.
#'
#' @section Primary workflow:
#' \enumerate{
#'   \item Wrap one or more fitted models with [explain_model()].
#'   \item Run [audit_explanations()] on held-out evaluation data.
#'   \item Investigate effects with [explain_effect()], which defaults to ALE.
#'   \item Preserve the evidence and limitations with
#'     [render_explanation_report()].
#' }
#'
#' @section Interpretation boundaries:
#' Permutation importance describes reliance of a fitted model on evaluation
#' data. It does not establish causality. Intervals reported by the default
#' estimator describe Monte Carlo variation across permutations, not sampling
#' uncertainty in a target population. Audit grades are transparent heuristics,
#' not safety, fairness, statistical, or regulatory certification.
#'
#' @references
#' Fisher A, Rudin C, Dominici F (2019). "All Models are Wrong, but Many are
#' Useful: Learning a Variable's Importance by Studying an Entire Class of
#' Prediction Models Simultaneously." *Journal of Machine Learning Research*,
#' 20(177), 1--81.
#'
#' Apley DW, Zhu J (2020). "Visualizing the Effects of Predictor Variables in
#' Black Box Supervised Learning Models." *Journal of the Royal Statistical
#' Society: Series B*, 82(4), 1059--1086.
#'
#' @docType package
#' @name AutoXplainR-package
#' @aliases AutoXplainR
#' @importFrom stats predict setNames
#' @importFrom utils head
#' @keywords internal
"_PACKAGE"
