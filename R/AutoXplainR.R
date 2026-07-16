#' AutoXplainR: guided model fitting, evaluation, and explanation
#'
#' AutoXplainR gives a first-time modeler one safe path from a data frame to a
#' held-out evaluation and plain-language explanation. An explicit local mode
#' tunes a portfolio spanning linear, regularized, additive, tree, forest,
#' boosting, neural, kernel, nearest-neighbor, and piecewise-linear behavior
#' using only the training portion before the final holdout is scored. Its
#' evidence layer compares what viable model families learned, then stress-tests
#' explanations with repeated permutation importance, dependence diagnostics,
#' accumulated local effects, and reproducible provenance. H2O AutoML, Plotly, and remote
#' narratives are optional integrations rather than startup requirements.
#'
#' @section Primary workflow:
#' \enumerate{
#'   \item Call [autoxplain()] with a data frame and target column.
#'   \item Read the baseline comparison and held-out metrics.
#'   \item Use `model_set = "tuned"` and [tuning_results()] when automatic model
#'     selection is needed without tuning on the final evaluation rows.
#'   \item Preserve the fitted patterns and limitations with
#'     [render_model_report()].
#'   \item Convert the result with [as_explainers()] to investigate an effect
#'     with [explain_effect()] or run [audit_explanations()] for advanced checks.
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
