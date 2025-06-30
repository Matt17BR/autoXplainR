#' AutoXplainR: Automated Machine Learning Explanation and Comparative Reporting
#'
#' AutoXplainR provides an efficient, automated, and standardized method for machine learning 
#' practitioners to generate comparative model explanations from Automated Machine Learning (AutoML) outputs.
#' The package generates clear reports suitable for communicating model behavior to diverse audiences.
#'
#' @section Key Features:
#' \itemize{
#'   \item Seamless H2O AutoML integration
#'   \item Custom explanation implementations (permutation importance, PDP, etc.)
#'   \item Interactive plotly visualizations
#'   \item Comparative model analysis
#'   \item LLM-powered natural language reports
#'   \item Comprehensive HTML dashboards
#' }
#'
#' @section Main Functions:
#' \itemize{
#'   \item \code{\link{autoxplain}}: Main AutoML pipeline function
#'   \item \code{\link{calculate_permutation_importance}}: Feature importance calculation
#'   \item \code{\link{calculate_partial_dependence}}: Partial dependence plots
#'   \item \code{\link{generate_dashboard}}: Comprehensive dashboard generation
#'   \item \code{\link{generate_natural_language_report}}: LLM report generation
#' }
#'
#' @section Example Usage:
#' \preformatted{
#' library(AutoXplainR)
#' 
#' # Run AutoML analysis
#' data(iris)
#' result <- autoxplain(iris, "Species", max_models = 3)
#' 
#' # Generate explanations
#' importance <- calculate_permutation_importance(result$models[[1]], iris, "Species")
#' 
#' # Create dashboard
#' generate_dashboard(result, "analysis_dashboard.html")
#' }
#'
#' @keywords internal
#' "_PACKAGE"
#' @name AutoXplainR-package
#' @aliases AutoXplainR
#' @author Matteo Mazzarelli \\email{matteo.mazzarelli@@gmail.com}
#' @keywords package
#' @importFrom magrittr %>%
NULL

#' Pipe operator
#'
#' Re-export of magrittr's pipe operator
#' @importFrom magrittr %>%
#' @export
#' @rdname pipe
#' @name %>%
#' @keywords internal
NULL