# Public synthetic data; generated output is suitable for sharing.
if (dir.exists("R")) pkgload::load_all(quiet = TRUE) else library(AutoXplainR)
set.seed(82)
data <- data.frame(x = rnorm(240), z = rnorm(240))
data$outcome <- 3 * data$x + sin(data$z) + rnorm(240)
result <- autoxplain(data, "outcome", model_set = "comparison")
dir.create("pkgdown/assets", recursive = TRUE, showWarnings = FALSE)
render_model_report(result, "pkgdown/assets/model-report.html", uncertainty = TRUE)
