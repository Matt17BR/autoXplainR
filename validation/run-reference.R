# Run from the repository root after installing AutoXplainR, or use pkgload.
# This file uses published package APIs; no reference source is copied.
if (dir.exists("R")) pkgload::load_all(quiet = TRUE) else library(AutoXplainR)
stopifnot(requireNamespace("iml", quietly = TRUE))
dir.create("validation/results", recursive = TRUE, showWarnings = FALSE)
set.seed(20260907)
data <- data.frame(x = rnorm(300), z = rnorm(300), noise = rnorm(300))
scenarios <- list(
  additive = function(x, z) 3 * x - 2 * z,
  interaction = function(x, z) 2 * x * z,
  nonlinear = function(x, z) sin(2 * x) + z^2,
  correlated = function(x, z) 3 * x - 2 * z
)
results <- lapply(names(scenarios), function(scenario) {
  frame <- data
  if (scenario == "correlated") frame$z <- 0.95 * frame$x + 0.1 * frame$z
  frame$y <- scenarios[[scenario]](frame$x, frame$z) + rnorm(nrow(frame), sd = 0.2)
  fit <- rpart::rpart(y ~ ., data = frame, control = rpart::rpart.control(cp = 0.001))
  x <- frame[c("x", "z", "noise")]
  ours <- calculate_partial_dependence(fit, x, feature = "x", n_points = 20)
  predictor <- iml::Predictor$new(fit, data = x, y = frame$y)
  reference <- iml::FeatureEffect$new(predictor, "x", method = "pdp", grid.points = ours$x)$results
  reference <- reference[match(ours$x, reference$x), ]
  error <- max(abs(ours$partial_dependence - reference$.value))
  stopifnot(is.finite(error), error < 1e-10)
  data.frame(scenario = scenario, method = "PDP", reference = "iml",
             rows = nrow(frame), grid_points = nrow(ours), max_absolute_error = error)
})
utils::write.csv(do.call(rbind, results), "validation/results/reference-agreement.csv", row.names = FALSE)
writeLines(c(capture.output(sessionInfo()), paste("iml", utils::packageVersion("iml"))),
           "validation/results/reference-session.txt")
print(do.call(rbind, results))
