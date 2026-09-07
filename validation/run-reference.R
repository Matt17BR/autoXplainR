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
  ale <- calculate_accumulated_local_effects(fit, x, feature = "x", n_points = 20)
  ale_reference <- iml::FeatureEffect$new(predictor, "x", method = "ale",
                                          grid.points = ale$x)$results
  ale_reference <- ale_reference[match(ale$x, ale_reference$x), ]
  # The packages use different finite-bin centering conventions. Compare the
  # accumulated shape after anchoring at the same minimum, not raw intercepts.
  ale_error <- max(abs((ale$accumulated_effect - ale$accumulated_effect[[1L]]) -
                         (ale_reference$.value - ale_reference$.value[[1L]])))
  stopifnot(is.finite(ale_error), ale_error < 1e-10)
  rbind(
    data.frame(scenario = scenario, method = "PDP", reference = "iml",
               rows = nrow(frame), grid_points = nrow(ours), max_absolute_error = error),
    data.frame(scenario = scenario, method = "ALE anchored shape", reference = "iml",
               rows = nrow(frame), grid_points = nrow(ale), max_absolute_error = ale_error)
  )
})
analytic <- lapply(c("irregular_linear", "tied_linear", "tied_quadratic"), function(scenario) {
  values <- if (scenario == "irregular_linear") {
    c(seq(0, 1, length.out = 51), seq(10, 100, length.out = 50))
  } else {
    rep(c(-3, -1, 0, 2, 10), c(5, 11, 7, 17, 9))
  }
  frame <- data.frame(x = values, z = sin(seq_along(values)))
  squared <- scenario == "tied_quadratic"
  predict_function <- function(model, newdata) {
    if (model$squared) newdata$x^2 + newdata$z else 3 * newdata$x + newdata$z
  }
  frame$y <- predict_function(list(squared = squared), frame)
  explainer <- explain_model(list(squared = squared), frame, "y", task = "regression",
                             predict_function = predict_function)
  effect <- explain_effect(explainer, feature = "x", method = "ale", n_points = 20)
  exact <- if (squared) effect$x^2 else 3 * effect$x
  expected <- exact - mean(stats::approx(effect$x, exact, xout = frame$x)$y)
  error <- max(abs(effect$accumulated_effect - expected))
  stopifnot(is.finite(error), error < 1e-10, all(attr(effect, "bin_counts") > 0L))
  data.frame(scenario = scenario, method = "ALE values and centering", reference = "closed-form additive",
             rows = nrow(frame), grid_points = nrow(effect), max_absolute_error = error)
})
results <- c(results, analytic)
utils::write.csv(do.call(rbind, results), "validation/results/reference-agreement.csv", row.names = FALSE)
writeLines(c(trimws(capture.output(sessionInfo()), which = "right"), paste("iml", utils::packageVersion("iml"))),
           "validation/results/reference-session.txt")
print(do.call(rbind, results))
