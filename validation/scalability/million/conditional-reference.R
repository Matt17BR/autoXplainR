# Known regression conditional mean after the fixture hides x3 independently.
# This is a diagnostic reference, never a candidate or a selection input.
args <- commandArgs(TRUE)
stopifnot(length(args) == 1L)
script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
source(file.path(dirname(normalizePath(script)), "fixtures.R"))
fixture <- scale_fixture("regression", 20000L, evaluation = TRUE)
data <- fixture$data
observed_x3 <- ifelse(is.na(data$x3), 0, data$x3)
# x3 is independent of the other inputs, has mean zero and is missing at random.
# x8 has no role in the generating mean. All other relevant inputs are observed.
conditional_mean <- 2 * sin(data$x1) + 0.75 * (data$x2^2 - 1) +
  1.5 * observed_x3 * data$x4 + 0.8 * data$x5
stopifnot(identical(conditional_mean[!is.na(data$x3)], fixture$truth[!is.na(data$x3)]))
jsonlite::write_json(list(
  scope = "Known conditional-mean reference on the fixed independent regression evaluation set. Not used for fitting or selecting models.",
  rows = nrow(data), hidden_x3_rows = sum(is.na(data$x3)),
  complete_input_mean_rmse = independent_scale_loss(data$outcome, fixture$truth),
  observed_input_conditional_mean_rmse = independent_scale_loss(data$outcome, conditional_mean),
  population_conditional_mean_rmse = sqrt(0.7^2 + 0.02 * 1.5^2),
  derivation = paste(
    "Independent mean-zero x3 contributes zero to the conditional mean when hidden.",
    "Its expected residual variance adds 0.02 * 1.5^2 = 0.045 to the 0.49 noise variance,",
    "because Var(x3) = E(x4^2) = 1."
  ),
  fixture_source_md5 = unname(tools::md5sum(file.path(dirname(normalizePath(script)), "fixtures.R")))
), args[[1L]], pretty = TRUE, auto_unbox = TRUE, digits = 16)
