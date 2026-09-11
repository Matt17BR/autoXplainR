# Check the benchmark oracle against hand-computable predictions, including ties.
script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1L]])
source(file.path(dirname(normalizePath(script)), "common.R"))
y <- factor(c("no", "no", "yes", "yes"), levels = c("no", "yes"))
constant <- stress_metrics(y, rep(0.5, 4L), "binary")
stopifnot(abs(constant[["auc"]] - 0.5) < 1e-12,
  abs(constant[["average_precision"]] - 0.5) < 1e-12,
  abs(constant[["log_loss"]] - log(2)) < 1e-12,
  abs(constant[["brier"]] - 0.25) < 1e-12)
perfect <- stress_metrics(y, c(0, 0, 1, 1), "binary")
stopifnot(perfect[["auc"]] == 1, perfect[["average_precision"]] == 1,
  perfect[["brier"]] == 0, perfect[["recall_at_0_5"]] == 1)
reversed <- stress_metrics(y, c(0.9, 0.8, 0.2, 0.1), "binary")
stopifnot(reversed[["auc"]] == 0,
  abs(reversed[["average_precision"]] - (1 / 3 + 2 / 4) / 2) < 1e-12)
regression <- stress_metrics(c(1, 2, 3), c(2, 2, 2), "regression")
stopifnot(abs(regression[["rmse"]] - sqrt(2 / 3)) < 1e-12,
  regression[["mae"]] == 2 / 3, regression[["r_squared"]] == 0)
cat("Independent metric oracle agrees with constant, perfect, reversed and regression examples.\n")
