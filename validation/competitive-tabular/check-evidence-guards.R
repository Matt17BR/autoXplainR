# Development artifacts only; tamper checks operate on in-memory copies.
arguments <- commandArgs(TRUE)
stopifnot(length(arguments) == 2L)
script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1L]])
source(file.path(dirname(normalizePath(script)), "common.R"))
source(file.path(dirname(normalizePath(script)), "evidence-common.R"))
destination <- normalizePath(arguments[[1L]], mustWork = TRUE)
summary <- jsonlite::read_json(file.path(destination, "summary.json"))
stopifnot(identical(summary$phase, "development"), identical(summary$variant, "package"))
partition <- tabular_verify_partition(destination, summary)
training <- readRDS(file.path(partition$path, "training.rds"))
y <- readRDS(file.path(partition$path, "evaluation-targets.rds"))
predictions <- readRDS(file.path(destination, "predictions.rds"))
result <- readRDS(file.path(destination, "model.rds"))
checked <- character()
reject <- function(label, expression) {
  if (!inherits(try(force(expression), silent = TRUE), "try-error")) stop("Tamper unexpectedly passed: ", label)
  checked <<- c(checked, label)
}
verify <- function(r = result, p = predictions, s = summary) tabular_verify_package(r, p, s, training, y)
base <- verify()
stopifnot(length(base$ids) == length(result$models), length(base$forest_ids) >= 1L)
reject("empty predictions", verify(p = list()))
reject("missing retained prediction", verify(p = predictions[-1L]))
bad <- predictions; names(bad)[2L] <- names(bad)[1L]
reject("duplicate prediction IDs", verify(p = bad))
bad <- predictions; bad$extra <- predictions[[1L]]
reject("extra prediction", verify(p = bad))
bad <- result; bad$leaderboard <- bad$leaderboard[-1L, , drop = FALSE]
reject("missing leaderboard model", verify(r = bad))
bad <- result; bad$evaluation$metrics <- bad$evaluation$metrics[-1L]
reject("missing evaluation metrics", verify(r = bad))
bad <- summary; bad$metrics <- bad$metrics[-1L]
reject("missing summary metrics", verify(s = bad))
bad <- summary; first <- names(bad$metrics)[1L]; metric <- names(bad$metrics[[first]])[1L]
bad$metrics[[first]][[metric]] <- bad$metrics[[first]][[metric]] + .01
reject("altered summary metric", verify(s = bad))
bad <- result; bad$evaluation$metrics[[first]][[metric]] <- bad$evaluation$metrics[[first]][[metric]] + .01
reject("altered result metric", verify(r = bad))
bad <- result; bad$leaderboard[[metric]][1L] <- bad$leaderboard[[metric]][1L] + .01
reject("altered leaderboard metric", verify(r = bad))
bad <- summary; bad$leaderboard[[1L]][[metric]] <- bad$leaderboard[[1L]][[metric]] + .01
reject("altered summary leaderboard metric", verify(s = bad))
bad <- summary; bad$primary <- names(predictions)[2L]
reject("changed primary identity", verify(s = bad))
bad <- summary; bad$final_configuration <- "absent"
reject("changed final configuration", verify(s = bad))
forest <- base$forest_ids[[1L]]
bad <- result; bad$models[[forest]]$fit$num.samples <- NULL
reject("missing native forest rows", verify(r = bad))
bad <- result; bad$models[[forest]]$fit$num.samples <- Inf
reject("nonfinite native forest rows", verify(r = bad))
bad <- result; bad$models[[forest]]$fit$num.samples <- nrow(training$data) - 1L
reject("incomplete native forest rows", verify(r = bad))
bad <- result; bad$models[[forest]]$parameters$num.trees <- NULL
reject("missing requested forest count", verify(r = bad))
boost <- names(result$models)[vapply(result$models, function(m) inherits(m, "autoxplain_fitted_model") && identical(m$backend, "xgboost"), logical(1))]
stopifnot(length(boost) >= 1L)
bad <- result; bad$models[[boost[[1L]]]]$fit_details$computation$rows <- NULL
reject("missing XGBoost row metadata", verify(r = bad))
bad <- result; bad$models[[boost[[1L]]]]$parameters$nrounds <- NULL
reject("missing expected boosted rounds", verify(r = bad))
bad <- result; attr(bad$models[[boost[[1L]]]], "autoxplain_tuning_fit")$scope <- "screening"
reject("screening model presented as final", verify(r = bad))
if (identical(summary$request, "public-tabular")) {
  bad <- summary; bad$resolved_public_defaults$nfolds <- NULL
  reject("missing reported public fold count", tabular_verify_cv(result, bad, training))
  bad <- summary; bad$resolved_public_defaults$max_models <- NULL
  reject("missing reported public budget", tabular_verify_cv(result, bad, training))
  bad <- summary; bad$public_request$explicit_arguments <- c(bad$public_request$explicit_arguments, "seed")
  reject("overridden public default", tabular_verify_cv(result, bad, training))
}
bad <- result; bad$tuning$out_of_fold_predictions <- bad$tuning$out_of_fold_predictions[-1L, , drop = FALSE]
reject("missing OOF row", tabular_verify_cv(bad, summary, training))
bad <- result; bad$tuning$out_of_fold_predictions$training_row[2L] <- bad$tuning$out_of_fold_predictions$training_row[1L]
reject("duplicate OOF row", tabular_verify_cv(bad, summary, training))
keep <- setdiff(base$ids, base$forest_ids)
bad <- result; bad$models <- bad$models[keep]; bad$leaderboard <- bad$leaderboard[bad$leaderboard$model_id %in% keep, , drop = FALSE]
bad$evaluation$metrics <- bad$evaluation$metrics[keep]
s <- summary; s$metrics <- s$metrics[keep]; s$leaderboard <- Filter(function(row) row$model_id %in% keep, s$leaderboard)
reject("no retained forest despite consistent IDs", verify(r = bad, p = predictions[keep], s = s))
reject("acceptance freeze absent", tabular_verify_freeze(destination, ""))
reject("missing scalar", tabular_scalar(NULL, "fixture"))
reject("missing expected scalar", tabular_scalar(500, "fixture", NULL))
references <- list(worse = list(variant = "ranger", metrics = list(rmse = 12)),
  best = list(variant = "ranger", metrics = list(rmse = 10)), boost = list(variant = "xgboost", metrics = list(rmse = 9)))
limits <- tabular_quality_gates(list(primary = list(rmse = 9.45), forest = list(rmse = 11)), "primary", "forest", references, "regression")
stopifnot(limits$primary_pass, limits$forest_pass, abs(limits$primary_limit - 9.45) < 1e-12, limits$forest_limit == 11)
limits <- tabular_quality_gates(list(primary = list(rmse = 9.46), forest = list(rmse = 11.01)), "primary", "forest", references, "regression")
stopifnot(!limits$primary_pass, !limits$forest_pass)
reject("missing successful native forest", tabular_quality_gates(list(primary = list(rmse = 9), forest = list(rmse = 10)), "primary", "forest", references["boost"], "regression"))
references <- list(forest = list(variant = "ranger", metrics = list(log_loss = .2)), boost = list(variant = "xgboost", metrics = list(log_loss = .1)))
limits <- tabular_quality_gates(list(primary = list(log_loss = .11), forest = list(log_loss = .23)), "primary", "forest", references, "multiclass")
stopifnot(limits$primary_pass, limits$forest_pass, abs(limits$primary_limit - .112) < 1e-15, abs(limits$forest_limit - .232) < 1e-15)
# Endpoint binary arithmetic must check each documented definition, not reject
# truthful public evidence due to the two clipping operations' rounding difference.
y_edge <- factor(c("no", "yes"), levels = c("no", "yes"))
p_edge <- c(1, 0)
raw_edge <- tabular_metrics(y_edge, p_edge, "binary")
reported_edge <- tabular_reported_metrics(y_edge, p_edge, "binary")
stopifnot(abs(raw_edge$log_loss + log(1e-15)) < 1e-12,
  abs(reported_edge[["log_loss"]] + mean(c(log1p(-(1 - 1e-15)), log(1e-15)))) < 1e-12,
  abs(raw_edge$log_loss - reported_edge[["log_loss"]]) > 1e-4)
regression_metrics <- tabular_reported_metrics(c(0, 2), c(1, 1), "regression")
stopifnot(identical(regression_metrics, c(rmse = 1, mae = 1, r_squared = 0)))
y_multi <- factor(c("a", "b", "c"), levels = c("a", "b", "c"))
p_multi <- matrix(c(.7, .2, .1, .1, .7, .2, .2, .1, .7), nrow = 3L, byrow = TRUE,
  dimnames = list(NULL, levels(y_multi)))
multi_metrics <- tabular_reported_metrics(y_multi, p_multi, "multiclass")
stopifnot(abs(multi_metrics[["log_loss"]] + log(.7)) < 1e-12,
  abs(multi_metrics[["brier_score"]] - .14) < 1e-12,
  multi_metrics[["accuracy"]] == 1, multi_metrics[["macro_recall"]] == 1,
  abs(multi_metrics[["calibration_error"]] - .3) < 1e-12)
# The output is a new explicitly chosen cache record, never a benchmark summary.
output <- path.expand(arguments[[2L]])
if (file.exists(output)) stop("Refusing to replace previous guard evidence.")
tabular_json(list(status = "ok", scope = "Existing development artifact and in-memory tamper copies only; no fitting or acceptance outcomes.",
  source_run = destination, verified_models = base$ids, rejected = checked,
  quality_threshold_and_best_reference_checks = TRUE,
  script_sha256 = tabular_hash(script), helper_sha256 = tabular_hash(file.path(dirname(normalizePath(script)), "evidence-common.R"))), output)
cat("Verified complete development evidence and rejected", length(checked), "tamper cases.\n")
