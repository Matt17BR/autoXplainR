# Exact replay comparison, with two explicitly defined binary evidence repairs.
# Usage: Rscript compare-streaming.R baseline.rds candidate.rds verdict.json
args <- commandArgs(TRUE)
stopifnot(length(args) == 3L)
before <- readRDS(args[[1L]])
after <- readRDS(args[[2L]])
stopifnot(identical(names(before), names(after)), length(after) == 6L)
changes <- list()
metadata <- list()
for (name in names(before)) {
  old <- before[[name]]
  new <- after[[name]]
  # The old adapter already reports maxit = 500 in learned fit evidence. The
  # current grid also names it explicitly. Require that exact budget before
  # removing only the new redundant annotation from this fixed-budget replay.
  neural <- which(startsWith(new$folds$configuration_id, "neural_"))
  explicit <- 0L
  for (i in neural) {
    if (is.null(new$folds$requested_parameters[[i]]$maxit)) next
    stopifnot(identical(old$folds$learned[[i]]$maxit, 500L))
    stopifnot(identical(new$folds$learned[[i]]$maxit, 500L))
    for (kind in c("requested", "effective")) {
      parameters <- paste0(kind, "_parameters")
      key <- paste0(kind, "_parameter_key")
      stopifnot(identical(new$folds[[parameters]][[i]]$maxit, 500L))
      stopifnot(endsWith(new$folds[[key]][[i]], "|maxit=integer:500"))
      new$folds[[parameters]][[i]]$maxit <- NULL
      new$folds[[key]][[i]] <- sub("|maxit=integer:500", "", new$folds[[key]][[i]], fixed = TRUE)
    }
    note <- new$folds$learned[[i]]$call_reconstruction
    stopifnot(is.character(note), length(note) == 1L, nzchar(note))
    new$folds$learned[[i]]$call_reconstruction <- NULL
    explicit <- explicit + 1L
  }
  if (explicit) {
    candidate <- which(new$candidates$family == "neural")
    stopifnot(all(endsWith(new$candidates$hyperparameters[candidate], ", iteration limit = 500")))
    new$candidates$hyperparameters[candidate] <- sub(", iteration limit = 500", "",
      new$candidates$hyperparameters[candidate], fixed = TRUE)
  }
  metadata[[name]] <- list(explicit_500_iteration_fold_records = explicit)
  fields <- setdiff(names(old), "oof")
  stopifnot(identical(old[fields], new[fields]))
  old_rows <- old$oof
  new_rows <- new$oof
  if (!startsWith(name, "binary/")) {
    stopifnot(identical(old_rows, new_rows))
    next
  }
  unchanged <- setdiff(names(old_rows), c("case_loss", "predicted_class"))
  stopifnot(identical(old_rows[unchanged], new_rows[unchanged]))
  classes <- colnames(new_rows$probabilities)
  stopifnot(length(classes) == 2L)
  p <- new_rows$probabilities[, classes[[2L]]]
  truth <- new_rows$truth == classes[[2L]]
  clipped <- pmin(pmax(p, 1e-15), 1 - 1e-15)
  oracle <- if (endsWith(name, "/brier")) {
    (clipped - as.numeric(truth))^2
  } else {
    -ifelse(truth, log(clipped), log1p(-clipped))
  }
  labels <- ifelse(p >= .5, classes[[2L]], classes[[1L]])
  stopifnot(identical(unname(new_rows$case_loss), unname(oracle)))
  stopifnot(identical(as.character(new_rows$predicted_class), unname(labels)))
  changed_labels <- as.character(old_rows$predicted_class) != labels
  stopifnot(all(p[changed_labels] == .5))
  changes[[name]] <- list(
    case_losses_changed = sum(old_rows$case_loss != new_rows$case_loss),
    max_absolute_case_loss_change = max(abs(old_rows$case_loss - new_rows$case_loss)),
    half_probability_labels_corrected = sum(changed_labels)
  )
}
jsonlite::write_json(list(
  passed = TRUE,
  workflows = length(after),
  out_of_fold_rows = sum(vapply(after, function(x) nrow(x$oof), integer(1))),
  unchanged = paste(
    "Exact equality for settings, selection, fold scores, fit seeds, omissions, preprocessing,",
    "all out-of-fold probabilities and every final holdout prediction."
  ),
  intentional_repairs = paste(
    "Binary case losses use the same probability clipping and arithmetic as CV scoring;",
    "binary 0.5 probabilities select the positive class. Each candidate value is checked",
    "against an independent explicit formula; no general numeric tolerance or normalization is used."
  ),
  metadata_comparison = paste(
    "Both versions use the former 500-iteration neural budget. Require that exact value",
    "in requested/effective settings and learned evidence, then remove only its new",
    "redundant parameter-key/display annotation and the new call-reconstruction prose.",
    "The default 2,000-iteration search is measured separately."
  ),
  metadata = metadata,
  changes = changes
), args[[3L]], auto_unbox = TRUE, pretty = TRUE, digits = 16)
cat("Passed exact streaming replay with explicitly checked binary evidence repairs.\n")
