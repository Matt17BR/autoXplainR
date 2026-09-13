script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1L]])
source(file.path(dirname(normalizePath(script)), "common.R"))

# These cases are small enough to calculate by hand; they do not reuse package
# metrics, the benchmark model implementation, or sampled expected values.
regression <- tabular_metrics(c(0, 2), c(1, 1), "regression")
stopifnot(identical(regression$rmse, 1), identical(regression$mae, 1),
  identical(regression$r_squared, 0))
binary <- tabular_metrics(factor(c("no", "yes"), levels = c("no", "yes")), c(.25, .75), "binary")
stopifnot(abs(binary$log_loss + log(.75)) < 1e-15, binary$brier == .0625,
  binary$auc == 1, binary$average_precision == 1, binary$accuracy == 1)
protected <- I(cbind(no = c(.75, .25), yes = c(.25, .75)))
protected_binary <- tabular_metrics(factor(c("no", "yes"), levels = c("no", "yes")),
  protected, "binary")
stopifnot(protected_binary$auc == 1, protected_binary$average_precision == 1,
  protected_binary$brier == .0625, abs(protected_binary$log_loss + log(.75)) < 1e-15)
ties <- tabular_metrics(factor(c("no", "yes"), levels = c("no", "yes")), c(.5, .5), "binary")
stopifnot(ties$auc == .5, ties$average_precision == .5)
positive_ties <- tabular_metrics(factor(c("yes", "yes", "no"), levels = c("no", "yes")),
  rep(.5, 3L), "binary")
stopifnot(positive_ties$accuracy == 2 / 3)
y <- factor(c("c", "a", "b"), levels = c("a", "b", "c"))
p <- matrix(c(.7, .2, .1, .1, .7, .2, .2, .1, .7), byrow = TRUE, ncol = 3L,
  dimnames = list(NULL, c("c", "a", "b")))
multi <- tabular_metrics(y, p, "multiclass")
stopifnot(abs(multi$log_loss + log(.7)) < 1e-15,
  abs(multi$brier - .14) < 1e-15, multi$accuracy == 1,
  identical(multi, tabular_metrics(y, p[, c("a", "c", "b")], "multiclass")))
stopifnot(inherits(try(tabular_metrics(y, p[, 1:2], "multiclass"), silent = TRUE), "try-error"))

fitting <- data.frame(value = c(1, 2), category = c("b", "a"))
calibration <- data.frame(value = c(3, 4), category = c("c", NA_character_))
blueprint <- tabular_blueprint(fitting)
baked_fitting <- tabular_bake(fitting, blueprint, matrix = TRUE)
baked_calibration <- tabular_bake(calibration, blueprint, matrix = TRUE)
stopifnot(identical(colnames(baked_fitting), colnames(baked_calibration)),
  !anyNA(baked_calibration), !"categoryc" %in% colnames(baked_calibration),
  baked_calibration[1, "category__new__"] == 1,
  baked_calibration[2, "category__missing__"] == 1)

cache <- tabular_cache()
plan <- jsonlite::read_json(file.path(cache, "partitions.json"))
for (name in c("yearprediction", "covertype", "bank")) {
  development <- file.path(cache, "cases", name, "development")
  acceptance <- file.path(cache, "cases", name, "acceptance")
  dev_train <- readRDS(file.path(development, "training.rds"))
  dev_eval <- readRDS(file.path(development, "evaluation-features.rds"))
  final_train <- readRDS(file.path(acceptance, "training.rds"))
  final_eval <- readRDS(file.path(acceptance, "evaluation-features.rds"))
  # Deliberately do not read any acceptance outcomes here.
  stopifnot(!length(intersect(dev_train$source_rows, dev_eval$source_rows)),
    !length(intersect(final_train$source_rows, final_eval$source_rows)),
    all(dev_train$source_rows %in% final_train$source_rows),
    all(dev_eval$source_rows %in% final_train$source_rows),
    !"y" %in% names(dev_eval$data), !"y" %in% names(final_eval$data),
    length(dev_train$folds) == nrow(dev_train$data),
    identical(sort(unique(dev_train$folds)), 1:5))
  if (name == "yearprediction") {
    stopifnot(identical(final_train$source_rows, 1:463715),
      identical(final_eval$source_rows, 463716:515345))
  }
  if (name == "bank") stopifnot(!"duration" %in% names(final_train$data))
  cat("Verified partition isolation:", name, "\n")
}
cat("Independent metric arithmetic, category encoding and partition boundaries verified.\n")
