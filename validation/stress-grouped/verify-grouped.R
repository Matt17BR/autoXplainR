# Independent coverage oracle. Run from the package root.
library_path <- Sys.getenv("AXR_GROUPED_LIBRARY")
if (nzchar(library_path)) {
  .libPaths(c(library_path, .libPaths()))
  library(AutoXplainR)
} else {
  pkgload::load_all(quiet = TRUE)
}
output <- Sys.getenv("AXR_GROUPED_OUTPUT")
if (!nzchar(output)) stop("Set AXR_GROUPED_OUTPUT to a JSON destination.")

expand_counts <- function(counts) {
  do.call(rbind, lapply(seq_len(nrow(counts)), function(group) {
    data.frame(group = group, outcome = rep(paste0("class", seq_len(ncol(counts))), counts[group, ]))
  }))
}

covers_classes <- function(assignment, counts, folds) {
  all(vapply(seq_len(folds), function(fold) {
    all(colSums(counts[assignment == fold, , drop = FALSE]) > 0)
  }, logical(1)))
}

# Enumerate set partitions, identifying permutations of the fold labels.
# This oracle never scores imbalance or follows the production greedy rule.
partitions <- function(n, k) {
  answer <- list()
  grow <- function(x) {
    if (length(x) == n) {
      if (max(x) == k) answer[[length(answer) + 1L]] <<- x
      return(invisible(NULL))
    }
    for (next_fold in seq_len(min(k, max(x) + 1L))) grow(c(x, next_fold))
  }
  grow(1L)
  answer
}
partition_cache <- list()
feasible_partition <- function(counts, folds) {
  if (any(colSums(counts > 0) < folds)) return(NULL)
  key <- paste(nrow(counts), folds)
  if (is.null(partition_cache[[key]])) {
    partition_cache[[key]] <<- partitions(nrow(counts), folds)
  }
  for (assignment in partition_cache[[key]]) {
    if (covers_classes(assignment, counts, folds)) return(assignment)
  }
  NULL
}

check_case <- function(counts, folds, witness, case_id, check_labels = FALSE) {
  data <- expand_counts(counts)
  before_rng <- .Random.seed
  result <- tryCatch(
    AutoXplainR:::grouped_fold_ids(data$group, folds, 7L, data$outcome), error = identity
  )
  stopifnot(identical(.Random.seed, before_rng))
  if (inherits(result, "error")) {
    return(list(case = case_id, feasible = !is.null(witness), success = FALSE,
                error = conditionMessage(result)))
  }
  allocation <- result[match(seq_len(nrow(counts)), data$group)]
  intact <- all(vapply(split(result, data$group), function(x) length(unique(x)) == 1L, logical(1)))
  stopifnot(intact, covers_classes(allocation, counts, folds), !is.null(witness))
  if (check_labels) {
    renamed <- paste0("renamed_", ncol(counts) - match(data$outcome, unique(data$outcome)))
    second <- AutoXplainR:::grouped_fold_ids(data$group, folds, 7L, renamed)
    stopifnot(identical(second, result))
  }
  list(case = case_id, feasible = TRUE, success = TRUE)
}

set.seed(51312)
random <- list()
start <- proc.time()[["elapsed"]]
for (index in seq_len(2500L)) {
  folds <- sample(2:4, 1)
  classes <- sample(3:5, 1)
  groups <- folds * sample(2:3, 1)
  witness <- rep(seq_len(folds), length.out = groups)
  zero_count <- if (index <= 1500L) 3L else 18L
  counts <- matrix(sample(c(rep(0L, zero_count), 1:12), groups * classes, TRUE), groups, classes)
  for (fold in seq_len(folds)) for (class in seq_len(classes)) {
    if (!any(counts[witness == fold, class] > 0)) {
      counts[sample(which(witness == fold), 1), class] <- sample(1:12, 1)
    }
  }
  for (group in which(rowSums(counts) == 0)) counts[group, sample(seq_len(classes), 1)] <- 1L
  stopifnot(covers_classes(witness, counts, folds))
  random[[index]] <- check_case(counts, folds, witness, index, check_labels = index %% 100L == 0L)
}
random_seconds <- proc.time()[["elapsed"]] - start
cat("Random known-feasible cases:", length(random), "in", random_seconds, "seconds\n")

# Every multiset of 3-class nonempty group supports, with 3 through 6 groups.
# Ordering groups by their bit mask avoids repeating equivalent support sets.
# These are exhaustive support multisets, not every ordering or row multiplicity.
small <- list()
start <- proc.time()[["elapsed"]]
for (groups in 3:6) {
  combinations <- combn(seq_len(7L + groups - 1L), groups)
  masks <- sweep(combinations, 1L, seq_len(groups) - 1L, "-")
  for (index in seq_len(ncol(masks))) {
    counts <- vapply(c(1L, 2L, 4L), function(bit) {
      as.integer(bitwAnd(masks[, index], bit) > 0)
    }, integer(groups))
    if (any(colSums(counts) == 0)) next
    for (folds in 2:3) {
      witness <- feasible_partition(counts, folds)
      small[[length(small) + 1L]] <- check_case(
        counts, folds, witness, paste(groups, index, folds, sep = ":")
      )
    }
  }
}
small_seconds <- proc.time()[["elapsed"]] - start
summarize <- function(records) {
  possible <- vapply(records, `[[`, logical(1), "feasible")
  success <- vapply(records, `[[`, logical(1), "success")
  list(cases = length(records), feasible = sum(possible), infeasible = sum(!possible),
       false_failures = sum(possible & !success), failures = records[possible & !success])
}
summary <- list(
  seed = 51312L, allocator_seed = 7L,
  allocator_source_hash = digest::digest(body(AutoXplainR:::grouped_class_fold_ids)),
  package_version = as.character(packageVersion("AutoXplainR")),
  random = summarize(random), random_elapsed_seconds = random_seconds,
  exhaustive_support_multisets = summarize(small), exhaustive_elapsed_seconds = small_seconds,
  class_renaming_checks = 25L,
  contracts = "All returned allocations preserve groups, cover every class in every fold and preserve RNG state."
)
jsonlite::write_json(summary, output, pretty = TRUE, auto_unbox = TRUE, digits = 16)
print(summary[c("random", "exhaustive_support_multisets")])
stopifnot(summary$random$false_failures == 0L,
          summary$exhaustive_support_multisets$false_failures == 0L)
