# Independent oracle: compare current indices with the published implementation's
# serialized-row keys. Tiny frames probe equality, not million-row performance.
args <- commandArgs(TRUE)
stopifnot(length(args) == 2L)
.libPaths(c(normalizePath(args[[1L]]), .libPaths()))
old <- asNamespace("AutoXplainR")
current <- new.env(parent = old)
sys.source("R/guided_workflow.R", envir = current)
legacy <- get("split_row_keys", envir = old)
checked <- 0L
check <- function(training, evaluation, label) {
  evaluation <- evaluation[names(training)]
  expected <- which(legacy(evaluation) %in% legacy(training))
  actual <- current$split_row_overlap(training, evaluation)
  if (!identical(expected, actual)) {
    saveRDS(list(training = training, evaluation = evaluation, expected = expected,
      actual = actual, label = label), file.path(args[[2L]], "overlap-witness.rds"))
    stop("Overlap mismatch: ", label)
  }
  checked <<- checked + 1L
}
utf8 <- enc2utf8(c("é", "雪", "plain"))
latin <- iconv("é", from = "UTF-8", to = "latin1")
bytes <- latin
Encoding(bytes) <- "bytes"
strings <- c(NA_character_, "", "NA", "NaN", "1", utf8, latin, bytes)
for (seed in seq_len(300L)) {
  set.seed(22000L + seed)
  n <- 28L
  pool <- data.frame(
    first_constant = rep(1, n),
    number = sample(c(-0, 0, NA_real_, NaN, Inf, -Inf, 1, 2, .Machine$double.xmax), n, TRUE),
    text = sample(strings, n, TRUE),
    flag = sample(c(TRUE, FALSE, NA), n, TRUE),
    integer = sample(c(NA_integer_, -2L, 0L, 2L), n, TRUE),
    category = factor(sample(c("a", "b", NA), n, TRUE)),
    date = as.Date(sample(c(0, 1, NA), n, TRUE), origin = "1970-01-01"))
  training <- pool[sample.int(n, 21L, replace = TRUE), ]
  evaluation <- pool[sample.int(n, 19L, replace = TRUE), ]
  if (seed %% 2L == 0L) evaluation$integer <- as.numeric(evaluation$integer)
  if (seed %% 3L == 0L) evaluation$category <- as.character(evaluation$category)
  if (seed %% 5L == 0L) evaluation$text[[1L]] <- "new-value"
  check(training, evaluation, paste("mixed", seed))
}
# Atomic values shared by every individual column do not imply a matching row.
check(data.frame(a = c(1, 2), b = c("a", "b")),
  data.frame(a = c(1, 2), b = c("b", "a")), "joint combinations")
for (kind in c("list", "complex", "raw")) {
  frame <- data.frame(x = c(0, 1, 1, 2))
  frame$unusual <- switch(kind,
    list = I(list(c(1, 2), 1, NULL, c(1, 2))),
    complex = c(1+1i, 1+0i, NA_complex_, 2+1i),
    raw = as.raw(c(1, 2, 1, 2)))
  check(frame, frame[c(4, 2, 1, 3), ], kind)
}
shared <- new.env(parent = emptyenv())
other <- new.env(parent = emptyenv())
frame <- data.frame(x = c(1, 1, 1))
frame$a <- I(list(shared, shared, other))
frame$b <- I(list(shared, other, shared))
check(frame[1:2, ], frame[3:1, ], "cross-column reference sharing")
check(data.frame(x = numeric()), data.frame(x = 1:3), "empty training")
check(data.frame(x = 1:3), data.frame(x = numeric()), "empty evaluation")
jsonlite::write_json(list(status = "passed", cases = checked,
  oracle = "Published0.6.2 serialized-row equality, using its installed namespace",
  baseline_library = getNamespaceInfo(old, "path"),
  source_md5 = unname(tools::md5sum("R/guided_workflow.R"))),
  file.path(args[[2L]], "overlap-independent.json"), auto_unbox = TRUE, pretty = TRUE)
cat(checked, "independent overlap cases passed\n")
