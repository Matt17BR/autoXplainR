data_explorer_fixture <- function(strategy = "impute") {
  training <- data.frame(
    x = c(NA, 2:20), kind = rep(c("a", "b"), 10),
    y = 2 * seq_len(20), private_id = paste0("TRAIN_PRIVATE_", seq_len(20))
  )
  evaluation <- data.frame(
    x = c(NA, 1, 10, 20, 21, 30), kind = c("new", "a", "b", "a", NA, "b"),
    y = 2 * c(1, 1, 10, 20, 21, 30), private_id = paste0("EVAL_PRIVATE_", 1:6)
  )
  train <- preprocess_data(training[c("x", "kind", "y")], "y", missing_value_strategy = strategy)
  test <- AutoXplainR:::apply_preprocessing_recipe(evaluation, train$recipe, "y",
    missing_value_strategy = strategy, novel_level_strategy = "mode"
  )
  context <- AutoXplainR:::capture_data_context(training, evaluation, "y", c("x", "kind"), train, test)
  structure(list(
    training_data = train$data, test_data = test$data, target_column = "y",
    features = c("x", "kind"), data_context = context, provenance = list(seed = 4)
  ), class = "autoxplain_result")
}

test_that("raw snapshots and source mappings survive preprocessing and reject changed attachments", {
  result <- data_explorer_fixture("drop_rows")
  context <- result$data_context
  evaluation <- context$row_map[context$row_map$partition == "evaluation", ]
  expect_identical(evaluation$processed_position, c(NA_integer_, 1L, 2L, 3L, NA_integer_, 4L))
  expect_identical(evaluation$source_row, 1:6)
  expect_identical(evaluation$row_key, paste0("test_data:", 1:6))
  expect_true(is.na(context$raw$training$x[1]))
  expect_false(anyNA(result$training_data$x))
  expect_silent(AutoXplainR:::validate_data_context(context, result$training_data, result$test_data))
  changed <- context
  changed$row_map$source_row[1] <- 100L
  expect_error(AutoXplainR:::validate_data_context(changed, result$training_data, result$test_data),
               "row mapping changed")
  changed <- result$test_data
  changed$x[1] <- 999
  expect_error(AutoXplainR:::validate_data_context(context, result$training_data, changed), "processed result rows")
  restored <- unserialize(serialize(result, NULL))
  expect_identical(
    AutoXplainR:::prepare_data_explorer(restored)$profile,
    AutoXplainR:::prepare_data_explorer(result)$profile
  )
})

test_that("numeric profiles use common bins, explicit overflow and independently counted pairs", {
  axis <- AutoXplainR:::data_axis(c(0, 2, 4, 6, 8), c(-1, 0, 1, 8, 10, NA), bins = 4)
  expect_equal(axis$breaks, c(0, 2, 4, 6, 8))
  code <- AutoXplainR:::data_bin_codes(c(-1, 0, 1, 2, 8, 10, NA, Inf), axis)
  expect_identical(code, c(1L, 2L, 2L, 3L, 5L, 6L, NA_integer_, NA_integer_))
  distribution <- AutoXplainR:::data_distribution(c(-1, 0, 1, 2, 8, 10, NA, Inf), axis, 8L)
  expect_identical(distribution$counts, c(1L, 2L, 1L, 0L, 1L, 1L))
  expect_equal(distribution$n_missing, 1)
  expect_equal(distribution$n_nonfinite, 1)
  expect_equal(distribution$n_used, 6)
  pair <- AutoXplainR:::data_pair_profile(c(0, 1, 2, 3, NA), c(0, 2, 4, NA, 8), axis, axis, 5L)
  expect_equal(pair$cells, data.frame(x = c(2L, 2L, 3L), y = c(2L, 3L, 4L), n = c(1L, 1L, 1L)))
  expect_equal(pair$n_complete, 3)
  expect_equal(pair$n_excluded, 2)
  expect_equal(pair$conditional$mean, c(1, 4))
  expect_equal(pair$conditional$n, c(2L, 1L))
})

test_that("categorical counts preserve missing values, other levels and novel categories", {
  axis <- AutoXplainR:::data_axis(c("a", "a", "b", "c", "(missing)"),
    c("a", "new", NA, "(missing)"),
    max_levels = 1
  )
  expect_identical(axis$levels, "a")
  expect_identical(
    AutoXplainR:::data_bin_codes(c("a", "b", "new", NA, "(missing)"), axis),
    c(1L, 2L, 3L, NA_integer_, 2L)
  )
  distribution <- AutoXplainR:::data_distribution(c("a", "new", NA, "(missing)"), axis, 4L)
  expect_identical(distribution$counts, c(1L, 1L, 1L))
  expect_equal(distribution$n_missing, 1L)
  ordered <- ordered(c("high", "low", "middle"), levels = c("low", "middle", "high"))
  expect_identical(AutoXplainR:::data_axis(ordered, ordered)$levels, levels(ordered))
})

test_that("summary exports use full populations and omit raw rows and unrequested context values", {
  result <- data_explorer_fixture()
  summary <- AutoXplainR:::prepare_data_explorer(result)
  expect_null(summary$rows)
  expect_identical(summary$manifest$columns, c("y", "x", "kind"))
  expect_false("private_id" %in% summary$profile$columns$name)
  text <- as.character(jsonlite::toJSON(summary, auto_unbox = TRUE))
  expect_false(grepl("TRAIN_PRIVATE|EVAL_PRIVATE", text))
  raw <- summary$profile$stages$raw$columns$x
  processed <- summary$profile$stages$processed$columns$x
  expect_equal(raw$training$n_missing, 1L)
  expect_equal(processed$training$n_missing, 0L)
  expect_equal(raw$evaluation$n_total, 6L)
  row_mode <- AutoXplainR:::prepare_data_explorer(result, report_data_control("rows", max_rows = 4))
  expect_identical(row_mode$profile, summary$profile)
  expect_length(row_mode$rows, 4L)
  expect_true(row_mode$manifest$sampled)
  expect_equal(sum(vapply(row_mode$rows, function(row) row$partition == "evaluation", logical(1))), 1L)
  explicit <- AutoXplainR:::prepare_data_explorer(
    result,
    report_data_control("rows", context_columns = "private_id")
  )
  expect_true("private_id" %in% explicit$manifest$columns)
  expect_true(grepl("EVAL_PRIVATE", jsonlite::toJSON(explicit)))
  none <- AutoXplainR:::prepare_data_explorer(result, "none")
  expect_null(none$profile)
  expect_null(none$rows)
})

test_that("bounded row exports retain exact source/processed positions and preserve the RNG", {
  result <- data_explorer_fixture("drop_rows")
  set.seed(90)
  before <- .Random.seed
  rows <- AutoXplainR:::prepare_data_explorer(result, report_data_control("rows", max_rows = 26))
  expect_identical(.Random.seed, before)
  excluded <- Filter(function(row) row$row_key == "test_data:1", rows$rows)[[1]]
  expect_false(excluded$retained)
  expect_null(excluded$processed$x)
  retained <- Filter(function(row) row$row_key == "test_data:3", rows$rows)[[1]]
  expect_identical(retained$processed_position, 2L)
  expect_equal(retained$raw$x, 10)
  first <- AutoXplainR:::prepare_data_explorer(result, report_data_control("rows", max_rows = 5, seed = 88))
  second <- AutoXplainR:::prepare_data_explorer(result, report_data_control("rows", max_rows = 5, seed = 88))
  expect_identical(first$rows, second$rows)
  expect_identical(.Random.seed, before)
})

test_that("old results, absent data, dates and constants have truthful profile states", {
  result <- data_explorer_fixture()
  result$data_context <- NULL
  export <- AutoXplainR:::prepare_data_explorer(result)
  expect_identical(names(export$profile$stages), "processed")
  expect_identical(export$manifest$raw_status, "unavailable")
  expect_match(export$manifest$scope, "older result", fixed = TRUE)
  expect_error(AutoXplainR:::prepare_data_explorer(
    result,
    report_data_control(context_columns = "private_id")
  ), "unavailable")
  expect_identical(AutoXplainR:::data_axis(rep(NA_real_, 4), rep(NA_real_, 2))$status, "unavailable")
  expect_identical(AutoXplainR:::data_axis(list(1, 2), list(3))$status, "unsupported")
  expect_identical(AutoXplainR:::data_axis(
    as.Date("2026-01-01") + 0:3,
    as.Date("2026-01-05")
  )$kind, "date")
  axis <- AutoXplainR:::data_axis(rep(5, 4), c(5, 6))
  expect_true(all(diff(axis$breaks) > 0))
  expect_equal(sum(AutoXplainR:::data_distribution(c(5, 6), axis, 2L)$counts), 2)
})

test_that("report markup contains active controls and escaped data without script injection", {
  result <- data_explorer_fixture()
  export <- AutoXplainR:::prepare_data_explorer(result)
  html <- AutoXplainR:::explorer_data(result, export)
  expect_match(html, 'id="data"', fixed = TRUE)
  expect_match(html, 'id="relationships"', fixed = TRUE)
  expect_match(html, 'id="data-distribution"', fixed = TRUE)
  expect_match(html, 'data-data-view="distribution"', fixed = TRUE)
  expect_match(html, 'data-data-view="relationships"', fixed = TRUE)
  expect_match(html, 'data-data-view="records"', fixed = TRUE)
  expect_match(html, 'id="axr-data-payload"', fixed = TRUE)
  expect_false(grepl('id="data-filter-form"', html, fixed = TRUE))
  rows <- AutoXplainR:::explorer_data(result, AutoXplainR:::prepare_data_explorer(result, "rows"))
  expect_match(rows, 'id="data-filter-form"', fixed = TRUE)
  payload <- AutoXplainR:::report_json_script(list(name = "</script><script>alert(1)</script>"), "fixture")
  expect_false(grepl("<script>alert", payload, fixed = TRUE))
  expect_match(payload, "\\u003c", fixed = TRUE)
})

test_that("evaluation-only snapshots never invent a training population", {
  data <- data.frame(x = c(2, 4, NA, 8), y = c(3, 5, 8, 10))
  context <- AutoXplainR:::capture_data_context(
    NULL, data, "y", "x", list(data = NULL, row_indices = integer()),
    list(data = data, row_indices = seq_len(nrow(data)))
  )
  result <- list(
    training_data = NULL, test_data = data, target_column = "y", features = "x",
    data_context = context, task = "regression"
  )
  export <- AutoXplainR:::prepare_data_explorer(result, "rows")
  expect_null(context$raw$training)
  expect_true(all(context$row_map$partition == "evaluation"))
  expect_false(export$manifest$training_available)
  expect_null(export$profile$stages$raw$training_rows)
  expect_identical(export$profile$stages$raw$columns$x$training$status, "unavailable")
  expect_equal(export$profile$stages$raw$columns$x$evaluation$n_total, 4L)
  expect_length(export$rows, 4L)
  expect_true(all(vapply(export$rows, function(row) row$partition == "evaluation", logical(1))))
  html <- AutoXplainR:::explorer_data(result, export)
  expect_match(html, "Training data was not supplied", fixed = TRUE)
  expect_false(grepl('<option value="both">', html, fixed = TRUE))
  mapped <- AutoXplainR:::capture_data_context(
    data, data, "y", "x", list(data = data, row_indices = seq_len(nrow(data))),
    list(data = data, row_indices = seq_len(nrow(data))),
    training_source = "training_data", evaluation_source = "data"
  )
  expect_identical(mapped$row_map$row_key, c(paste0("training_data:", 1:4), paste0("data:", 1:4)))
  categories <- paste0("category-", 1:30)
  axis <- AutoXplainR:::data_axis(NULL, categories)
  expect_identical(axis$labels[axis$other_code], "Other evaluation levels")
  expect_identical(axis$labels[axis$novel_code], "Unmapped values")
  expect_equal(sum(AutoXplainR:::data_bin_codes(categories, axis) == axis$other_code), 10L)
})

test_that("binary outcome relationships report independently counted empirical events", {
  x <- c(0, 1, 1, 2, 3, 3, NA)
  y <- factor(c("no", "yes", "no", "yes", "yes", NA, "no"), levels = c("no", "yes"))
  axis_x <- AutoXplainR:::data_axis(c(0, 2, 4), c(0, 2, 4), bins = 2)
  axis_y <- AutoXplainR:::data_axis(y, y)
  pair <- AutoXplainR:::data_pair_profile(x, y, axis_x, axis_y, length(x), positive = "yes")
  expect_equal(pair$conditional_event$x, c(2L, 3L))
  expect_equal(pair$conditional_event$n, c(3L, 2L))
  expect_equal(pair$conditional_event$events, c(1L, 2L))
  expect_equal(pair$conditional_event$rate, c(1 / 3, 1))
  expect_equal(pair$n_excluded, 2L)
})

test_that("conditional summaries preserve occupied bin order and exclude incomplete pairs", {
  axis <- AutoXplainR:::data_axis(0:12, NULL, bins = 12L)
  x <- c(-1, .1, .2, .9, 2, 2, 10, 10, 12, 13, Inf, NA, NaN, 11)
  y <- c(2, 1, 9, 5, 4, 6, 8, NA, 7, 10, 11, 12, 13, Inf)
  numeric_pair <- AutoXplainR:::data_pair_profile(x, y, axis, axis, length(x))
  expect_identical(numeric_pair$conditional, data.frame(
    x = c(1L, 2L, 4L, 12L, 13L, 14L), n = c(1L, 3L, 2L, 1L, 1L, 1L),
    mean = c(2, 5, 5, 8, 7, 10), median = c(2, 5, 5, 8, 7, 10),
    min = c(2, 1, 4, 8, 7, 10), max = c(2, 9, 6, 8, 7, 10)
  ))
  expect_identical(numeric_pair$n_complete, 9L)
  expect_equal(numeric_pair$n_excluded, 5L)
  event <- factor(c("no", "yes", "no", "yes", "no", "yes", "yes", NA,
                    "no", "yes", "no", "no", "no", NA), levels = c("no", "yes"))
  event_axis <- AutoXplainR:::data_axis(event, NULL)
  event_pair <- AutoXplainR:::data_pair_profile(x, event, axis, event_axis, length(x), positive = "yes")
  expect_identical(event_pair$conditional_event, data.frame(
    x = c(1L, 2L, 4L, 12L, 13L, 14L), n = c(1L, 3L, 2L, 1L, 1L, 1L),
    events = c(0L, 2L, 1L, 1L, 0L, 1L), rate = c(0, 2 / 3, .5, 1, 0, 1)
  ))
  empty <- AutoXplainR:::data_pair_profile(c(NA, Inf), c(1, 2), axis, axis, 2L)
  expect_null(empty$conditional)
  expect_equal(nrow(empty$cells), 0L)
  expect_identical(empty$n_complete, 0L)
})

test_that("pair association retains direction and undefined states without category encodings", {
  association <- AutoXplainR:::data_pair_association(1:6, 6:1)
  expect_equal(association$value, -1)
  expect_identical(association$method, "Spearman correlation (signed)")
  expect_equal(association$n, 6)
  constant <- AutoXplainR:::data_pair_association(rep(1, 6), 1:6)
  expect_identical(constant$status, "unavailable")
  expect_true(is.na(constant$value))
  expect_equal(AutoXplainR:::data_pair_association(c(1, 2, Inf), 1:3)$n, 2)
  x <- factor(c("low", "low", "low", "high", "high", "high"))
  y <- factor(c("yes", "yes", "no", "no", "no", "yes"))
  categorical <- AutoXplainR:::data_pair_association(x, y)
  renamed <- AutoXplainR:::data_pair_association(
    factor(ifelse(x == "low", "z", "a"), levels = c("z", "a")),
    factor(ifelse(y == "yes", "b", "x"), levels = c("x", "b"))
  )
  expect_equal(categorical$value, 1 / 3)
  expect_equal(renamed$value, categorical$value)
  expect_match(categorical$method, "unsigned", fixed = TRUE)
  mixed <- AutoXplainR:::data_pair_association(x, c(0, 0, 0, 1, 1, 1))
  expect_equal(mixed$value, 1)
  expect_identical(mixed$method, "Correlation ratio (unsigned)")
})

test_that("exported non-finite values remain distinct from actual missing values", {
  data <- data.frame(x = c(NA, Inf, -Inf, 2), y = c(1, 2, 3, 4))
  context <- AutoXplainR:::capture_data_context(
    NULL, data, "y", "x", list(data = NULL, row_indices = integer()),
    list(data = data, row_indices = seq_len(nrow(data)))
  )
  result <- list(training_data = NULL, test_data = data, target_column = "y", features = "x",
                 data_context = context, task = "regression")
  export <- AutoXplainR:::prepare_data_explorer(result, "rows")
  expect_null(export$rows[[1L]]$raw$x)
  expect_length(export$rows[[1L]]$nonfinite$raw, 0L)
  expect_identical(export$rows[[2L]]$nonfinite$raw, "x")
  expect_identical(export$rows[[3L]]$nonfinite$processed, "x")
  expect_equal(export$profile$stages$raw$columns$x$evaluation$n_missing, 1L)
  expect_equal(export$profile$stages$raw$columns$x$evaluation$n_nonfinite, 2L)
})

test_that("column export caching preserves selected positions, date values and nulls", {
  day <- as.Date("2026-01-01")
  stamp <- as.POSIXct("2026-01-01", tz = "UTC")
  data <- data.frame(
    x = c(1.25, Inf, NA, -Inf), flag = c(TRUE, FALSE, NA, TRUE),
    label = ordered(c("b", NA, "a", "b")), date = day + c(0, NA, 2, 3),
    stamp = stamp + c(0, 60, Inf, 180)
  )
  data$unsupported <- I(as.list(1:4))
  variables <- c(names(data), "absent")
  actual <- AutoXplainR:::data_export_partition(data, c(3L, 1L, NA_integer_, 2L), variables)
  expect_identical(actual$values[[1L]], list(
    x = NULL, flag = NULL, label = "a", date = as.numeric(day) + 2,
    stamp = Inf, unsupported = NULL, absent = NULL
  ))
  expect_identical(actual$values[[2L]], list(
    x = 1.25, flag = "TRUE", label = "b", date = as.numeric(day),
    stamp = as.numeric(stamp), unsupported = NULL, absent = NULL
  ))
  expect_identical(actual$values[[3L]], setNames(rep(list(NULL), length(variables)), variables))
  expect_identical(actual$values[[4L]], list(
    x = NULL, flag = "FALSE", label = NULL, date = NULL,
    stamp = as.numeric(stamp) + 60, unsupported = NULL, absent = NULL
  ))
  expect_identical(actual$nonfinite, list("stamp", character(), character(), "x"))
  expect_identical(AutoXplainR:::data_export_partition(data, integer(), variables),
                   list(values = list(), nonfinite = list()))
})

test_that("bounded pair generation agrees exhaustively with small full combinations", {
  expected <- actual <- list()
  for (n in 0:7) {
    targets <- if (n) seq_len(n) else NA_integer_
    for (target in targets) {
      for (limit in seq.int(0L, choose(n, 2L) + 1L)) {
        key <- paste(n, target, limit, sep = ":")
        full <- if (n >= 2L) utils::combn(seq_len(n), 2L) else matrix(integer(), 2L)
        priority <- if (ncol(full)) colSums(full == target) > 0L else logical()
        value <- as.data.frame(t(full[, order(!priority), drop = FALSE]))
        names(value) <- c("x", "y")
        expected[[key]] <- head(value, max(limit, sum(priority)))
        actual[[key]] <- AutoXplainR:::data_pair_indices(n, target, limit)
      }
    }
  }
  expect_identical(actual, expected)
})

test_that("wide input pair generation allocates only the required bounded output", {
  n <- 5000L
  target <- 2500L
  allocation_path <- tempfile()
  on.exit(unlink(allocation_path), add = TRUE)
  measure <- isTRUE(capabilities("profmem"))
  if (measure) utils::Rprofmem(allocation_path)
  pairs <- tryCatch(AutoXplainR:::data_pair_indices(n, target, 512L), finally = {
    if (measure) utils::Rprofmem(NULL)
  })
  expect_equal(nrow(pairs), n - 1L)
  expect_true(all(pairs$x == target | pairs$y == target))
  expect_identical(pairs[1L, ], data.frame(x = 1L, y = target))
  expect_identical(unname(as.matrix(tail(pairs, 1L))), matrix(c(target, n), nrow = 1L))
  expect_equal(length(unique(paste(pairs$x, pairs$y))), n - 1L)
  if (measure) {
    allocations <- readLines(allocation_path)
    sizes <- as.numeric(sub(" .*", "", allocations[grepl("^[0-9]+", allocations)]))
    # A full 5,000-column combination matrix alone needs about 100 MB.
    expect_lt(max(c(0, sizes)), 1024^2)
  }
  without_target <- AutoXplainR:::data_pair_indices(n, NA_integer_, 512L)
  expect_identical(without_target$x, rep(1L, 512L))
  expect_identical(without_target$y, 2:513)
  empty <- list(training = NULL, evaluation = NULL)
  profile <- AutoXplainR:::build_data_profile(empty, empty, NULL,
    data.frame(name = letters[1:8]), "d", max_pairs = 3L
  )
  expect_identical(profile$pair_coverage$included, 7L)
  expect_equal(profile$pair_coverage$total, 28)
})
