test_that("one call retains evidence and writes a report without recomputation", {
  path <- tempfile(fileext = ".html")
  result <- autoxplain(mtcars, "mpg", report = path)
  expect_s3_class(result$explanations$audit, "autoxplain_audit")
  expect_true(length(result$explanations$effects) > 0L)
  expect_identical(result$schema_version, "1.0")
  expect_true(file.exists(result$report_file))
  local_mocked_bindings(prepare_model_report_data = function(...) stop("recomputed"))
  second <- tempfile(fileext = ".html")
  expect_identical(render_model_report(result, second), normalizePath(second))
  expect_identical(readLines(path), readLines(second))
  expect_error(render_model_report(result, second, n_repeats = 2), "recomputed")
})

test_that("fitting only and input errors are explicit", {
  expect_null(autoxplain(mtcars, "mpg", explain = FALSE)$explanations)
  expect_error(autoxplain(mtcars, "mpg", explain = NA), "explain")
  expect_error(autoxplain(mtcars, "mpg", enable_preprocessing = 1), "enable_preprocessing")
  expect_error(autoxplain(mtcars, "mpg", report = "bad.pdf"), "html")
})

test_that("raw predictions use the saved recipe and survive serialization", {
  train <- data.frame(x = seq_len(30), category = rep(c("a", "b"), 15), y = sin(seq_len(30)))
  result <- autoxplain(train, "y", explain = FALSE)
  new <- data.frame(x = c(NA, 4, 10), category = c("a", "new", "b"))
  baked <- new
  baked$x[1] <- median(result$training_data$x)
  baked$category[2] <- result$preprocessing_metadata$training_data$recipe$factor_fallbacks$category
  expect_equal(predict(result, new), as.numeric(predict(result$models$main_model, baked)))
  file <- tempfile()
  saveRDS(result, file)
  expect_identical(predict(readRDS(file), new), predict(result, new))
  expect_equal(length(predict(result, new[FALSE, ])), 0L)
  expect_error(predict(result, new["x"]), "missing model features")
  expect_error(predict(result, new, model = c(1, 2)), "exactly one")
  expect_error(predict(result, new, type = "class"), "classification")
  expect_error(predict(result, new, unused = TRUE), "Unused")
})

test_that("drop-row prediction keeps input positions and ignores outcomes", {
  result <- autoxplain(mtcars, "mpg", explain = FALSE,
                       preprocessing_config = list(missing_value_strategy = "drop_rows"))
  new <- mtcars[1:3, ]
  new$mpg <- NA_real_
  new$wt[2] <- NA_real_
  predictions <- predict(result, new)
  expect_length(predictions, 3L)
  expect_true(is.na(predictions[2]))
  expect_true(all(is.finite(predictions[c(1, 3)])))
  expect_equal(predictions[c(1, 3)], predict(result, mtcars[c(1, 3), ]))
})

test_that("classification prediction preserves levels including a one-row matrix", {
  result <- autoxplain(iris, "Species", explain = FALSE)
  probability <- predict(result, iris[1, ])
  expect_equal(dim(probability), c(1L, 3L))
  expect_equal(colnames(probability), levels(iris$Species))
  expect_equal(sum(probability), 1)
  expect_equal(as.character(predict(result, iris[1, ], type = "class")), "setosa")
  expect_equal(dim(predict(result, iris[FALSE, ])), c(0L, 3L))
  binary <- transform(mtcars, am = factor(am, labels = c("no", "yes")))
  result <- autoxplain(binary, "am", explain = FALSE)
  expect_equal(predict(result, binary), as.numeric(predict(result$models$main_model, binary, type = "response")))
  expect_identical(levels(predict(result, binary, type = "class")), c("no", "yes"))
})

test_that("evaluation may omit trained classes without inventing metrics", {
  binary <- transform(iris, y = factor(ifelse(Species == "setosa", "yes", "no")))
  binary$Species <- NULL
  result <- autoxplain(binary, "y", test_data = binary[1:10, ], overlap_action = "ignore")
  expect_true(is.na(result$evaluation$metrics$main_model[["roc_auc"]]))
  expect_true(is.na(result$evaluation$metrics$main_model[["balanced_accuracy"]]))
  expect_true(is.finite(result$evaluation$metrics$main_model[["log_loss"]]))
  multi <- autoxplain(iris, "Species", test_data = iris[1:10, ], overlap_action = "ignore")
  expect_true(is.na(multi$evaluation$metrics$main_model[["macro_recall"]]))
  expect_s3_class(multi$explanations$audit, "autoxplain_audit")
})

test_that("failed effects are retained and escaped in reports", {
  result <- autoxplain(mtcars, "mpg", explain = FALSE)
  local_mocked_bindings(explain_effect = function(...) stop("<script>bad</script>"))
  prepared <- AutoXplainR:::prepare_model_report_data(result, n_repeats = 2)
  expect_equal(nrow(prepared$failures), 3L)
  expect_length(prepared$effects, 0L)
  result$explanations <- prepared
  path <- tempfile(fileext = ".html")
  render_model_report(result, path)
  html <- paste(readLines(path), collapse = "\n")
  expect_match(html, "Effects that could not be estimated")
  expect_match(html, "&lt;script&gt;")
  expect_false(grepl("<script>bad", html, fixed = TRUE))
})

test_that("entirely missing evaluation columns use training values", {
  train <- data.frame(x = 1:30, category = factor(rep(c("a", "b"), 15)), y = sin(1:30))
  test <- data.frame(x = c(NA, NA, NA), category = factor(c(NA, "b", "b")), y = 1:3)
  result <- autoxplain(train, "y", test_data = test, explain = FALSE)
  expect_equal(result$test_data$x, rep(median(train$x), 3))
  expect_equal(as.character(result$test_data$category[1]), "a")
  expect_true(all(is.finite(predict(result, test))))
  test$x <- c("1", "2", "3")
  expect_error(predict(result, test), "require numeric")
})

test_that("uncertainty report records its sampling assumptions", {
  result <- autoxplain(mtcars, "mpg")
  path <- tempfile(fileext = ".html")
  render_model_report(result, path, uncertainty = TRUE)
  html <- paste(readLines(path), collapse = "\n")
  expect_match(html, "How variable is this score")
  expect_match(html, "conditional on the fitted models")
  expect_match(html, "@media print", fixed = TRUE)
})
