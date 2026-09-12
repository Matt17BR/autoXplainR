fingerprint_streaming_fixtures <- function() {
  shared <- new.env(parent = emptyenv())
  shared$values <- c(1, 3, 5)
  shared$self <- shared
  captured <- new.env(parent = baseenv())
  captured$offset <- 3
  closure <- eval(quote(function(x, shift = 1, ...) x + shift + offset), envir = captured)
  latin <- iconv("caf\u00e9", from = "UTF-8", to = "latin1")
  bytes <- rawToChar(as.raw(c(0xff, 0xfe)))
  Encoding(bytes) <- "bytes"
  list(
    null = NULL, logical = c(TRUE, FALSE, NA), integer = c(0L, 1L, NA_integer_),
    real = c(-0, 0, NA_real_, NaN, Inf, -Inf, 1e-300, 1e300),
    complex = c(1 + 2i, NA_complex_), raw = as.raw(c(0, 1, 127, 255)),
    text = c("", "caf\u00e9", latin, bytes, NA_character_),
    factor = ordered(c("b", "a", NA), levels = c("unused", "a", "b")),
    frame = data.frame(x = c(1, NA, 3), y = factor(c("a", "b", "a")), row.names = c("9", "2", "7")),
    matrix = matrix(1:6, 2L, dimnames = list(c("r1", "r2"), c("a", "b", "c"))),
    date = as.Date(c("2020-01-01", NA)),
    instant = as.POSIXct(c("2020-01-01", "2020-06-01"), tz = "UTC"),
    calendar = as.POSIXlt(as.Date("2020-01-01") + 0:20, tz = "UTC"),
    call = quote(f(x, , option = TRUE)),
    formals = formals(function(x, optional = 1, ...) NULL), missing = alist(x = , y = ),
    expression = expression(x + y, function(z) z),
    shared_reference = list(first = shared, second = shared),
    closure = closure, attributes = structure(1:3, class = "example", note = list(scale = 2)),
    nested_missing = list(list(quote(expr = )))
  )
}

test_that("file and ordinary fingerprints preserve the complete version-2 digest contract", {
  for (value in fingerprint_streaming_fixtures()) {
    expected <- paste0("axr-", digest::digest(value, algo = "sha256", serializeVersion = 2L))
    bytes <- serialize(value, NULL, ascii = FALSE, xdr = TRUE, version = 2L)
    expect_identical(rawToChar(bytes[1:2]), "X\n")
    expect_identical(readBin(bytes[3:6], integer(), n = 1L, endian = "big"), 2L)
    # Independently strip the documented header from an ordinary raw serialization.
    stripped <- paste0("axr-", digest::digest(bytes[-seq_len(14L)], algo = "sha256", serialize = FALSE))
    expect_identical(stripped, expected)
    expect_identical(AutoXplainR:::fingerprint_from_file(value), expected)
    expect_identical(AutoXplainR:::content_fingerprint(value), expected)
  }
})

test_that("streamed identities detect ordered data, final values, metadata and reference changes", {
  original <- data.frame(x = c(1, 2, 3), group = factor(c("a", "b", "a")))
  modifications <- list(
    within(original, x[3] <- 4), original[3:1, ],
    structure(original, note = "changed"),
    structure(original, class = c("custom_frame", "data.frame")),
    transform(original, group = factor(group, levels = c("b", "a")))
  )
  expected <- AutoXplainR:::fingerprint_from_file(original)
  for (value in modifications) {
    observed <- AutoXplainR:::fingerprint_from_file(value)
    expect_false(identical(observed, expected))
    expect_identical(observed, paste0("axr-", digest::digest(value, algo = "sha256", serializeVersion = 2L)))
  }
  shared <- new.env(parent = emptyenv())
  shared$value <- 2
  alias <- list(shared, shared)
  key <- AutoXplainR:::fingerprint_from_file(alias)
  independent <- list2env(list(value = 2), parent = emptyenv())
  expect_false(identical(key, AutoXplainR:::fingerprint_from_file(list(shared, independent))))
  shared$value <- 3
  expect_false(identical(key, AutoXplainR:::fingerprint_from_file(alias)))
  captured <- list2env(list(offset = 1), parent = baseenv())
  fun <- eval(quote(function(x) x + offset), envir = captured)
  key <- AutoXplainR:::fingerprint_from_file(fun)
  captured$offset <- 2
  expect_false(identical(key, AutoXplainR:::fingerprint_from_file(fun)))
})

test_that("the planner bounds structure, handles missing arguments and never enters cyclic state", {
  expect_false(AutoXplainR:::fingerprint_needs_file(list(x = 1:10, y = letters)))
  expect_false(AutoXplainR:::fingerprint_needs_file(formals(function(x, y = 1, ...) NULL)))
  expect_false(AutoXplainR:::fingerprint_needs_file(quote(f(x, , y))))
  expect_false(AutoXplainR:::fingerprint_needs_file(alist(x = , y = )))
  expect_true(AutoXplainR:::fingerprint_needs_file(vector("list", 3000L)))
  expect_true(AutoXplainR:::fingerprint_needs_file(raw(64L * 1024L^2L)))
  cyclic <- new.env(parent = emptyenv())
  cyclic$self <- cyclic
  local_mocked_bindings(object.size = function(...) stop("must not inspect reference contents"), .package = "utils")
  expect_true(AutoXplainR:::fingerprint_needs_file(list(cyclic, cyclic)))
  expect_true(AutoXplainR:::fingerprint_needs_file(function(x) x))
})

test_that("fingerprinting preserves RNG and removes temporary directories on success", {
  directories <- function() list.dirs(tempdir(), recursive = FALSE, full.names = TRUE)
  before <- directories()
  set.seed(638L)
  rng <- .Random.seed
  value <- list(x = 1:20, formals = formals(function(x, ...) NULL))
  AutoXplainR:::fingerprint_from_file(value)
  expect_identical(.Random.seed, rng)
  expect_identical(directories(), before)
})

test_that("planning never invokes class-defined structural access methods", {
  state <- new.env(parent = emptyenv())
  state$calls <- 0L
  forbidden <- function(...) {
    state$calls <- state$calls + 1L
    stop("must not dispatch")
  }
  registry <- get(".__S3MethodsTable__.", envir = baseenv())
  registerS3method("length", "fingerprint_custom", forbidden, envir = baseenv())
  registerS3method("[[", "fingerprint_custom", forbidden, envir = baseenv())
  withr::defer(rm(list = c("length.fingerprint_custom", "[[.fingerprint_custom"), envir = registry))
  values <- list(
    structure(list(first = 1:3, second = letters), class = "fingerprint_custom"),
    structure(quote(f(x, , y)), class = "fingerprint_custom"),
    structure(expression(x, y), class = "fingerprint_custom")
  )
  for (value in values) {
    expected <- paste0("axr-", digest::digest(value, algo = "sha256", serializeVersion = 2L))
    expect_false(AutoXplainR:::fingerprint_needs_file(value))
    expect_identical(AutoXplainR:::content_fingerprint(value), expected)
    expect_identical(AutoXplainR:::fingerprint_from_file(value), expected)
  }
  expect_identical(state$calls, 0L)
})

test_that("serialization failures close the connection, remove bytes and preserve the original error", {
  state <- new.env(parent = emptyenv())
  state$connection <- NULL
  failure <- structure(list(message = "deliberate serialization failure", call = NULL),
                       class = c("fingerprint_test_error", "error", "condition"))
  before <- list.dirs(tempdir(), recursive = FALSE, full.names = TRUE)
  connections <- showConnections(all = TRUE)
  local_mocked_bindings(fingerprint_serialize = function(value, connection) {
    state$connection <- connection
    writeBin(as.raw(c(1, 2, 3)), connection)
    stop(failure)
  }, .package = "AutoXplainR")
  error <- tryCatch(AutoXplainR:::fingerprint_from_file(1), error = identity)
  expect_identical(error, failure)
  expect_error(isOpen(state$connection), "invalid connection")
  expect_identical(showConnections(all = TRUE), connections)
  expect_identical(list.dirs(tempdir(), recursive = FALSE, full.names = TRUE), before)
})

test_that("hash failures occur after close and still remove the serialization", {
  failure <- structure(list(message = "deliberate hash failure", call = NULL),
                       class = c("fingerprint_test_error", "error", "condition"))
  before <- list.dirs(tempdir(), recursive = FALSE, full.names = TRUE)
  connections <- showConnections(all = TRUE)
  local_mocked_bindings(digest = function(file, algo, serialize, skip) {
    expect_true(file.exists(file))
    expect_gt(file.info(file)$size, 14)
    expect_false(file %in% showConnections(all = TRUE)[, "description"])
    expect_identical(serialize, FALSE)
    expect_identical(skip, 14L)
    stop(failure)
  }, .package = "digest")
  error <- tryCatch(AutoXplainR:::fingerprint_from_file(list(a = 1)), error = identity)
  expect_identical(error, failure)
  expect_identical(showConnections(all = TRUE), connections)
  expect_identical(list.dirs(tempdir(), recursive = FALSE, full.names = TRUE), before)
})

test_that("only allocation errors retry through the bounded path", {
  state <- new.env(parent = emptyenv())
  state$writes <- 0L
  local_mocked_bindings(fingerprint_from_file = function(value) {
    state$writes <- state$writes + 1L
    "streamed"
  }, .package = "AutoXplainR")
  failure <- structure(list(message = "cannot allocate buffer", call = NULL), class = c("error", "condition"))
  local_mocked_bindings(digest = function(...) stop(failure), .package = "digest")
  expect_identical(AutoXplainR:::content_fingerprint(1), "streamed")
  expect_identical(state$writes, 1L)
  failure$message <- "unrelated hash failure"
  expect_identical(tryCatch(AutoXplainR:::content_fingerprint(1), error = identity), failure)
  expect_identical(state$writes, 1L)
})
