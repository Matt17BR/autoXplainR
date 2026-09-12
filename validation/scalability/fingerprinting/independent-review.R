arguments <- commandArgs(trailingOnly = TRUE)
stopifnot(length(arguments) == 2L)
repo <- normalizePath(arguments[[1L]], mustWork = TRUE)
out <- normalizePath(arguments[[2L]], mustWork = TRUE)
env <- new.env(parent = baseenv())
sys.source(file.path(repo, 'R/evidence_contract.R'), envir = env)
checks <- character()
check <- function(value, name) { stopifnot(isTRUE(value)); checks <<- c(checks, name) }
reference <- new.env(parent = emptyenv())
reference$tail <- c(17L, 19L)
reference$self <- reference
fixtures <- list(
  posixlt = as.POSIXlt(as.Date('2021-01-01') + 0:20, tz = 'UTC'),
  omitted = quote(f(, x, option = , nested = function(a, b, ...) b)),
  missing_pairlist = alist(a = , b = list(1, 2)),
  references = list(reference, reference),
  flat = rep(list(1L), 10000L),
  unicode_rows = data.frame(y = c(1, NA, 3), row.names = c('last', '\u03b1', 'first'))
)
for (name in names(fixtures)) {
  value <- fixtures[[name]]
  old <- paste0('axr-', digest::digest(value, algo = 'sha256', serializeVersion = 2L))
  check(identical(env$content_fingerprint(value), old), paste(name, 'ordinary parity'))
  check(identical(env$fingerprint_from_file(value), old), paste(name, 'file parity'))
}
check(env$fingerprint_needs_file(fixtures$flat), 'flat list takes bounded path')
check(env$fingerprint_needs_file(fixtures$references), 'cyclic references take bounded path')
original <- env$fingerprint_from_file(fixtures$unicode_rows)
changed <- fixtures$unicode_rows
changed$y[3] <- 4
check(!identical(env$fingerprint_from_file(changed), original), 'last row changes hash')
check(!identical(env$fingerprint_from_file(fixtures$unicode_rows[3:1, , drop = FALSE]), original), 'row order changes hash')

state <- new.env(parent = emptyenv())
state$path <- NULL
state$connection <- NULL
owned_file <- function(description, ...) {
  state$path <- description
  check(bitwAnd(as.integer(file.info(dirname(description))$mode), 511L) == 448L, 'private directory mode 0700')
  connection <- base::file(description, ...)
  state$connection <- connection
  connection
}
env$file <- owned_file
sentinel <- tempfile('unrelated-review-file-')
writeLines('retain me', sentinel)
before_connections <- showConnections(all = TRUE)
env$fingerprint_from_file(list(x = 1:3))
check(!dir.exists(dirname(state$path)), 'success removes owned directory')
check(identical(showConnections(all = TRUE), before_connections), 'success closes owned connection')
check(identical(readLines(sentinel), 'retain me'), 'unrelated file retained after success')

failure <- structure(list(message = 'controlled file-open failure', call = NULL, marker = 712L),
  class = c('review_io_error', 'error', 'condition'))
env$file <- function(description, ...) { state$path <- description; stop(failure) }
observed <- tryCatch(env$fingerprint_from_file(list(x = 1:3)), error = identity)
check(identical(observed, failure), 'file-open error identity preserved')
check(!dir.exists(dirname(state$path)), 'open failure removes owned directory')
check(identical(readLines(sentinel), 'retain me'), 'unrelated file retained after open failure')

env$file <- owned_file
env$fingerprint_serialize <- function(value, connection) { writeBin(as.raw(c(1, 2, 3)), connection); stop(failure) }
observed <- tryCatch(env$fingerprint_from_file(list(x = 1:3)), error = identity)
check(identical(observed, failure), 'partial-write error identity preserved')
check(!dir.exists(dirname(state$path)), 'partial-write failure removes owned directory')
check(identical(showConnections(all = TRUE), before_connections), 'partial-write failure closes owned connection')
check(identical(readLines(sentinel), 'retain me'), 'unrelated file retained after partial failure')
unlink(sentinel)
record <- list(passed = TRUE, checks = length(checks), checked = checks,
  source_sha256 = digest::digest(file = file.path(repo, 'R/evidence_contract.R'), algo = 'sha256'),
  r_version = R.version.string, digest_version = as.character(packageVersion('digest')))
jsonlite::write_json(record, file.path(out, 'independent-review.json'), pretty = TRUE, auto_unbox = TRUE)
cat(length(checks), 'independent bounded checks passed.\n')
