# Run from the repository root. Optional arguments: baseline|candidate, rows.
args <- commandArgs(trailingOnly = TRUE)
variant <- if (length(args)) args[1L] else 'candidate'
n <- if (length(args) > 1L) as.integer(args[2L]) else 1000000L
output <- Sys.getenv('AXR_SCALE_OUTPUT', path.expand('~/.cache/autoxplain-scale-0.7.0/overlap'))
library_path <- Sys.getenv('AXR_BASELINE_LIBRARY', path.expand('~/.cache/autoxplain-stress-0.6.2/published-release/library'))
dir.create(output, recursive = TRUE, showWarnings = FALSE)
namespace <- loadNamespace('AutoXplainR', lib.loc = library_path)
stopifnot(as.character(getNamespaceVersion(namespace)) == '0.6.2', variant %in% c('baseline','candidate'))
implementation <- if (variant == 'baseline') namespace else new.env(parent = namespace)
source_file <- 'R/guided_workflow.R'
if (variant == 'candidate') sys.source(source_file, implementation)
functions <- c('check_evaluation_row_overlap', 'split_row_keys', 'normalize_split_column')
if (variant == 'candidate') functions <- c(functions, 'split_row_overlap')
source_hash <- digest::digest(lapply(functions, function(name) body(get(name, implementation))))
source('validation/scalability/million/fixtures.R')
training <- scale_fixture('regression', n)$data
evaluation <- scale_fixture('regression', 20000L, evaluation=TRUE)$data
# With finite ordinary numeric x1 values, no shared x1 independently proves no
# complete row can match. Injected matches then exercise every remaining column.
stopifnot(!any(evaluation$x1 %in% training$x1))
scenarios <- list(disjoint=evaluation)
positions <- c(1L, 5000L, 10000L, 15000L, 20000L)
source_rows <- as.integer(c(1L, n, floor(n/2), 1L, 7L))
evaluation[positions,] <- training[source_rows,]
evaluation$x1[9999L] <- training$x1[20L]
scenarios$injected <- evaluation
results <- lapply(names(scenarios), function(name) {
  evaluation <- scenarios[[name]]
  expected <- if (name == 'injected') positions else integer()
  warning <- NULL
  gc()
  elapsed <- system.time(withCallingHandlers(
    implementation$check_evaluation_row_overlap(training, evaluation),
    warning=function(condition) { warning <<- conditionMessage(condition); invokeRestart('muffleWarning') }
  ))['elapsed']
  if (length(expected)) {
    stopifnot(grepl(paste('Found', length(expected), 'rows'), warning, fixed=TRUE),
      grepl(paste(expected, collapse=', '), warning, fixed=TRUE))
  } else stopifnot(is.null(warning))
  if (variant == 'candidate') stopifnot(identical(implementation$split_row_overlap(training,evaluation), expected))
  list(scenario=name, elapsed_seconds=unname(elapsed), matching_evaluation_rows=expected,
       expected_matches_verified=TRUE)
})
stopifnot(identical(source_hash, digest::digest(lapply(functions, function(name) body(get(name, implementation))))))
record <- list(variant=variant, training_rows=n, evaluation_rows=20000L, columns=ncol(training),
               baseline_version='0.6.2', source_function_hash=source_hash,
               measurements=results, r_version=R.version.string)
jsonlite::write_json(record, file.path(output,paste0(variant,'-',n,'.json')),auto_unbox=TRUE,pretty=TRUE,digits=16)
cat(jsonlite::toJSON(record,auto_unbox=TRUE,pretty=TRUE), '\n')
