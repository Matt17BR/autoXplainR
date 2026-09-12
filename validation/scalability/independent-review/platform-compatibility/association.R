# Run from the repository root. This tiny probe does not fit models.
output_dir <- path.expand(Sys.getenv("AXR_PLATFORM_REVIEW_OUTPUT",
  "~/.cache/autoxplain-platform-review"))
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
pkgload::load_all('.', quiet = TRUE)
set.seed(945)
x <- rep(paste0('x', seq_len(1200)), each = 3)
y <- sample(rep(paste0('y', seq_len(1100)), length.out = length(x)))
counts <- table(x, y)
expected <- outer(rowSums(counts), colSums(counts)) / sum(counts)
terms <- (counts - expected)^2 / expected
# The explicit recurrence rounds every addition to binary64, including on
# systems where sum() uses a wider C accumulator.
serial_double_sum <- compiler::cmpfun(function(values) {
  total <- 0
  for (value in values) total <- total + value
  total
})
column_n <- colSums(counts)
numerator <- sweep(counts * 1200, 2L, column_n, '-')
scaled_terms <- sweep(numerator^2, 2L, 12 / column_n, '*')
stopifnot(all(rowSums(counts) == 3), setequal(unique(column_n), c(3,4)),
  all(scaled_terms == floor(scaled_terms)), sum(scaled_terms) < 2^53)
scaled_sum <- serial_double_sum(scaled_terms)
statistic <- scaled_sum / 14400
normalizer <- sum(counts) * (min(dim(counts)) - 1)
old_double_statistic <- serial_double_sum(terms)
value <- AutoXplainR:::feature_association(x, y)
transposed <- AutoXplainR:::feature_association(y, x)
reference <- sqrt(statistic / normalizer)
stopifnot(isTRUE(all.equal(value,reference,tolerance=1e-13)),
  isTRUE(all.equal(transposed,reference,tolerance=1e-13)))
output <- list(schema_version='1.0',R=R.version.string,platform=R.version$platform,
  seed=945L,n=length(x),table_dimensions=dim(counts),normalizer=normalizer,
  scaled_sum=scaled_sum,integer_chi_square=statistic,
  exact_reference=reference,sparse=value,sparse_transposed=transposed,
  original_dense_platform=sqrt(sum(terms)/normalizer),
  original_dense_sequential_double=sqrt(old_double_statistic/normalizer),
  original_dense_sequential_double_chi_square=old_double_statistic,
  sequential_double_reference_error=sqrt(old_double_statistic/normalizer)-reference,
  scaled_sequential_double_equals_platform_sum=identical(scaled_sum,sum(scaled_terms)),
  tolerance=1e-13,production_changed=FALSE)
jsonlite::write_json(output,file.path(output_dir, 'association.json'),
  auto_unbox=TRUE,pretty=TRUE,digits=17)
print(output)
