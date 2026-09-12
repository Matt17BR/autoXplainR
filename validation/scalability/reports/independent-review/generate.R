pkgload::load_all(quiet=TRUE)
ns <- asNamespace('AutoXplainR')
output <- Sys.getenv('AXR_COMPACT_REVIEW_OUTPUT', path.expand('~/.cache/autoxplain-scale-0.7.0/independent-review/compact'))
dir.create(output,recursive=TRUE,showWarnings=FALSE)
frame <- function(n,offset=0L) {
 i <- seq_len(n)+offset
 data.frame(y=i/3, x=I(rep(c(pi,NA,Inf,-Inf,1+.Machine$double.eps),length.out=n)),
    category=rep(c('a','</script> & \u2028 \u2029 \u4f60\u597d',NA,'new'),length.out=n),
    id=paste0('person-',i), flag=rep(c(TRUE,FALSE,NA),length.out=n),
    date=as.Date('2026-01-01')+i,
    time=as.POSIXct('2026-01-01',tz='UTC')+i/4,
    check.names=FALSE)
}
train <- frame(35)
evaluation <- frame(9,30)
train$category <- factor(train$category,levels=c('a','</script> & \u2028 \u2029 \u4f60\u597d','new','unused'))
tr <- list(data=train[-3,],row_indices=setdiff(seq_len(nrow(train)),3L))
te <- list(data=evaluation[-2,],row_indices=setdiff(seq_len(nrow(evaluation)),2L))
tr$data$x[is.infinite(tr$data$x)] <- NA_real_
te$data$x[is.infinite(te$data$x)] <- NA_real_
features <- setdiff(names(train),'y')
result <- list(training_data=tr$data,test_data=te$data,target_column='y',features=features,task='regression',provenance=list(seed=4))
result$data_context <- ns$capture_data_context(train,evaluation,'y',features,tr,te)
read_review_json_text <- function(html, id) {
  marker <- paste0('<script type="application/json" id="', id, '"')
  start <- regexpr(marker, html, fixed = TRUE)
  stopifnot(start > 0L)
  tail <- substring(html, start + attr(start, "match.length"))
  header_end <- regexpr(">", tail, fixed = TRUE)
  stopifnot(header_end > 0L)
  header <- substring(tail, 1L, header_end - 1L)
  body <- substring(tail, header_end + 1L)
  end <- regexpr("</script>", body, fixed = TRUE)
  stopifnot(end > 0L)
  json <- substring(body, 1L, end - 1L)
  if (grepl('data-json-chunks="', header, fixed = TRUE)) {
    count <- as.integer(sub('.*data-json-chunks="([0-9]+)".*', "\\1", header))
    stopifnot(is.finite(count), count > 0L, identical(json, ""))
    prefix <- paste0('<script type="application/octet-stream" data-json-owner="', id, '" data-json-chunk="')
    positions <- gregexpr(prefix, html, fixed = TRUE)[[1L]]
    stopifnot(length(positions) == count, all(positions > start))
    pieces <- vapply(seq_len(count), function(index) {
      tag <- paste0(prefix, index, '">')
      position <- regexpr(tag, html, fixed = TRUE)
      stopifnot(position == positions[[index]])
      remaining <- substring(html, position + attr(position, "match.length"))
      close <- regexpr("</script>", remaining, fixed = TRUE)
      stopifnot(close > 0L)
      substring(remaining, 1L, close - 1L)
    }, character(1))
    json <- paste0(pieces, collapse = "")
  }
  json
}

cases <- list(context=result)
no_context <- result; no_context$data_context <- NULL; cases$no_context <- no_context
no_training <- no_context; no_training$training_data <- NULL; cases$no_training <- no_training
empty_training <- no_context; empty_training$training_data <- train[FALSE,]; cases$empty_training <- empty_training
for(case in names(cases)) for(limit in c(4L,100L)) {
 control <- report_data_control('rows',max_rows=limit,max_pair_rows=10,seed=4)
 records <- ns$prepare_data_explorer(cases[[case]],control)
 columns <- ns$prepare_data_explorer(cases[[case]],control,row_layout='columns')
 stopifnot(identical(records$manifest,columns$manifest),identical(records$profile,columns$profile))
 values <- list(legacy=ns$report_data_payload(records),records=ns$report_data_payload(records,compact=TRUE),columns=ns$report_data_payload(columns,compact=TRUE))
 for(name in names(values)) {
  script <- ns$report_json_script(values[[name]],'fixture')
  body <- read_review_json_text(script, 'fixture')
  writeLines(body,file.path(output,paste(case,limit,paste0(name,'.json'),sep='-')),useBytes=TRUE)
 }
}
writeLines(read_review_json_text(ns$report_json_script(ns$report_payload_block(rep('</script> \u4f60\u597d',4000),vector=TRUE),'fixture'), 'fixture'),file.path(output,'compressed.json'),useBytes=TRUE)
cat('Generated 8 public preparation scenarios with record, direct-column, and legacy wire paths.\n')
