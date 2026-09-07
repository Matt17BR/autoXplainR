# Run with Rscript; common.R selects an installed package and an explicit output directory.
script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1L]])
source(file.path(dirname(normalizePath(script)), "common.R"))
library(mgcv)
library(ranger)
library(xgboost)
source <- readRDS(file.path(output,'source-and-result.rds'))
result <- source$result
observed <- jsonlite::read_json(file.path(output,'detail-observations-before-oracles.json'))
row_map <- result$data_context$row_map
rows <- row_map[row_map$partition=='evaluation' & row_map$retained, ]
rows <- rows[order(rows$processed_position), ]
raw_evaluation <- source$original[rows$source_row, ]
stopifnot(nrow(raw_evaluation)==72L,
  isTRUE(all.equal(unname(as.matrix(raw_evaluation)), unname(as.matrix(result$test_data)),check.attributes=FALSE)))
native <- function(id,new_data) {
  model <- result$models[[id]]
  inputs <- new_data[result$features]
  if (id=='main_model') {
    names(inputs) <- unname(model$fit_details$feature_map[names(inputs)])
    return(as.numeric(predict(model$fit,newdata=inputs,type='response')))
  }
  if(id=='boosting_model') {
    matrix <- model.matrix(~distance_km+load_tonnes+depot_queue+service,inputs)[,-1,drop=FALSE]
    stopifnot(identical(colnames(matrix),model$blueprint$columns))
    return(as.numeric(predict(model$fit,newdata=matrix)))
  }
  as.numeric(predict(model$fit,data=inputs)$predictions)
}
answers <- list()
for(id in names(observed$predictions)) {
  predicted <- native(id,raw_evaluation)
  residual <- raw_evaluation$delay_hours - predicted
  metrics <- c(rmse=sqrt(mean(residual^2)),mae=mean(abs(residual)),bias=mean(residual))
  key <- observed$predictions[[id]]$source_key
  position <- match(key,rows$row_key)
  stopifnot(which.max(abs(residual)) == position)
  first_case <- as.numeric(strsplit(observed$predictions[[id]]$first_case,'\t')[[1]][-1])
  exact_case <- c(raw_evaluation$delay_hours[position],predicted[position],residual[position])
  stopifnot(max(abs(first_case-signif(exact_case,4)))<1e-10)
  new_data <- raw_evaluation[1:5, ]
  copied <- eval(parse(text=observed$predictions[[id]]$r_code))
  expected <- native(id,new_data)
  stopifnot(max(abs(copied-expected))<1e-12)
  answers[[id]] <- list(metrics=metrics, source_key=key,source_position=rows$source_row[position],
    source_values=source$original[rows$source_row[position],],case=exact_case,
    copied_r_code=observed$predictions[[id]]$r_code,copied_max_error=max(abs(copied-expected)))
}
# Reconstruct candidate RMSE and its delta-method fold SE directly from OOF rows.
cv <- lapply(c('additive_01','additive_02'),function(id){
  rows <- result$tuning$out_of_fold_predictions
  rows <- rows[rows$configuration_id==id,]
  squared <- (rows$truth-rows$estimate)^2
  fold_mse <- tapply(squared,rows$fold,mean)
  score <- sqrt(mean(squared))
  se <- sd(fold_mse)/sqrt(length(fold_mse))/(2*score)
  data.frame(configuration=id,cv_rmse=score,se=se,fold_rmse=paste(format(sqrt(fold_mse),digits=12),collapse=', '))
})
cv <- do.call(rbind,cv)
stopifnot(max(abs(cv$cv_rmse-result$tuning$candidates$cv_score[match(cv$configuration,result$tuning$candidates$configuration_id)]))<1e-12)
threshold <- cv$cv_rmse[2]+cv$se[2]
stopifnot(abs(threshold-result$tuning$selection$threshold)<1e-12,cv$cv_rmse[1]<=threshold)
# GAM is additive: differences along its reported load grid must equal direct
# native predictions at those coordinates, independent of ALE centering.
curve <- read.csv(file.path(output,'observed-gam-curve.csv'))
new_data <- raw_evaluation[rep(1L,nrow(curve)), ]
new_data$load_tonnes <- curve$load_tonnes
prediction <- native('main_model',new_data)
curve_error <- max(abs((curve$effect-curve$effect[1])-(prediction-prediction[1])))
stopifnot(curve_error < 1e-5)
answers$gam_curve <- list(max_difference_error=curve_error,peak_load=curve$load_tonnes[which.max(curve$effect)],
  trough_load=curve$load_tonnes[which.min(curve$effect)],interpretation='A rise, fall and partial recovery; no monotone trend was inferred.')
answers$selection <- list(cv=cv,threshold=threshold,selected=result$tuning$selected_configuration,final=result$tuning$final_configuration)
answers$native_structure <- list(gam_coefficients=length(coef(result$models$main_model$fit)),
  gam_basis=sapply(result$models$main_model$fit$smooth,function(s)s$bs.dim),
  gam_total_edf=sum(result$models$main_model$fit$edf),
  boosting_rounds=xgb.get.num.boosted.rounds(result$models$boosting_model$fit),
  forest_trees=result$models$forest_model$fit$num.trees,forest_mtry=result$models$forest_model$fit$mtry,
  forest_node_size=result$models$forest_model$fit$min.node.size)
jsonlite::write_json(answers,file.path(output,'independent-native-answers.json'),pretty=TRUE,auto_unbox=TRUE,digits=15)
print(answers)
