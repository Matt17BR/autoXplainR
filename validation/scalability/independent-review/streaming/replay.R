args <- commandArgs(TRUE)
if(args[1]=='source')pkgload::load_all(quiet=TRUE) else library(AutoXplainR,lib.loc=args[1])
ns <- asNamespace('AutoXplainR')
original <- ns$fit_tuning_configuration
fit <- function(configuration,data,target,task,...) {
 if(configuration$configuration_id=='tree_01' && !'case-17'%in%rownames(data))stop('deliberate single-fold failure')
 original(configuration,data,target,task,...)
}
run <- function() {
 set.seed(441)
 data <- data.frame(x=rnorm(48),z=rnorm(48)); data$y <- sin(data$x)+data$z+rnorm(48,.1)
 rownames(data)<-paste0('case-',1:48)
 removed<-c(2L,5L,14L,18L,35L,37L);data$x[removed]<-NA_real_
 evaluation<-data.frame(x=c(.222,.444,.666,.888),z=c(.19,.71,.35,.93),y=c(1,2,3,4))
 folds<-rep(c('a','b','c'),c(12,16,20))
 output<-list()
 for(task in c('regression','binary','multiclass')) for(retain in c(TRUE,FALSE)) {
  train<-data;test<-evaluation
  if(task!='regression') {
   classes<-if(task=='binary')c('yes','no') else c('z','a','m')
   train$y<-factor(rep(classes,length.out=48),levels=classes)
   test$y<-factor(rep(classes,length.out=4),levels=classes)
  }
  result<-autoxplain(train,'y',test_data=test,task=task,learners=c('linear','tree'),max_models=3,
   preprocessing_config=list(missing_value_strategy='drop_rows'),explain=FALSE,seed=71,
   tuning_control=tuning_control(fold_ids=folds,retain_oof=retain,metric=if(task=='regression')'rmse' else 'log_loss'))
  tuning<-result$tuning
  stopifnot(identical(tuning$omitted_rows$training_row,removed))
  failed<-tuning$candidates$configuration_id[tuning$candidates$status=='failed']
  stopifnot(identical(failed,'tree_01'))
  pool_checks<-list()
  if(retain) {
   oof<-tuning$out_of_fold_predictions
   stopifnot(!any(oof$configuration_id%in%failed))
   for(id in unique(oof$configuration_id)) {
    rows<-oof[oof$configuration_id==id,]
    stopifnot(setequal(rows$training_row,setdiff(1:48,removed)),identical(rows$source_row,paste0('case-',rows$training_row)))
    stopifnot(all(rows$fold==match(folds[rows$training_row],unique(folds))))
    pooled<-if(task=='regression')sqrt(mean((train$y[rows$training_row]-rows$estimate)^2)) else {
     p<-rows$probabilities
     stopifnot(identical(colnames(p),classes))
     -mean(log(pmax(p[cbind(seq_len(nrow(p)),match(as.character(train$y[rows$training_row]),classes))],1e-15)))
    }
    pool_checks[[id]]<-c(direct=pooled,cv=tuning$candidates$cv_score[tuning$candidates$configuration_id==id],recorded=mean(rows$case_loss))
    cat('POOLED',task,id,format(pooled,digits=17),'CV',format(tuning$candidates$cv_score[tuning$candidates$configuration_id==id],digits=17),'DIFF',format(pooled-tuning$candidates$cv_score[tuning$candidates$configuration_id==id],digits=17),'\n')
   }
  } else stopifnot(is.null(tuning$out_of_fold_predictions))
  fields<-c('configuration_id','fold','score','validation_rows','validation_rows_requested','validation_rows_omitted','error')
  candidate_fields<-c('configuration_id','cv_score','cv_se','folds_completed','evaluated_rows','selected','status')
  output[[paste(task,retain,sep='/')]]<-list(pool_checks=pool_checks,selected=tuning$selected_configuration,folds=tuning$fold_scores[fields],candidates=tuning$candidates[candidate_fields],omissions=tuning$omitted_rows,oof=tuning$out_of_fold_predictions,predictions=predict(result,test))
 }
 output
}
output<-testthat::with_mocked_bindings(run(),fit_tuning_configuration=fit,.package='AutoXplainR')
saveRDS(output,args[2])
cat('Partial-fit failure, original row identity and omission checks passed; pooled-score differences are recorded, not assumed to pass.\n')
