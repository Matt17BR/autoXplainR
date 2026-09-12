pkgload::load_all(quiet=TRUE)
data<-data.frame(x=1:48,y=factor(rep(c('negative','positive'),24)))
evaluation<-data.frame(x=c(.2,.4,.6,.8),y=factor(c('negative','positive','negative','positive'),levels=levels(data$y)))
result<-autoxplain(data,'y',test_data=evaluation,learners='tree',explain=FALSE,seed=18,
 tuning_control=tuning_control(fold_ids=rep(1:3,each=16),grids=list(tree=list(cp=1,minsplit=20L,maxdepth=1L)),family_budgets=c(tree=1L)))
oof<-result$tuning$out_of_fold_predictions
stopifnot(all(oof$probabilities[,'positive']==.5))
cat('OOF_CLASSES=',paste(unique(oof$predicted_class),collapse=','),'PUBLIC_CLASSES=',paste(unique(predict(result,evaluation,type='class')),collapse=','),'\n')
