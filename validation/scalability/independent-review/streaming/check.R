args <- commandArgs(TRUE)
stopifnot(length(args)==3L)
before<-readRDS(args[1]);after<-readRDS(args[2])
changes<-list()
for(name in names(before)){
 a<-before[[name]];b<-after[[name]]
 fields<-setdiff(names(a),c('pool_checks','oof'))
 stopifnot(identical(a[fields],b[fields]))
 if(!is.null(a$oof)) {
  other<-setdiff(names(a$oof),'case_loss')
  stopifnot(identical(a$oof[other],b$oof[other]))
  if(!startsWith(name,'binary/'))stopifnot(identical(a$oof,b$oof))
  for(id in unique(b$oof$configuration_id)){
   row<-b$oof[b$oof$configuration_id==id,]
   pooled<-if(startsWith(name,'regression/'))sqrt(mean(row$case_loss)) else mean(row$case_loss)
   score<-b$candidates$cv_score[b$candidates$configuration_id==id]
   stopifnot(isTRUE(all.equal(pooled,score,tolerance=1e-14)))
  }
  changed<-which(a$oof$case_loss!=b$oof$case_loss)
  if(length(changed))changes[[name]]<-list(field='case_loss',rows=length(changed),maximum_absolute_change=max(abs(a$oof$case_loss-b$oof$case_loss)),configurations=unique(b$oof$configuration_id[changed]))
 }
}
for(task in c('regression','binary','multiclass')) {
 a<-after[[paste(task,TRUE,sep='/')]];b<-after[[paste(task,FALSE,sep='/')]]
 fields<-setdiff(names(a),c('pool_checks','oof'))
 stopifnot(identical(a[fields],b[fields]))
}
jsonlite::write_json(list(verdict='All comparisons passed with explicitly checked binary case_loss correction.',workflows=length(before),retained_rows=sum(vapply(after,function(x)if(is.null(x$oof))0L else nrow(x$oof),integer(1))),changes=changes),args[3],auto_unbox=TRUE,pretty=TRUE,digits=16)
