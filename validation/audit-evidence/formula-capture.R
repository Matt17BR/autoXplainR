# Diagnostic reproduction for the audited 0.3.0 source; run from repository root.
pkgload::load_all('.',quiet=TRUE)
set.seed(42); d <- as.data.frame(matrix(rnorm(3000*10),nrow=3000)); names(d)<-paste0('x',1:10); d$y<-d$x1+rnorm(3000)
r<-autoxplain(d,'y',explain=FALSE)
f<-r$models$simple_baseline
cat('model formula environment bindings:\n'); print(ls(environment(formula(f))))
cat('serialized baseline bytes:',length(serialize(f,NULL)),' object.size:',as.numeric(object.size(f)),'\n')
g<-unserialize(serialize(f,NULL)); environment(g$terms)<-baseenv(); tt<-attr(g$model,'terms'); environment(tt)<-baseenv(); attr(g$model,'terms')<-tt
cat('with sterile formula env:',length(serialize(g,NULL)),'\n')
cat('training rows in environment:',nrow(get('tuning_data',environment(formula(f)))),'\n')
cat('baseline environment contains primary model:',exists('primary',environment(formula(f)),inherits=FALSE),'\n')
cat('predictions unchanged:',isTRUE(all.equal(predict(f,d[1:3,]),predict(g,d[1:3,]))),'\n')
