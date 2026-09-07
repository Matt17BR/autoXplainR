# Diagnostic reproduction for the audited 0.3.0 source; run from repository root.
pkgload::load_all('.',quiet=TRUE)
set.seed(42)
d <- data.frame(x=ordered(rep(c('low','medium','high'),20),levels=c('low','medium','high')),z=rnorm(60),y=rnorm(60))
m <- AutoXplainR:::fit_tuned_neural_network(d,'y','regression',size=2,decay=0.1)
print(tryCatch(predict(m,d[1:4,]),error=conditionMessage))
r <- tryCatch(autoxplain(d,'y',model_set='tuned',learners='neural',max_models=1,nfolds=2,explain=FALSE),error=conditionMessage)
print(if(is.character(r)) r else r$tuning$candidates[c('family','status','error')])
