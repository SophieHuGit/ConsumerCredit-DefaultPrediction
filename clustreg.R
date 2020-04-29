clustreg=function(dat,k,tries,sed,niter)
{
  
  set.seed(sed)
  dat=as.data.frame(dat)
  rsq=rep(NA,niter)
  res=list()
  rsq.best=0
  for(l in 1:tries) 
  {
    
    c = sample(1:k,nrow(dat),replace=TRUE)
    yhat=rep(NA,nrow(dat))
    for(i in 1:niter) 
    {		
      resid=pred=matrix(0,nrow(dat),k)
      for(j in 1:k)
      {	
        pred[,j]=predict(glm(dat[c==j,],family="gaussian"),newdata=dat)		
        resid[,j] = (pred[,j]-dat[,1])^2
      }
      
      c = apply(resid,1,which.min)
      for(m in 1:nrow(dat)) {yhat[m]=pred[m,c[m]]}
      rsq[i] = cor(dat[,1],yhat)^2	
    }
    
    if(rsq[niter] > rsq.best) 
    {	
      rsq.best=rsq[niter]
      l.best=l
      c.best=c
      yhat.best=yhat
    }
  }
  
  res=list("Complete")
  for(i in k:1) {res=list(summary(lm(dat[c.best==i,])),res)}
  
  return(list(data=dat,nclust=k,tries=tries,seed=sed,rsq.best=rsq.best,number.loops=niter, Best.try=l.best,cluster=c.best,results=res))
}