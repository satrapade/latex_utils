
selection_path<-function(n,normalize=TRUE){
  res<-t(apply(diag(n)[sample(1:n,n),],2,cumsum))
  if(!normalize)return(res)
  f<-matrix(apply(res,2,sum),nrow=1)[rep(1,nrow(res)),]
  res/f
}

n_stock<-500
x<-mapply(rnorm,mean=rnorm(n_stock),sd=runif(n_stock,0.1,10),MoreArgs=list(n=1000))+   # random, uncorrelated
  cbind(rnorm(1000,sd=2),rep(0,100))[,sample(c(1,rep(2,1)),n_stock,replace=TRUE)]+ # 50% with a low vol common factor
  cbind(rnorm(1000,sd=20),rep(0,100))[,sample(c(1,rep(2,10)),n_stock,replace=TRUE)]+ # 5% with a medium vol common factor
  cbind(rnorm(1000,sd=200),rep(0,100))[,sample(c(1,rep(2,100)),n_stock,replace=TRUE)] # 0.5% with a high vol common factor

p<-mapply(log,mapply(function(i)apply(x%*%(selection_path(ncol(x))[,1:ncol(x)]),2,sd),1:100,SIMPLIFY=FALSE),SIMPLIFY = FALSE)
plot(0,xlim=c(1,length(p[[1]])),ylim=c(min(mapply(c,p)),max(mapply(c,p))),type="n")
invisible(mapply(lines,x=p,MoreArgs=list(lwd=2,col=rgb(0,0,0,0.1))))
#abline(h=log(sd(apply(x,1,mean))),col="red")

#m<-cor(x)
#hist(m[which(row(m)>col(m),arr.ind = TRUE)])




