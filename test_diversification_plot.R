
selection_path<-function(n,normalize=TRUE){
  res<-t(apply(diag(n)[sample(1:n,n),],2,cumsum))
  if(!normalize)return(res)
  f<-matrix(apply(res,2,sum),nrow=1)[rep(1,nrow(res)),]
  res/f
}

x<-matrix(rnorm(5*100),ncol=100)
p<-mapply(function(i)apply(x%*%(selection_path(ncol(x))[,1:ncol(x)]),2,sd),1:100,SIMPLIFY=FALSE)
plot(0,xlim=c(1,length(p)),ylim=c(0,max(mapply(max,p))),type="n")
invisible(mapply(lines,x=p,MoreArgs=list(lwd=2,col=rgb(0,0,0,0.1))))

#m<-cor(x)
#hist(m[which(row(m)>col(m),arr.ind = TRUE)])




