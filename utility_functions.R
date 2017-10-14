#
#
#

.satrapade_functions_version="Version 1.00"
library(RcppRoll)
library(stringi)
library(data.table)
library(Matrix)
library(Matrix.utils)
library(scales)
options(stringsAsFactors=FALSE)
library(R.cache)
library(digest)
library(timeDate)


# fast data dump/pump functions, using binary little-endian connections
fdump<-function(object,filename){
  conn<-file(filename,open="wb")
  serialize(object,conn,ascii=FALSE,xdr=FALSE)
  close(conn)
}
fpump<-function(filename){
  unserialize(readBin(filename,"raw",file.info(filename)[["size"]]))
}

make_date_range<-function(
  start="2017-06-01",
  end="2017-06-30"
){
  fmt="%Y-%m-%d"
  date_seq<-seq(from=as.Date(start,format=fmt),to=as.Date(end,format=fmt),by=1)
  as.character(date_seq,format=fmt)
}



#
# > replace_zero_with_last(c(0,0,1,1,2,3,0,0,0,1,1,1,2,3,0,0,0))
# [1] 1 1 1 1 2 3 3 3 3 1 1 1 2 3 3 3 3
#
replace_zero_with_last<-function(x,a=x!=0){
  if(all(abs(x)<1e-10))return(x)
  x[which(a)[c(1,1:sum(a))][cumsum(a)+1]]
}
stopifnot(all(replace_zero_with_last(c(0,0,1,2,3,0,0,4,5,6,0,0))==c(1,1,1,2,3,3,3,4,5,6,6,6)))


rec<-function(x)seq_along(x)-which(c(TRUE,tail(x,-1)>0))[cumsum(c(TRUE,tail(x,-1)>0))]
meanrd<-function(x)mean(rec(cummax(cumsum(x))==cumsum(x)))
maxrd<-function(x)max(rec(cummax(cumsum(x))==cumsum(x)))



vol_pa<-function(x,exclude_zero=(x!=0), holidays = holidayNYSE()) {
  if(is.null(names(x))) 
    good_days <- TRUE 
  else 
    good_days <- !weekdays(as.Date(names(x),format="%Y-%m%-%d")) %in% c("Saturday","Sunday")
  sqrt(252)*sd(drop(x[exclude_zero & good_days]))
}

sharpe<-function(x,exclude_zero=(x!=0), holidays = holidayNYSE() ) {
  if(is.null(names(x))) 
    good_days <- TRUE 
  else 
    good_days <- !weekdays(as.Date(names(x),format="%Y-%m%-%d")) %in% c("Saturday","Sunday")
  round(sqrt(252)*mean(drop(x[exclude_zero & good_days]))/(vol_pa(x)/sqrt(252)),digits=2)   # annualized
}
bps<-function(x,exclude_zero=(x!=0))round(10000*mean(drop(x[exclude_zero])),digits=2)
gain_ratio<-function(x)round(sum(x>0)/sum(x!=0),digits=2)
trade_ratio<-function(x)round(sum(x!=0)/length(x),digits=2)
drawdown<-function(x)round(100*max(cummax(cumsum(x))-cumsum(x)),digits=2)
pnl_stats<-function(x, show_tr = FALSE, show_gr = FALSE){
  if(class(x)=="matrix")if(ncol(x)>1)x<-x[,1]
  ret<-c(ret=round(100*365.25*mean(x),digits=3),vol=round(100*vol_pa(x),digits=2),sharpe=sharpe(x),dd=drawdown(x),mrd=round(meanrd(x),digits=2))
  if(show_tr) ret<-c(ret, trade_ratio=trade_ratio(x))
  if(show_gr) ret<-c(ret, gain_ratio=gain_ratio(x))
  ret
}
pnl_plot<-function(x,...){
  pargs<-as.list(match.call(expand.dots=TRUE))
  if(!"ylab" %in% names(pargs)) ylab<-deparse(substitute(x)) else ylab<-pargs$ylab
  if(!"main" %in% names(pargs)) main<-paste(names(pnl_stats(x)),pnl_stats(x),sep=":",collapse=" ") else main<-pargs$main
  z<-cumsum(x)
  ylim<-c(min(z),max(z))
  if("ylim" %in% names(pargs))ylim=eval(pargs$ylim,parent.frame())
  if("lwd" %in% names(pargs)){lwd<-eval(pargs$lwd,parent.frame())} else lwd<-1
  if("col" %in% names(pargs)){col<-eval(pargs$col,parent.frame())} else col<-"black"
  plot(cumsum(x),type="l",xlab="",ylab=ylab,main=main,axes=FALSE,ylim=ylim,lwd=lwd,col=col)
  if(!is.null(names(x))){
    axis(1,at=seq(1,length(x),length.out=5),labels=names(x)[seq(1,length(x),length.out=5)])
    axis(2)
  } else { axis(1); axis(2)}
}

plot_with_dates<-function(
  x,
  main="",
  ylab="m$",
  divisor=1e6,
  ylim=local({
    if(diff(range(c(min(x),max(x))))>0){
      c(min(x),max(x))
    } else {
      c(mean(x)-1,mean(x)+1)
    }
  }),
  col=rgb(0,0,1,0.25)
){
  plot(x,type="l",lwd=5,col=col,axes=FALSE,xlab="",ylab="",main=main,cex.main=2,ylim=ylim)
  dates<-as.character(as.Date(names(x),format="%Y-%m-%d"),format="%Y-%b-%d")
  at<-seq(1,length(x),length.out = 5)
  labels<-gsub("-","\n",dates)
  axis(1,at=at,labels=labels[at],line=NA,padj=0.75,xlab="",cex.axis=1.5)
  at<-axTicks(2)
  labels<-comma(round(at/divisor,digits=1),digits=1)
  axis(2,at=at,labels=labels,cex.axis=2)
}

# moving average of previous n elements : 0 for first n-1 elements
ma<-function(x,n,f=identity){
  res<-as.numeric(filter(f(x),rep(1/n,n),method="convolution",sides=1,circular=FALSE)); ifelse(is.na(res),0,res)
}

rolling_beta<-function(strat,hedge,w) {
  var_hedge<-roll_var(hedge,w)
  cov_strat_hedge<-(w/(w-1))*(roll_mean(strat*hedge,w)-roll_mean(strat,w)*roll_mean(hedge,w))
  beta<-ifelse(var_hedge>0,cov_strat_hedge/var_hedge,0)
  beta
}


# shift forward if n +ve , backward if n -ve.
shift<-function(a,n=1,filler=0){
  x<-switch(class(a),matrix=a,matrix(a,ncol=1,dimnames=list(names(a),NULL)))
  if(n==0)return(x)
  if(n>0){
    rbind(matrix(filler,ncol=ncol(x),nrow=n),head(x,-n)) 
  } else {
    rbind(tail(x,n),matrix(filler,ncol=ncol(x),nrow=abs(n)))
  }
}


# replace non-finite elements in x with zeroes
scrub<-function(x){
  if(length(x)==0)return(0)
  x[which(!is.finite(x))]<-0
  x
}

pnl_matrix<-function(perf, digits = 2){
  month_map<-c("01"="Jan","02"="Feb","03"="Mar","04"="Apr","05"="May","06"="Jun","07"="Jul","08"="Aug","09"="Sep","10"="Oct","11"="Nov","12"="Dec")
  perf_dates<-structure(do.call(rbind,strsplit(names(perf),"-")),dimnames=list(names(perf),c("Year","Month","Day")))
  perf_dates[,"Month"]<-month_map[perf_dates[,"Month"]]
  perf_years  <- sort(unique(perf_dates[,"Year"]))
  perf_months <- month_map
  res<-structure(
    outer(perf_years,perf_months,function(i_vec,j_vec)mapply(function(i,j){
      perf_ndx <- perf_dates[,"Year"]==i & perf_dates[,"Month"]==j
      if(sum(perf_ndx)==0)return(NA)
      prod(perf[perf_ndx]+1)-1
    },i_vec,j_vec)),
    dimnames=list(perf_years,perf_months)
  )
  round(cbind(res,Year=apply(res,1,function(r)prod(r[!is.na(r)]+1)-1))*100,digits=2)
}



make_matrix<-function(x,fun=function(x)x,empty_value=NA){
  month_map<-c(
    "01"="Jan","02"="Feb","03"="Mar","04"="Apr","05"="May","06"="Jun",
    "07"="Jul","08"="Aug","09"="Sep","10"="Oct","11"="Nov","12"="Dec"
  )
  x_dates<-structure(do.call(rbind,strsplit(names(x),"-")),dimnames=list(names(x),c("Year","Month","Day")))
  x_dates[,"Month"]<-month_map[x_dates[,"Month"]]
  x_years  <- sort(unique(x_dates[,"Year"]))
  x_months <- month_map
  res<-structure(
    outer(x_years,x_months,function(i_vec,j_vec)mapply(function(i,j){
      x_ndx <- x_dates[,"Year"]==i & x_dates[,"Month"]==j
      if(sum(x_ndx)==0)return(empty_value)
      fun(x[x_ndx])
    },i_vec,j_vec)),
    dimnames=list(x_years,x_months)
  )
  cbind(res,Year=apply(res,1,function(r)fun(r[!is.na(r)])))
}

user_id<-paste(system("whoami",TRUE),"_",system("hostname",TRUE),sep="")

# find which environment contains a name
where<-function (name, env = parent.frame()) {
  if(!is.character(name))return(NULL)
  if(length(name)!=1)return(NULL)
  if(class(env)!="environment")return(NULL)
  if (identical(env, emptyenv())) return(NULL)
  if (exists(name, env, inherits = FALSE)) return(env)
  where(name, parent.env(env))
}


########
#
#
list2data.frame<-function(x){
  l2e<-expression({data.frame(
    names=ls(),
    values=mapply(get,ls(),MoreArgs=list(envir=environment())),
    stringsAsFactors=FALSE,
    row.names=NULL
  )})
  eval(l2e,as.list(x))
}

###################
#
# repeat rows, columns
# to make a matrix
#
rep.row<-function(x,n){
  matrix(rep(x,each=n),nrow=n)
}
rep.col<-function(x,n){
  matrix(rep(x,each=n), ncol=n, byrow=TRUE)
}

apply_column<-function(x,f){
  y<-apply(x,2,f)
  rownames(y)<-tail(rownames(x),nrow(y))
  y
}

apply_row<-function(x,f){
  y<-t(apply(x,1,f))
  rownames(y)<-tail(rownames(x),nrow(y))
  colnames(y)<-colnames(x)
  y
}

#
shift_matrix<-structure(function(m,n,filler=0){
  if(n==0)return(m)
  if(n>0){
    sm<-rbind(
      matrix(filler,ncol=ncol(m),nrow=n),
      m[head(1:nrow(m),-n),,drop=FALSE]
    )
    dimnames(sm)<-dimnames(m)
    return(sm)
  }
  if(n<0){
    sm<-rbind(
      m[tail(1:nrow(m),n),,drop=FALSE],
      matrix(filler,ncol=ncol(m),nrow=abs(n))
    )
    dimnames(sm)<-dimnames(m)
    return(sm)
  }
  stop("invalid shift")
},location="helper_functions_insight.R")

#
matrix_indices<-structure(function(x)cbind(
  rep(1:nrow(x),times=ncol(x)),
  rep(1:ncol(x),each=nrow(x))
),location="helper_functions_insight.R")

fast_col_apply<-structure(function(x,fun)structure(
  apply(structure(as.matrix(x),dimnames=NULL),2,fun),
  dimnames=dimnames(x)
),location="helper_functions_insight.R")

nz<-structure(function(x,tol=1e-12){
  if(all(abs(x)<tol))return(0)
  x[abs(x)>tol]
},location="helper_functions_insight.R")

stopifnot(nz(c(0,1,0))==1)



