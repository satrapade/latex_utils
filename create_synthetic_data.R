
require(Hmisc)
require(Rtsne)
require(digest)
require(stringi)
require(readxl)
require(scales)
require(data.table)
require(Matrix)
require(Matrix.utils)
require(clue)
require(magick)
source("utility_functions.R")
source("sheet_utility_functions.R")
source("latex_helpers_v2.R")

set.seed(0)

managers<-sort(mapply(function(i)stri_flatten(sample(LETTERS,sample(c(2,3),1))),1:10))
pairs<-sort(paste0(sample(managers,100,replace=TRUE),sample(1:1000,100)))
stocks<-paste0("STOCK",1:500)

manager_col<-assign_color(managers,col_alpha=1)
fwrite(manager_col,"synthetic_data/manager_col.csv")
pair_col<-assign_color(pairs,col_alpha=1)
fwrite(pair_col,"synthetic_data/pair_col.csv")

# 
# random total returns with random addition of common factors
# with varying volatility and 0-return weekends
history_days<-1000
tret<-(
  matrix( # uncorrelated stock-specific risk
    rnorm(length(stocks)*history_days,sd=0.01,mean=0),
    ncol=length(stocks),
    dimnames=list(1:history_days,stocks)
  )+
  cbind( # low vol common factor added to roughly 50% of stocks
    rnorm(history_days,sd=0.01),
    rep(0,history_days)
  )[,sample(c(1,rep(2,1)),length(stocks),replace=TRUE)]+
  cbind( # medium vol common factor added to roughly 10% of stocks
    rnorm(history_days,sd=0.02),
    rep(0,history_days)
  )[,sample(c(1,rep(2,10)),length(stocks),replace=TRUE)]+ 
  cbind( # high vol common factor added to roughly 1% of stocks
    rnorm(history_days,sd=0.04), 
    rep(0,history_days)
  )[,sample(c(1,rep(2,100)),length(stocks),replace=TRUE)] 
)*matrix(
  rep(rep(c(0,1),times=c(2,5)),length.out=history_days),
  ncol=1
)[,rep(1,length(stocks))] # weekends have 0 zeturns

adv<-matrix(
  sample(1:100,length(stocks),replace=TRUE),
  nrow=length(stocks),
  ncol=1,
  dimnames=list(stocks,NULL)
)

# 1-5 stocks per "pair", random directions. todo: force L/S positions only
# pair exposures
pair_exposure<-mapply(function(i){
  selected_stocks<-(seq_along(stocks) %in% sample(seq_along(stocks),sample(1:5,1)))
  stock_direction<-sample(c(-1,1),length(stocks),replace=TRUE)
  structure(selected_stocks*stock_direction,.Names=stocks)
},pairs)
pair_days<-pair_exposure*adv[,rep(1,ncol(pair_exposure))]
# pair p&l 
pair_local_pnl<-tret%*%pair_exposure
pair_long_pnl<-tret%*%pmax(pair_exposure,0)
pair_short_pnl<-tret%*%pmax(-pair_exposure,0)
drop_one_pair_pnl<-structure(
  pair_local_pnl%*%(1-diag(length(pairs))),
  dimnames=dimnames(pair_local_pnl)
)
# save pair-level data
fwrite(data.table(pair_exposure),"synthetic_data/pair_exposure.csv")
fwrite(data.table(pair_days),"synthetic_data/pair_days.csv")
fwrite(data.table(pair_local_pnl),"synthetic_data/pair_local_pnl.csv")
fwrite(data.table(pair_long_pnl),"synthetic_data/pair_long_pnl.csv")
fwrite(data.table(pair_short_pnl),"synthetic_data/pair_short_pnl.csv")
fwrite(data.table(drop_one_pair_pnl),"synthetic_data/drop_one_pair_pnl.csv")


# mapping matrix of pair ownership by manager
manager_pairs<-structure(
  diag(length(managers))[match(gsub("[0-9]+","",pairs),managers),],
  dimnames=list(pairs,managers)
)
# manager exposure
manager_exposure<-pair_exposure%*%manager_pairs
# manager p&l
manager_local_pnl<-tret%*%manager_exposure
drop_one_manager_pnl<-structure(
  manager_local_pnl%*%(1-diag(length(managers))),
  dimnames=dimnames(manager_local_pnl)
)
# save pm-level data
fwrite(data.table(manager_exposure),"synthetic_data/manager_exposure.csv")
fwrite(data.table(manager_local_pnl),"synthetic_data/manager_local_pnl.csv")
fwrite(data.table(drop_one_manager_pnl),"synthetic_data/drop_one_manager_pnl.csv")

# fund exposure
exposure<-pair_exposure%*%matrix(1,ncol=1,nrow=length(pairs))
# fund p&l
long_exposure<-pmax(exposure,0)
short_exposure<-pmax(-exposure,0)
local_pnl<-tret%*%exposure
# save fund-level data
fwrite(data.table(exposure),"synthetic_data/exposure.csv")
fwrite(data.table(long_exposure),"synthetic_data/long_exposure.csv")
fwrite(data.table(short_exposure),"synthetic_data/short_exposure.csv")
fwrite(data.table(local_pnl),"synthetic_data/local_pnl.csv")











