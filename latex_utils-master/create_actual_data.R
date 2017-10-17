
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


data_directory<-"H:/equity_one_pager/latex_child_luke_and_duke_portfolio_summary"
prefix<-"duke_"
row_names<-TRUE

# pair-level data
pair_exposure     <- load_matrix(paste0(data_directory,"/",prefix,"pair_exposure.csv"),row_names=row_names)
pair_days         <- load_matrix(paste0(data_directory,"/",prefix,"pair_days.csv"),row_names=row_names)
pair_local_pnl    <- load_matrix(paste0(data_directory,"/",prefix,"pair_local_pnl.csv"),row_names=row_names)
pair_long_pnl     <- load_matrix(paste0(data_directory,"/",prefix,"pair_long_pnl.csv"),row_names=row_names)
pair_short_pnl    <- load_matrix(paste0(data_directory,"/",prefix,"pair_short_pnl.csv"),row_names=row_names)
drop_one_pair_pnl <- load_matrix(paste0(data_directory,"/",prefix,"drop_one_pair_pnl.csv"),row_names=row_names)

# manager-level data
manager_exposure     <- load_matrix(paste0(data_directory,"/",prefix,"manager_exposure.csv"),row_names=row_names)
manager_local_pnl    <- load_matrix(paste0(data_directory,"/",prefix,"manager_local_pnl.csv"),row_names=row_names)
drop_one_manager_pnl <- load_matrix(paste0(data_directory,"/",prefix,"drop_one_manager_pnl.csv"),row_names=row_names)

# fund-level data
exposure        <- load_matrix(paste0(data_directory,"/",prefix,"exposure.csv"),row_names=row_names)
long_exposure   <- load_matrix(paste0(data_directory,"/",prefix,"long_exposure.csv"),row_names=row_names)
short_exposure  <- load_matrix(paste0(data_directory,"/",prefix,"short_exposure.csv"),row_names=row_names)
local_pnl       <- load_matrix(paste0(data_directory,"/",prefix,"local_pnl.csv"),row_names=row_names)

# color assigment
prefix<-""
manager_col <- fread(paste0(data_directory,"/",prefix,"manager_col.csv"))
pair_col    <- fread(paste0(data_directory,"/",prefix,"pair_col.csv"))


#determine pairs, managers and pair-to-manager mapping
pairs<-sort(unique(colnames(pair_exposure)))
managers<-sort(unique(colnames(manager_exposure)))
manager_pairs<-structure(
  diag(length(managers))[match(gsub("[0-9]+","",pairs),managers),],
  dimnames=list(pairs,managers)
)

# fix any data issues
if(any(colnames(drop_one_pair_pnl)!=pairs))colnames(drop_one_pair_pnl)<-pairs
if(any(colnames(drop_one_manager_pnl)!=managers))colnames(drop_one_manager_pnl)<-managers


# save color assigments
fwrite(manager_col,"actual_data/manager_col.csv")
fwrite(pair_col,"actual_data/pair_col.csv")

# save pair-level data
fwrite(data.table(instrument=rownames(pair_exposure),pair_exposure),"actual_data/pair_exposure.csv")
fwrite(data.table(instrument=rownames(pair_days),pair_days),"actual_data/pair_days.csv")
fwrite(data.table(instrument=rownames(pair_local_pnl),pair_local_pnl),"actual_data/pair_local_pnl.csv")
fwrite(data.table(instrument=rownames(pair_long_pnl),pair_long_pnl),"actual_data/pair_long_pnl.csv")
fwrite(data.table(instrument=rownames(pair_short_pnl),pair_short_pnl),"actual_data/pair_short_pnl.csv")
fwrite(data.table(instrument=rownames(drop_one_pair_pnl),drop_one_pair_pnl),"actual_data/drop_one_pair_pnl.csv")

# save pm-level data
fwrite(data.table(instrument=rownames(manager_exposure),manager_exposure),"actual_data/manager_exposure.csv")
fwrite(data.table(instrument=rownames(manager_local_pnl),manager_local_pnl),"actual_data/manager_local_pnl.csv")
fwrite(data.table(instrument=rownames(drop_one_manager_pnl),drop_one_manager_pnl),"actual_data/drop_one_manager_pnl.csv")

# save fund-level data
fwrite(data.table(instrument=rownames(exposure),exposure),"actual_data/exposure.csv")
fwrite(data.table(instrument=rownames(long_exposure),long_exposure),"actual_data/long_exposure.csv")
fwrite(data.table(instrument=rownames(short_exposure),short_exposure),"actual_data/short_exposure.csv")
fwrite(data.table(instrument=rownames(local_pnl),local_pnl),"actual_data/local_pnl.csv")











