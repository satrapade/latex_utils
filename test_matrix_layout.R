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

x<-rnorm(100)
y<-rnorm(100)
df<-data.table(x=x,y=y,text=paste0("t:",bin(x,10),"-",bin(y,10)))

stopifnot(all(names(table(df$text))==names(table(df2matrix(df,row_slack = 0,col_slack = 0)))))
stopifnot(all(table(df$text)==table(df2matrix(df,row_slack = 0,col_slack = 0))))

m<-df2matrix(df,row_slack = 0,col_slack = 0)


