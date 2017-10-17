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

df2matrix(df,row_slack = 0,col_slack = 0)


