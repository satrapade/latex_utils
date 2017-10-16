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

df<-data.table(
  x=structure(
    hdr="test",
    format=quote(n_fmt(this)),
    round(rnorm(100),digits=1)
  )
)


data.frame_format(df)

