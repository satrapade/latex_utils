library(digest)
library(data.table)
library(png)
library(magick)
library(scales)
library(brotli)
library(sodium)
library(bitops)

# test image packing
image_write(pack_image(
  #fig=image_read("frink.png"),
  fig=image_read("lag_plot.png"),
  data=data.table(file=list.files(pattern="*\\.(R$)|(Rnw$)",recursive=TRUE))[,.(
    file=file,
    content=mapply(readLines,file,SIMPLIFY=FALSE)
  )],
  pword="arxidia"
),path="lag_plot.png",format="png")

# unpack packed image
x<-unpack_image(img=image_read("lag_plot.png"),pword="arxidia")

# test that all went well
digest(x$data)==x$md5


