#
# create pair icons f
#
PROJECT_DIRECTORY<-"" # keep updated

library(knitr)
library(magick)
library(pdftools)
library(brotli)
library(base64enc)

#setwd(paste0(PROJECT_DIRECTORY,"latex_create_pair_icons"))

temp_files<-list.files(
  path=paste0(PROJECT_DIRECTORY,"tikz-figures"),
  pattern="(^[A-Za-z0-9]+\\.dpth$)|(^[A-Za-z0-9]+\\.log$)|(^[A-Za-z0-9]+\\.md5$)|(^[A-Za-z0-9]+\\.pdf$)",
  recursive = FALSE,
  full.names = TRUE
)
file.remove(temp_files)

plot_files<-list.files(
  path=paste0(PROJECT_DIRECTORY,"figure"),
  pattern="(^[A-Za-z0-9]+\\.dpth$)|(^[A-Za-z0-9]+\\.log$)|(^[A-Za-z0-9]+\\.md5$)|(^[A-Za-z0-9]+\\.pdf$)",
  recursive = FALSE,
  full.names = TRUE
)
file.remove(plot_files)

knit("create_synthetic_pair_icons.Rnw")
system("pdflatex -shell-escape create_synthetic_pair_icons.tex")

temp_files<-list.files(
  path=paste0(PROJECT_DIRECTORY,"tikz-figures"),
  pattern="(^[A-Za-z0-9]+\\.dpth$)|(^[A-Za-z0-9]+\\.log$)|(^[A-Za-z0-9]+\\.md5$)",
  recursive = FALSE,
  full.names = TRUE
)
file.remove(temp_files)

images<-data.table(
  file=list.files(
    path=paste0(PROJECT_DIRECTORY,"tikz-figures"),
    pattern="^[A-Za-z0-9]+\\.pdf$",
    recursive = FALSE,
    full.names = TRUE
  ),
  name=gsub("\\.pdf$","",list.files(
    path=paste0(PROJECT_DIRECTORY,"tikz-figures"),
    pattern="^[A-Za-z0-9]+\\.pdf$",
    recursive = FALSE,
    full.names = FALSE
  ))
)

# [,
#   "image":=do.call(c,mapply(function(f)base64encode(readBin(f,"raw",file.info(f)[["size"]])),file,SIMPLIFY=FALSE))
# ]

fwrite(images,"images.csv")

x<-mapply(function(f)image_read(pdf_render_page(f,page=1,dpi=300)),images$file,SIMPLIFY = FALSE)


