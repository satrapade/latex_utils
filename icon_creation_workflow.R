#
# create pair icons f
#

library(knitr)
library(magick)
library(pdftools)
library(brotli)
library(base64enc)

target<-"synthetic_data"
target_directory<-gsub("\\\\","/",normalizePath(paste0(target)))
  
temp_files<-list.files(
  path=paste0(target_directory,"/","tikz-figures"),
  pattern="(^[A-Za-z0-9]+\\.dpth$)|(^[A-Za-z0-9]+\\.log$)|(^[A-Za-z0-9]+\\.md5$)|(^[A-Za-z0-9]+\\.pdf$)",
  recursive = FALSE,
  full.names = TRUE
)
file.remove(temp_files)

plot_files<-list.files(
  path="figure",
  pattern="(^[A-Za-z0-9]+\\.dpth$)|(^[A-Za-z0-9]+\\.log$)|(^[A-Za-z0-9]+\\.md5$)|(^[A-Za-z0-9]+\\.pdf$)",
  recursive = FALSE,
  full.names = TRUE
)
file.remove(plot_files)

#
# write the icon-generation job spec
# to a file where "create_synthetic_pair_icons.Rnw" 
# will read it and direct its output accordingly
#
fwrite(data.table(
  externalization_flag="ENABLE", # enable externalization
  externalization_command=paste0("\\tikzexternalize[prefix=",paste0(target,"/","tikz-figures"),"/]"),
  target=target,
  data_directory=target_directory,
  icon_directory=paste0(target_directory,"/","tikz-figures"),
  timestamp=as.character(Sys.time()),
  md5=digest(file.info(list.files(target_directory,full.names = TRUE))),
  items=paste(fread(paste0(target_directory,"/","pair_col.csv"))$item,collapse=",")
),"icon_creation_job.csv")

knit("create_synthetic_pair_icons.Rnw")
system("pdflatex -shell-escape create_synthetic_pair_icons.tex")

# now disable externalization, so it does not happen a second time
fwrite(data.table(
  externalization_flag="DISABLE",
  externalization_command="",
  target=target,
  data_directory=target_directory,
  icon_directory=paste0(target_directory,"/","tikz-figures"),
  timestamp=as.character(Sys.time()),
  md5=digest(file.info(list.files(target_directory,full.names = TRUE))),
  items=paste(fread(paste0(target_directory,"/","pair_col.csv"))$item,collapse=",")
),"icon_creation_job.csv")


temp_files<-list.files(
  path=paste0(target_directory,"/","tikz-figures"),
  pattern="(^[A-Za-z0-9]+\\.dpth$)|(^[A-Za-z0-9]+\\.log$)|(^[A-Za-z0-9]+\\.md5$)",
  recursive = FALSE,
  full.names = TRUE
)
file.remove(temp_files)

#
# create name-to-image-filename maps
# for pair icons and other assets
#

images<-data.table(
  file=list.files(
    path=paste0(target_directory,"/","tikz-figures"),
    pattern="^[A-Za-z0-9]+\\.pdf$",
    recursive = FALSE,
    full.names = TRUE
  ),
  name=gsub("\\.pdf$","",list.files(
    path=paste0(target_directory,"/","tikz-figures"),
    pattern="^[A-Za-z0-9]+\\.pdf$",
    recursive = FALSE,
    full.names = FALSE
  ))
)[!grepl("^ICON",name)]

keys<-data.table(
  file=list.files(
    path=paste0(target_directory,"/","tikz-figures"),
    pattern="^[A-Za-z0-9]+\\.pdf$",
    recursive = FALSE,
    full.names = TRUE
  ),
  name=gsub("\\.pdf$","",list.files(
    path=paste0(target_directory,"/","tikz-figures"),
    pattern="^[A-Za-z0-9]+\\.pdf$",
    recursive = FALSE,
    full.names = FALSE
  ))
)[grepl("^ICON",name)]


fwrite(images,paste0(target_directory,"/","images.csv"))
fwrite(keys,paste0(target_directory,"/","keys.csv"))

x<-mapply(function(f)image_read(pdf_render_page(f,page=1,dpi=300)),images$file,SIMPLIFY = FALSE)
y<-mapply(function(f)image_read(pdf_render_page(f,page=1,dpi=300)),keys$file,SIMPLIFY = FALSE)


