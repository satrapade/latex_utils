#
# remove intermediate files
#

gz_files<-list.files(pattern="*.gz",recursive=TRUE)
if(length(gz_files)>0)file.remove(gz_files)

log_files<-list.files(pattern="*.log",recursive=TRUE)
if(length(log_files)>0)file.remove(log_files)

concordance_tex_files<-list.files(pattern="*-concordance.tex",recursive=TRUE)
if(length(concordance_tex_files)>0)file.remove(concordance_tex_files)

aux_files<-list.files(pattern="*.aux",recursive=TRUE)
if(length(aux_files)>0)file.remove(aux_files)

