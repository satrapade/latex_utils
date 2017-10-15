

make_external_latex<-function(
  latex="testing123",
  filename=NULL
)paste0(
  if(!is.null(filename)){paste0("\\tikzsetnextfilename{",filename,"}"," ")}else{""},
  "\\tikz{\\path (0,0) node[inner sep=0,outer sep=0]{",
  latex,
  "};}"
)


curly<-function(...)paste0("{",...,"}",collapse="")
asc <- function(x) { strtoi(charToRaw(x),16L) }
pct<-paste("\\",as.octmode(asc("%")),sep="")

assign_color<-function(x,prefix="",col_scheme=rainbow,col_alpha=0.33){
  items<-sort(unique(x))
  col<-col_scheme(length(items),alpha=col_alpha)
  res<-data.table(
    item=x,
    name=paste0(prefix,gsub("[\\.]+","",make.names(toupper(x)))),
    r_color=col[match(x,items)]
  )[,
    c("latex_col","latex_col_def"):=list(
      latex_col=stri_sub(gsub("^#","",r_color),1,6),
      latex_col_def=paste0("\\definecolor{",name,"}{HTML}{",stri_sub(gsub("^#","",r_color),1,6),"}\n")
    )
  ]
  res
}

data.frame_attributes<-function(x)mapply(function(n,x,v){
  list(attributes(x[[n]]))
},names(x),MoreArgs=list(x=x))


data.frame_attribute_names<-function(x){
  mapply(function(n,x){
    cn<-names(attributes(x[[n]]))
    do.call(paste,list(cn,collapse="|"))
  },names(x),MoreArgs=list(x=x))
}

data.frame_attribute_values<-function(x,v)mapply(function(n,x,v){
  list(attributes(x[[n]])[[v]])
},names(x),MoreArgs=list(x=x,v=v))


# data.frame collumns can have a "format" attribute
# containing  formatting expression on "this", which
# is the column's contents
data.frame_format<-function(x){
  n<-data.frame_attribute_names(x)
  i<-grepl("format",n)
  if(sum(i)>0){
    fx<-mapply(function(cn,x){
      f<-attributes(x[[cn]])$format
      e<-do.call(substitute,list(f,list(this=x[[cn]])))
      res<-try(eval(e),silent=TRUE)
      if(class(res)=="try-error"){
        err_str<-paste0(
          "expression:[",paste0(deparse(e),collapse=""),"] ",
          "column name:[",cn,"] ",
          "error:[",res,"] ",
          collapse=""
        )
        stop("formatting error:",err_str)
      }
      res
    },names(x)[i],MoreArgs=list(x=x),SIMPLIFY=FALSE)
    x[names(fx)]<-fx
  }
  x
}

data.frame_add_format<-function(
  x,
  cols=names(x),
  format=quote(sanitize(this)),
  hdr=quote(add_latex_env(sanitize(this),"\\bf"))
){
  for(i in cols){x[[i]]<-structure(hdr=hdr,format=format,x[[i]])}
  x
}


data.frame_header<-function(x){
  n<-data.frame_attribute_names(x)
  v<-data.frame_attribute_values(x,"hdr")
  i<-grepl("hdr",n)
  res<-lapply(as.list(colnames(x)),latexTranslate)
  e<-as.list(as.data.frame(x))
  hds<-mapply(function(h,this){
    e$this<-this
    if(class(h)=="character")return(h)
    if(class(h)=="call")return(eval(h,e))
    return("")
  },v[i],names(e)[i])
  res[i]<-hds
  unlist(res)
}
# filenames are platform specific
make_full_directory<-function(x){
  os_type<-.Platform$OS.type
  if(os_type=="windows")return(paste0(gsub("\\\\","/",normalizePath(getwd())),"/",x))
  paste0(normalizePath(getwd()),"\\",x)
}
latex_wd<-function()do.call(
  paste0,
  c(mapply(latexTranslate,strsplit(gsub("\\\\","/",normalizePath(getwd())),":"),SIMPLIFY=FALSE),list(collapse=":"))
)
make_plot<-function(plot_expr,height="2cm",width="3cm",envir=parent.frame())paste(capture.output({
  #paste0(getwd(),"/figure")
  fname<-gsub("\\","/",tempfile(pattern = "plot",tmpdir=make_full_directory("figure"),fileext=".pdf"),fixed=TRUE)
  pdf(file=fname,width=7,height=7)
  eval(plot_expr,envir=envir)
  invisible(dev.off())
  cat("\\begin{minipage}[t][",height,"][t]{",width,"}\n")
  cat(paste0("\\includegraphics[height=",height,",width=",width,",valign=T]{",fname,"}\n"),sep="")
  cat("\\end{minipage}\n")
}),sep="",collapse="\n")

make_png_plot<-function(plot_expr,height="2cm",width="3cm",envir=parent.frame())paste0(
  "\\begin{minipage}[t][",height,"][t]{",width,"}",
  local({
    fname<-gsub("\\","/",tempfile(pattern = "plot",tmpdir=make_full_directory("figure"),fileext=".png"),fixed=TRUE)
    png(file=fname,width=1024,height=1024)
    eval(plot_expr,envir=envir)
    invisible(dev.off())
    paste0("\\includegraphics[height=",height,",width=",width,",valign=T]{",fname,"}")
  }),
  "\\end{minipage}\n",
  sep="",
  collapse="\n"
)



web_png<-function(url,height="2cm",width="3cm")paste(capture.output({
  #paste0(getwd(),"/figure")
  fname<-gsub("\\","/",tempfile(pattern = "web", tmpdir=make_full_directory("figure"),fileext = ".png"),fixed=TRUE)
  download.file(url,fname)
  cat("\\begin{minipage}{",width,"}\n")
  cat(paste0("\\includegraphics[height=",height,",width=",width,"]{",fname,"}\n"),sep="")
  cat("\\end{minipage}\n")
}),sep="",collapse="\n")

add_comma<-function(x,digits=2){
  x<-as.data.frame(x)
  if(nrow(x)==0)return(x)
  if(class(x)!="data.frame")return(NULL)
  n<-names(x)
  xc<-mapply(function(a)class(x[[a]]),n)
  x0<-mapply(function(a,ac){
    if(ac!="numeric")x[[a]]
    comma(x[[a]],digits=digits)
  },n,xc,SIMPLIFY=FALSE)
  do.call(data.frame,c(x0,list(row.names=rownames(x),stringsAsFactors=FALSE)))
}

#
# latex table rend
#

ntable<-function(
  df,
  add_rownames=TRUE,
  add_header=TRUE,
  title=NULL,
  pre_title_hline=TRUE,
  post_title_hline=FALSE,
  alternating=TRUE,
  alternating_scheme="\\rowcolors{2}{gray!25}{white}",
  align="l",
  halign=align,
  title_align="c",
  frname=function(x)add_latex_env(x,"\\bf"),
  row_end="",
  pos="[c]",
  scale=NULL
)paste(capture.output({
  x<-data.frame_format(df)
  if(nrow(x)==0)return(cat(""))
  n0<-data.frame_header(df)
  if(add_rownames)n0<-c("",n0)
  r0<-frname(rownames(x))
  x0<-mapply(function(i)unlist(as.list(x[i,])),1:nrow(x),SIMPLIFY=FALSE)
  if(add_rownames)x0<-mapply(function(rn,r)c(rn,r),r0,x0,SIMPLIFY=FALSE)
  if(!is.null(scale))cat(paste0("\\scalebox{",scale,"}{\n"))
  if(alternating)cat(alternating_scheme,"\n")
  cat("\\begin{tabular}",pos,"{",head(rep(align,length(n0)),length(n0)),"}\n",sep="")
  if(!is.null(title)){
    if(pre_title_hline)cat("\\hline \n")
    cat("\\rowcolor{gray!20}\n")
    cat("\\multicolumn{",length(n0),"}{",title_align,"}{",title,"} \\\\ \n",sep="")
    if(post_title_hline)cat("\\hline \n")
    cat("\\rowcolor{white}\n")
  }
  if(add_header){
    ha<-head(rep(halign,length(n0)),length(n0))
    header<-paste("\\multicolumn{1}{",ha,"}{",n0,"}",collapse=" & ")
    cat(header," \\\\ \n")
    cat("\\hline \n")
  }
  row_sep<-rep(paste0("\\\\ ",row_end,"\n"),length(x0))
  for(i in seq_along(x0)){
    line<-paste(x0[[i]],collapse=" & ")
    cat(line,row_sep[i])
  }
  cat("\\end{tabular} \n")
  if(!is.null(scale))cat(paste0("}\n"))
}),sep="",collapse="\n")

# set data.frame formats to value
set_format<-function(x,format,fcols=1:ncol(x)){
  for(i in fcols){
    x[[i]]<-structure(format=format,x[[i]])
  }
  x
}

dot<-function(col,size="0.5ex",raise="0.2ex"){
  paste0(
    "\\raisebox{",raise,"}{\\tikz \\fill[",col,"] (0,0) circle (",size,");}"
  )
}

cross<-function(col="black",size="3ex",width="0.25ex",raise="-0.2ex"){
  paste0(
    "\\raisebox{",raise,"}{",
    "\\tikz{\\draw(0,0) ",
    "node[cross out,draw=",col,",",
    "line width=",width,",",
    "minimum size=",size,",",
    "inner sep=0pt,",
    "outer sep=0pt",
    "]{};}",
    "}"
  )
}


rectangle<-function(col,raise="0.2ex",size="1.0ex"){
  paste0(
    "\\raisebox{",raise,"}{\\tikz \\fill[",col,"] (0ex,0ex) rectangle (",size,",",size,");}"
  )
}

triangle_up<-function(col,size=1,unit="ex",raise="0.2ex"){
  paste0(
    "\\raisebox{",raise,"}{",
    "\\tikz{",
    "\\filldraw[draw=",col,",fill=",col,"]",
    " (0,0) -- (",paste0(size,unit),",0) -- (",paste0(size/2,unit),",",paste0(size,unit),");",
    "}",
    "}"
  )
}

triangle_dn<-function(col,size=1,unit="ex",raise="0.2ex"){
  paste0(
    "\\raisebox{",raise,"}{",
    "\\tikz{",
    "\\filldraw[draw=",col,",fill=",col,"]",
    "(",paste0(0,unit),",",paste0(size,unit),") -- ",
    "(",paste0(size,unit),",",paste0(size,unit),") -- ",
    "(",paste0(size/2,unit),",",paste0(0,unit),");",
    "}",
    "}"
  )
}

make_tikz_price_action<-function(
  v,
  width=2,
  height=1,
  units="mm",
  line_width=0.1
){
  x<-rescale(seq_along(v),c(0,width))
  y<-rescale(rep(1,length(x)),c(0,height))
  node_dist<-max(diff(x))/6
  node<-mapply(function(s)switch(s,
    down=paste0("node[regular polygon, regular polygon sides=3,rotate=180,fill=red,inner sep=",node_dist,units,"]"),
    flat=paste0("node[cross out,draw=black,line width=",node_dist/2,units,",inner sep=",node_dist,units, "]",collapse=""),
    up=paste0("node[regular polygon, regular polygon sides=3,fill=green,inner sep=",node_dist,units,"]")
  ),c("down","flat","up")[sign(v)+2])
  path<-paste0(
    "(",
    format(x,scientific=FALSE),units,
    ",",
    format(y,scientific=FALSE),units,
    ")",
    node,
    "{}",
    " ",
    collapse=" "
  )
  paste0(
    "\\tikz{",
    "\\path ",path,";",
    "}"
  ) 
}

# sparkline
make_tikz_line<-function(
  v,
  width=2,
  height=1,
  units="mm",
  dot_size=0.25,
  line_width=0.1,
  back_txt=""
){
  x<-round(rescale(seq_along(v),c(0,width)),digits=4)
  y<-round(rescale(v,c(0,height)),digits=4)
  imax<-which.max(y)
  imin<-which.min(y)
  xmid=diff(range(x))/2+min(x)
  ymid=diff(range(y))/2+min(y)
  path<-paste0(
    "(",
    format(x,scientific=FALSE),units,
    ",",
    format(y,scientific=FALSE),units,
    ")",
    collapse="--"
  )
  tikz_txt<-ifelse(
    back_txt=="",
    "",
    paste0(
      "\\draw (",format(xmid,scientific=FALSE),units,",",format(ymid,scientific=FALSE),units,") node{\\bf\\tiny\\textcolor{blue!33}{",back_txt,"}};"
    )
  )
  paste0(
    "\\tikz{",
    tikz_txt,
    "\\fill[green] (",format(x[imax],scientific=FALSE),units,",",format(y[imax],scientific=FALSE),units,") circle(",dot_size,units,");",
    "\\fill[red] (",format(x[imin],scientific=FALSE),units,",",format(y[imin],scientific=FALSE),units,") circle(",dot_size,units,");",
    "\\draw[line width=",line_width,units,"] ",path,";",
    "}"
  )
}

# laout a data.table of content as tikz nodes
tikz_plot_nodes<-function(df,units="mm",node="draw,inner sep=1pt,outer sep=0pt"){
  coordinates<-paste0("(",
                      stri_trim(format(df$x,scientific=FALSE)),units,
                      ",",
                      stri_trim(format(df$y,scientific=FALSE)),units,
                      ")")
  nodes<-paste0(coordinates," ","node[",node,"]","{",df$text,"}",sep="")
  path<-paste("\\path",paste0(nodes,collapse=" "),";")
  paste0("\\tikz{",path,"}\n")
}

# layout content in a matrix, along with a matrix of formatting info
constant<-function(x)function(y)x
tikz_plot_matrix<-function(
  content,
  xscale=c(0,200),
  yscale=c(0,200),
  units="mm",
  node=apply(content,1:2,constant("draw,inner sep=1pt,outer sep=0pt"))
){
  df<-data.table(
    x=rescale(as.vector(col(content)),xscale),
    y=rescale(as.vector(row(content)),yscale),
    text=as.vector(content),
    node=as.vector(node)
  )[text!=""]
  tikz_plot_nodes(df,units=units,node=df$node)
}



table_page<-function(x,n=1,p=10){
  i<-(1:nrow(x))%/%p==n
  if(sum(i)==0)return(x[0,])
  x[i,]
}


color_ramp<-function(x,from="white",to="red"){
  cr<-colorRamp(c(from, to))(rescale(x))
  html_cr<-apply(cr,1,function(x)do.call(rgb,as.list(x/255)))
  html_latex<-gsub("#","",html_cr)
  html_latex
}

sorted_ramp<-function(
  x,
  low="green",
  high="red",
  mid="white"
){
  low_ramp<-colorRamp(c(low,mid))    
  high_ramp<-colorRamp(c(mid,high))
  mx<-median(x)
  i_lo<-which(x<=mx)
  i_hi<-which(x>mx)
  lo_col<-apply(low_ramp(rescale(x[i_lo])),1,function(x)do.call(rgb,as.list(x/255)))
  hi_col<-apply(high_ramp(rescale(x[i_hi])),1,function(x)do.call(rgb,as.list(x/255)))
  c(lo_col,hi_col)[order(c(i_lo,i_hi),decreasing=FALSE)]
}


signed_ramp<-function(x,from="white",to="red"){
  gcr<-color_ramp(abs(x),"white","green")
  rcr<-color_ramp(abs(x),"white","red")
  ifelse(x>0,gcr,rcr)
}

sign_color<-function(t,s){
  paste(ifelse(s<0,"\\textcolor{red}{","\\textcolor{black}{"),t,"}")
}


background_color<-function(t,c,model="[HTML]"){
  paste0("\\colorbox",model,"{",c,"}{",t,"}")
}

cell_color<-function(t,c,model="[HTML]"){
  paste0("\\cellcolor",model,"{",c,"}",t)
}

add_latex_env<-function(x,lenv="\\tt"){
  paste0("{",lenv," ",x,"}")
}

show_decile<-function(x,col="blue!25"){
  if(!is.finite(x))return("")
  x<-max(c(min(c(x,1)),0))
  paste0(
    "\\begin{tikzpicture} ",
    "\\draw[thick] (0,0) -- (1,0) ; ",
    "\\draw[thick] (0,-0.1) rectangle (0,0.1) ;",
    "\\draw[thick] (1,-0.1) rectangle (1,0.1) ;",
    "\\draw[thick] (",round(x,digits=2),",0) [radius=0.10,fill=",col,"] circle; ",
    "\\end{tikzpicture}"
  )
}

make_spark<-function(x,width=20){
  if(length(x)<2)return("")
  sx<-round(rescale(seq_along(x)),digits=2)
  sy<-round(rescale(x),digits=2)
  imax<-which.max(sy)
  imin<-which.min(sy)
  res<-paste0(
    "\\setlength{\\sparklinethickness}{0.75pt}\n",
    "\\setlength{\\sparkdotwidth}{2pt}\n",
    "\\definecolor{sparklinecolor}{rgb}{0.2,0.2,1}",
    "\\begin{sparkline}{",width,"} \n",
    "\\sparkdot ",sx[imax]," ",sy[imax]," green!50 \n",
    "\\sparkdot ",sx[imin]," ",sy[imin]," red!50 \n",
    "\\spark",
    paste(paste(sx,sy),collapse=" "),
    " / \n",
    "\\end{sparkline}\n\n"
  )
  res
}

smooth_spark<-function(x,width=10){
  xs<-ksmooth(seq_along(x),x, "normal", bandwidth = 5)$y
  make_spark(xs[seq(1,length(xs),length.out = 20)],width=width)
}

  
  
tbl<-function(x,sep=" \\\\ ",align="c"){
  paste0(
    "\\begin{tabular}{",align,"} ",
    paste(x,collapse=sep),
    "\\\\ ",
    "\\end{tabular}"
  )
}

htbl<-function(x,sep=" & ",align="c"){
  paste0(
    "\\begin{tabular}{",paste0(rep(align,length(x)),collapse=" "),"}",
    paste(x,collapse=sep),
    "\\\\ ",
    "\\end{tabular}"
  )
}

large_bold<-function(x)paste("\\Tstrut {\\large\\bf",x,"}")

pnl_fmt<-function(this){
  cell_color(add_latex_env(paste0(ifelse(this>0,"\\phantom{-}",""),comma(this/1e3,digits=0)),"\\tt"),signed_ramp(this))
}

n_fmt<-function(this)paste0(ifelse(this>=0,"\\phantom{-}",""),this)

sanitize<-function(this)latexTranslate(this)


#
# ntable used formatting information stored as attributes in
# a data.frame
# applying functions to a data frame loses this info
# the below functions preserve it
#
#do.call(structure,c(list(x$x),attributes(x$x)))
obj_subset<-function(x,o){
  y<-x[o]
  attributes(y)<-attributes(x)
  y
}

data.frame_rbind<-function(x,y){
  z<-rbind(x,y)
  for(i in 1:ncol(z))attributes(z[[i]])<-attributes(x[[i]])
  z
}

data.frame_sort<-function(df,n="USDPnL"){
  new_order<-order(df[[n]],decreasing=TRUE)
  df_list=as.list(df)
  df_plist<-mapply(obj_subset,df_list,MoreArgs=list(o=new_order),SIMPLIFY = FALSE)
  do.call(data.frame,df_plist)
}

data.frame_subset<-function(df,o){
  df_list=as.list(df)
  df_plist<-mapply(obj_subset,df_list,MoreArgs=list(o=o),SIMPLIFY = FALSE)
  do.call(data.frame,df_plist)
}

data.frame_split<-function(df,o){
  ov<-sort(unique(o))
  lapply(ov,function(x)data.frame_subset(df,o==x))
}

fold_character<-function(x,n,colnames=paste0("col",seq_along(res))){
  res<-list()
  while(length(x)>0){
    res<-c(res,list(head(c(head(x,n),rep("",n)),n)))
    x<-tail(x,-n)
  }
  names(res)<-colnames
  do.call(data.table,res)
}

fold_frame<-function(f,title,n){
  k<-as.integer(nrow(f)/n)
  i<-sort((1:nrow(f))%%k)
  sf<-data.frame_split(f,i)
  pfs<-mapply(function(pf,title){
    ntable(pf,title=title)
  },pf=sf,MoreArgs=list(title=title),SIMPLIFY=FALSE)
  pa<-c(pfs,list(sep=" \n \\newpage \n "))
  do.call(paste,pa)
}

multi_page<-function(x,n){
  res<-integer(0)
  p<-1
  while(length(x)>0){
    res<-c(res,rep(p,length(head(x,n))))
    x<-tail(x,-n)
    p<-p+1
  }
  res
}

multi_ntable<-function(
  df,
  n,
  title="",
  fmt=list(add_rownames=FALSE)
){
  o<-multi_page(1:nrow(df),n)
  sf<-data.frame_split(df,o)
  pfs<-mapply(
    function(pf,i){
      args<-fmt
      args$df<-pf
      args$title<-paste0(title," ",i,"/",max(o))
      do.call(ntable,args)
    },
    sf,
    seq_along(sf),
    SIMPLIFY=FALSE
  )
  pa<-c(pfs,list(sep=" \n\n "))
  do.call(paste,pa)
}

# arrange inputs in matrix format. used to layout data frames
make_matrix_from_args<-function(template_matrix,...){
  ij<-cbind(
    rep(1:nrow(template_matrix),times=ncol(template_matrix)),
    rep(1:ncol(template_matrix),each=nrow(template_matrix))
  )
  m<-template_matrix*0
  m[ij]<-do.call(c,list(...))[template_matrix[ij]]
  m
}

# arrange every row of a data frame as a matrix
df_row2matrix<-function(tm,df){
  do.call(mapply,c(list(make_matrix_from_args),df,list(MoreArgs=list(template_matrix=tm),SIMPLIFY=FALSE)))
}

# create latex tabular from matrix
matrix2tabular<-function(m,a="c")paste0(
  "\\begin{tabular}","{",paste0(rep(a,ncol(m)),collapse=" "),"}",
  paste0(apply(m,1,paste0,collapse="&"),collapse="\\\\\n"),
  "\\end{tabular}\n",
  collapse="\n"
)

# create latex tabular from every row of data frame
df_row2tabular<-function(tm,df,align="c"){
  res<-df_row2matrix(tm,df)
  mapply(matrix2tabular,res,MoreArgs = list(a=align))
}


fmt_mat<-function(
  x,
  format=quote(ifelse(is.na(this),"",sign_color(ifelse(this>0,comma(this,digits=2),paste0("(",comma(abs(this),digits=2),")")),this))),
  hdr=quote(add_latex_env(this,"\\bf")),
  digits=1
){
  x<-round(x,digits=digits)
  for(i in names(x)){x[[i]]<-structure(hdr=hdr,format=format,x[[i]])}
  x
}
# adding P&L matrices requires us to re-compute the YTD return
recalc_ytd<-function(x,digits=1){
  x$Year<-NULL
  x$YTD<-NULL
  ytd<-apply(x,1,function(r){
    a<-ifelse(is.na(r),0,r/100)
    round(100*prod(1+a[1:12])-100,digits=digits)
  })
  x$YTD<-ytd
  fmt_mat(x,digits=digits)
}
# P&L matrices pasted from excel
# need re-formatting
fix_mat<-function(x,digits=1){
  yr<-x[[1]]
  x0<-x[tail(names(x),-1)]
  row.names(x0)<-yr
  ytd<-apply(x0,1,function(r){
    a<-ifelse(is.na(r),0,r/100)
    round(100*prod(1+a)-100,digits=digits)
  })
  x0$YTD<-ytd
  fmt_mat(x0,digits=digits)
}

net<-function(x,m=2,i=20,digits=2)round(x*(1-ifelse(x-m/12>0,i/100,0))-m/12,digits=digits)

compound<-function(x,digits=2)round(100*(cumprod(x/100+1)-1),digits=digits)
uncompound<-function(x,digits=2)round(c(x[1],100*(tail(x+100,-1)/head(x+100,-1)-1)),digits=digits)
gross<-function(x,m=2,i=20,digits=2){
  mgmt<-compound(rep(100*((1+m/100)^(1/length(x))-1),length(x)),digits=10)
  round(x*ifelse(x>0,100/(100-i),1)+mgmt,digits=digits)
}
grossup<-function(x,m=2,i=20,digits=2)uncompound(gross(compound(x,digits=5),digits=5),digits=digits)


# gross up a P&L matrix with some management fee assumptions
fee_matrix<-function(x,management=2,incentive=20,digits=1){
  y<-as.matrix(x)
  y[1,10:12]<-0 # HACK: remove last 3 months of 2016
  z<-recalc_ytd(as.data.frame(t(apply(y,1,function(r)grossup(r,management,incentive)))),digits=2)
  z[1,10:12]<-0 # HACK: remove last 3 months of 2016
  res<-as.data.frame(z-y)
  fmt_mat(res,digits=digits)
}

gross_up<-function(x,management=2,incentive=20,digits=1){
  v1<-as.matrix(x[,1:12])
  v2<-t(apply(v1,1,function(r)grossup(r,management,incentive)))
  v2[1,10:12]<-0 # HACK: remove last 3 months of 2016
  v3<-recalc_ytd(as.data.frame(v2),digits=digits)
  fmt_mat(v3,digits=digits)
}


# convert a daily P&L timeseries to a matrix
make_pnl_mat<-function(x,template,digits=1,daily_aggregation=function(x)prod(1+x)-1,yearly_aggregation=daily_aggregation){
  n<-do.call(rbind,strsplit(toupper(as.character(as.Date(names(x),format="%Y-%m-%d"),format="%Y-%b")),"-",fixed=TRUE))
  y<-mapply(function(r)daily_aggregation(r),split(x,apply(n,1,paste,collapse="-")))
  m<-do.call(rbind,strsplit(names(y),"-",fixed=TRUE))
  i<-match(m[,1],rownames(template))
  j<-match(m[,2],toupper(colnames(template)))
  ij<-cbind(i,j)
  ij
  k<-matrix(0,ncol=ncol(template),nrow=nrow(template),dimnames=dimnames(template))
  k[ij]<-y*100
  k1<-as.data.frame(k)
  ytd<-apply(k1,1,function(r){
    a<-ifelse(is.na(r),0,r/100)
    round(100*daily_aggregation(a),digits=digits)
  })
  k1$YTD<-ytd
  fmt_mat(k1,digits=digits)
}


make_strat_matrix_plot<-function(m,title){
  ps<-period_stats(fetch_monthlies(pnl_matrix(m),period))
  paste0(
    "\\scalebox{0.50}{\\begin{tabular}{c c}\n",
    "\\multicolumn{2}{c}{",
    ntable(fmt_mat(as.data.frame(pnl_matrix(m))),title=title),
    "} \\\\\n",
    "\\\\\n",
    ntable(ps[1:4,],add_rownames=FALSE,add_header=FALSE,alternating=FALSE),
    "&",
    ntable(ps[5:7,],add_rownames=FALSE,add_header=FALSE,alternating=FALSE),
    "\\end{tabular}} \n",
    "& \n",
    make_plot(pnl_plot(m),width="5cm",height="4.5cm"),"\n",
    "\\\\ \n"
  )
}



# ICB position summary
icb_position_summary<-function(
  lmv,
  a,
  title="",
  latex_scale_factor="0.5",
  sort_on="last"
){
    position_icb_supersector<-a$static[colnames(lmv),"icb_supersector"]
    icb_supersectors<-sort(unique(position_icb_supersector))
    sector_template_matrix<-Matrix(matrix(0,nrow=nrow(lmv),ncol=length(icb_supersectors),dimnames=list(rownames(lmv),NULL)))  
    
    positioning<-do.call(rbind,mapply(function(s){
      x<-lmv[,position_icb_supersector==s,drop=FALSE]
      if(ncol(x)>0){
        x_sum<-apply(x,1,sum)
      } else {
        x_sum=rep(0,nrow(x))
      }
      data.frame(
        sector=latexTranslate(gsub(" ","",s,fixed=TRUE)),
        last=round(100*tail(x_sum,1),digits=1),
        mean=round(100*mean(x_sum),digits=1),
        max=round(100*max(x_sum),digits=1),
        min=round(100*min(x_sum),digits=1),
        row.names = NULL,
        stringsAsFactors=FALSE
      )
    },setdiff(icb_supersectors,c("","none")),SIMPLIFY=FALSE,USE.NAMES=TRUE))
    if(class(sort_on)=="character"){
      o<-order(positioning[[sort_on]],decreasing=TRUE)
      positioning<-positioning[o,,drop=FALSE]
    }
    x<-with(positioning,ifelse((max-min)>0,(last-min)/(max-min),0))
    positioning$decile<-mapply(show_decile,x)
    position_stats_table<-ntable(positioning,add_rownames=FALSE,add_header=TRUE,title=title)
    position_stats<-paste0("\\scalebox{",latex_scale_factor,"}{",position_stats_table,"}\n",collapse="")
    position_stats
}





