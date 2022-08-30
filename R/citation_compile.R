#' Compiling JSONs from the Anystyle extraction
#'
#' Reads in JSON files of extracted citations and combines them
#'
#' @param ref_dir directory name that holds the file(s) with .JSON extension extracted by Anystyle.io
#'
#' @return data frame
#'
#' @examples dt <- citation_compile('reference_extracts_gsp')
#'
#'
#' @export


citation_compile <- function(ref_dir){
  fl = list.files(ref_dir, full.names = T, pattern = 'json', recursive = T)
  fl_list = pblapply(fl,function(x) fromJSON(x) %>% data.table() %>% .[,File:=x],cl = 4)
  retry = which(sapply(fl_list,class)=='try-error')
  replace_entries = pblapply(fl[retry],function(x) fromJSON(x) %>% data.table() %>% .[,File:=basename(x)])
  fl_list[retry] <- replace_entries
  fl_dt = rbindlist(fl_list,fill = T,use.names=T)

  dt = data.table()
  if(nrow(fl_dt)==0){next}
  fl_dt = fl_dt[nchar(title)<400,]
  dt <- rbind(dt, fl_dt, use.names = T,fill = T)
  return(dt)
}



