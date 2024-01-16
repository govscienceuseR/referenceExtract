#' Compiling JSONs from the Anystyle extraction
#'
#' Reads in JSON files of extracted references and combines them inot tabular data
#'
#' @param ref_dir directory name that holds the file(s) with .JSON extension extracted by Anystyle.io
#' @param cores how many cores you want to use in mclapply, defaults to 1
#' @return data table
#'
#' @examples dt <- reference_compile('reference_extracts_gsp')
#' @import data.table
#' @import magrittr
#' @importFrom parallel mclapply
#' @importFrom jsonlite fromJSON
#' @export

reference_compile <- function(ref_dir,cores = 1){
  fl = list.files(ref_dir, full.names = T, pattern = 'json', recursive = T)
  l = length(fl)
  if(l > 100){print(paste('compiling',l,'files, this will not be instantaneous'))}
  fl_list = mclapply(fl,function(x) fromJSON(x) %>% data.table() %>% .[,File:=x],mc.cores = cores,mc.preschedule = T,mc.cleanup = T)
  if(any(sapply(fl_list,class)=='try-error')){
    retry = which(sapply(fl_list,class)=='try-error')
    replace_entries = mclapply(fl[retry],function(x) fromJSON(x) %>%
                                 data.table() %>% .[,File:=basename(x)],mc.cores = cores/2,mc.preschedule = T,mc.cleanup = T)
    fl_list[retry] <- replace_entries
  }
  fl_dt = rbindlist(fl_list,fill = T,use.names=T)

  dt = data.table()
  if(nrow(fl_dt)==0){next}
  fl_dt = fl_dt[nchar(title)<400,]
  dt <- rbind(dt, fl_dt, use.names = T,fill = T)
  return(dt)
}



