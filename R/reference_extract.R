#' Anystyle.io extraction
#'
#' Runs PDFs through the [Anystyle](https://anystyle.io/) and create JSON files for each PDF with the identified references.
#'
#' @param files A vector of PDF file names to be evaluated
#' @param doc_dir A directory containing PDFs
#' @param ref_dir The name of the directory where reference extractions are to be exported
#' @param layout Specification of whether PDFs should be evaluated as no layout ("no_layout"), or layout with two columns ("layout")
#' @param cores how many cores you want to use in pblapply
#' @return JSON files in ref_dir
#'
#' @examples
#' reference_extract(doc_dir = 'documents_gsp/', ref_dir = 'reference_extracts_gsp', layout = "none")
#'
#' @export

reference_extract <- function(doc_dir = NULL,files = NULL, ref_dir, layout = "layout",cores = 1){
  if(!is.null(doc_dir)&!is.null(files)){print('specify a directory or files, but not both')}
  if(!exists(ref_dir)){dir.create(ref_dir)}
  already_extracted = list.files(ref_dir, full.names = T, recursive = T, pattern = 'json')
  if(!is.null(doc_dir)){
  fls = list.files(doc_dir, recursive = T, pattern = 'PDF$|pdf$', full.names = T)
    }
  if(!is.null(files)){
    fls <- files
    }
  #dirs = dirname(fls)
  base_fls <- basename(fls)
  json_files <- paste(ref_dir,base_fls,sep = '/')
  json_files <- gsub('PDF$|pdf$', 'json', json_files)
  json_dirs <- dirname(json_files)
  sapply(unique(json_dirs[!dir.exists(json_dirs)]), dir.create, recursive = T)
  still_need = !json_files %in% already_extracted

  if(layout == "no_layout"){
    pblapply(seq_along(json_files[still_need]),function(i){
      system(paste('anystyle --overwrite -f json find --no-layout',
                   fls[still_need][i],' ',json_dirs[still_need][i]))
    },cl = cores)
  } else if(layout == "layout"){
    pblapply(seq_along(json_files[still_need]),function(i){
      system(paste('anystyle --overwrite -f json find',
                   fls[still_need][i],' ',json_dirs[still_need][i]))
    },cl = cores)
  }
}

