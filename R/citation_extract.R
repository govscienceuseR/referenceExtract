#' Anystyle.io extraction
#'
#' Runs PDFs through the [Anystyle API](https://anystyle.io/) and create JSON files for each PDF with the identified citations.
#'
#' @param doc_dir The name of the directory containing PDFs to be evaluated
#' @param ref_dir The name of the directory where citation extractions are to be exported
#' @param layout Specification of whether PDFs should be evaluated as no layout ("none"), column
#'
#' @return JSON
#'
#' @examples
#' citation_extract(doc_dir = 'documents_gsp/', ref_dir = 'reference_extracts_gsp', layout = "none")
#'
#' @export

citation_extract <- function(doc_dir, ref_dir, layout){
  dir.create(ref_dir)
  already_extracted = list.files(ref_dir, full.names = T, recursive = T, pattern = 'json')
  fls = list.files(doc_dir, recursive = T, pattern = 'PDF$|pdf$', full.names = T)
  dirs = dirname(fls)
  json_files <- gsub(paste0('^', doc_dir), ref_dir, fls)
  json_files <- gsub('PDF$|pdf$', 'json', json_files)
  json_dirs <- dirname(json_files)
  sapply(unique(json_dirs[!dir.exists(json_dirs)]), dir.create, recursive = T)
  still_need = !json_files %in% already_extracted

  if(layout == "none"){
    pblapply(seq_along(json_files[still_need]),function(i){
      system(paste('anystyle --overwrite -f json find --no-layout',
                   fls[still_need][i],' ',json_dirs[still_need][i]))
    },cl = 4)
  } else if(layout == "columns"){
    pblapply(seq_along(json_files[still_need]),function(i){
      system(paste('anystyle --overwrite -f json find',
                   fls[still_need][i],' ',json_dirs[still_need][i]))
    },cl = 4)
  }
}

