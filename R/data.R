#' Agencies from State and Federal lists
#'
#' A dataset containing the names of federal agencies, state agencies, and in some cases, their abbreviations
#'
#' @format A data frame with 7785 rows and 8 variables

#' @source \url{input URL}
"agencies"

#' A sample of candidate references extracted from a PDF file that can be fed into reference_clean()
#'
#' A dataset containing tagged references from a PDF extracted using anystyle.io.
#' @name working_references
#' @docType data
#' @format A data.table object with 1001 rows
NULL

#' A sample of candidate references extracted from a PDF file THAT DOES NOT CURRENTLY WORK IN reference_clean()
#'
#' A dataset containing tagged references from a PDF extracted using anystyle.io.
#' This set throws errors when fed into reference_clean(), and is primarily used for future debugging
#' @name error_references
#' @docType data
#' @format A data.table object with 1001 rows
NULL
