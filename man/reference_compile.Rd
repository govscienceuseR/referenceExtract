% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reference_compile.R
\name{reference_compile}
\alias{reference_compile}
\title{Compiling JSONs from the Anystyle extraction}
\usage{
reference_compile(ref_dir, cores = 1)
}
\arguments{
\item{ref_dir}{directory name that holds the file(s) with .JSON extension extracted by Anystyle.io}

\item{cores}{how many cores you want to use in mclapply, defaults to 1}
}
\value{
data table
}
\description{
Reads in JSON files of extracted references and combines them inot tabular data
}
\examples{
dt <- reference_compile('reference_extracts_gsp')
}
