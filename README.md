# citationExtract

The citationExtract tool from govscienceuseR is designed to take PDF documents, feed them through the anystyle.io citation extraction software, and return tagged citation data. Anystyle probabilistically identifies and tags items in a PDF that look like a citation and assign them to citations features like 'author' and 'journal'. This citationExtract packages uses and improves upon this existing software.  

See [citationExtract vignette](http://htmlpreview.github.io/?https://github.com/govscienceuseR/citationExtract/blob/master/vignettes/sgma.html) for an example using California Groundwater Sustainability Plan documents.

## Installation  

Run the following line in your console to install the package from GitHub:  
```
devtools::install_github("govscienceuseR/citationExtract")
```

## Overview  

This package expects users to begin with a folder of PDF documents, from which they would like to extract citations. With those documents they go through the following steps, each as a function.  

### 1. Extract citations from PDFs  

The `citation_extract()` function takes three arguments: doc_dir, ref_dir, layout. The function reads in every PDF in the document directory (doc_dir), and runs them through [anystyle.io](https://anystyle.io/). Anystyle extracts probable citations and exports them to the reference directory (ref_dir) as JSON files. If documents have a two-column layout, you can specify layout = "column", otherwise the default is layout = "none".  

### 2. Compile the citations as one tabular dataset  

The `citation_compile()` function takes one argument: ref_dir. The function reads in and compiles the JSON files in the reference directory (ref_dir), and transforms them to tabular data with the file name as an identifier. The transformations largely focuses on un-listing and un-nesting the data in an effort to provide a clean tabular dataset.     

### 3. Clean and filter the citation data frame    

The `citation_clean()` function takes one argument: dt, which is the data table output by the `citation_compile()` function. The function goes through a series of steps to try to improve Anystyle's citation output. For each column the function unlists the data and filters out unlikely candidates. For instance, if a number listed in the date column does not match any reasonable date format or expectation, it is removed. If a string in the URL column actually resembles a DOI, it is moved to that column. And so on.  



