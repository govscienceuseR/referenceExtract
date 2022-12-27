# referenceExtract

The referenceExtract tool from govscienceuseR is designed to take PDF documents, feed them through the anystyle.io reference extraction software, and return tagged reference data. Anystyle probabilistically identifies and tags items in a PDF that look like a reference and assign them to references features like 'author' and 'journal'. This referenceExtract packages uses and improves upon this existing software.  

See [referenceExtract vignette](http://htmlpreview.github.io/?https://github.com/govscienceuseR/referenceExtract/blob/master/vignettes/sgma.html) for an example using California Groundwater Sustainability Plan documents.

## Installation  

Run the following line in your console to install the package from GitHub:  
```
devtools::install_github("govscienceuseR/referenceExtract")
```

## Overview  

This package expects users to begin with a folder of PDF documents, from which they would like to extract references. With those documents they go through the following steps, each as a function.  

### 1. Extract references from PDFs  

The `reference_extract()` function takes three arguments: doc_dir, ref_dir, layout. The function reads in every PDF in the document directory (doc_dir), and runs them through [anystyle.io](https://anystyle.io/). Anystyle extracts probable references and exports them to the reference directory (ref_dir) as JSON files. If documents have a two-column layout, you can specify layout = "column", otherwise the default is layout = "none".  

### 2. Compile the references as one tabular dataset  

The `reference_compile()` function takes one argument: ref_dir. The function reads in and compiles the JSON files in the reference directory (ref_dir), and transforms them to tabular data with the file name as an identifier. The transformations largely focuses on un-listing and un-nesting the data in an effort to provide a clean tabular dataset.     

### 3. Clean and filter the reference data frame    

The `reference_clean()` function takes one argument: dt, which is the data table output by the `reference_compile()` function. The function goes through a series of steps to try to improve Anystyle's reference output. For each column the function unlists the data and filters out unlikely candidates. For instance, if a number listed in the date column does not match any reasonable date format or expectation, it is removed. If a string in the URL column actually resembles a DOI, it is moved to that column. And so on.  



