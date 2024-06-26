#' Take a list with NULL entries and replace with NA value placeholders
null2NA <- function(l){ifelse(sapply(l,is.null),NA,l)}

index.lengths <- function(x, y){
  data.frame(
    lengths = ifelse(is.na(x), 0, lengths(x)),
    ID = y)
}

date.formats <- c("\\d{1,2}-\\d{1,2}-\\d{4}", # XX-XX-XXXX
                  "\\d{1,2}-\\d{1,2}-\\s?\\d{2}", #XX-XX- XX
                  "\\d{4}-\\d{1,2}-\\d{1,2}", # XXXX-XX-XX
                  "\\d{4}-\\d{1,2}", # XXXX-XX
                  "\\d{1,2}\\/\\d{1,2}\\/\\d{4}", # XX/XX/XXXX
                  "\\d{1,2}\\/\\d{1,2}\\/\\s?\\d{2}", # XX/XX/ XX
                  "\\d{1,2}\\/\\s?\\d{4}", # XX/ XXXX
                  "\\d{4}\\/\\d{1,2}\\/\\d{1,2}", # XXXX/XX/XX
                  "\\d{4}\\/\\d{1,2}", # XXXX/XX
                  "(?<=\\s)\\d{4}$", # Anything before with 4 digits at the end
                  "^\\d{4}$") # Just 4 digits
date.formats <- paste(date.formats, collapse = "|")

extract_date_formats <- function(x){
  trimws(str_extract(x, date.formats))
}

rm_yrs <- function(x){
  ifelse(x < 1800 |
           x > year(Sys.Date()), NA,
         x)
}

assign_year <- function(x) {
  ifelse(str_detect(x,"^\\d{1,2}-\\d{1,2}-\\d{4}$"),
         year(as.Date(x, format = "%m-%d-%Y")),
  ifelse(str_detect(x,"^\\d{4}-\\d{1,2}-\\d{1,2}$"),
         year(as.Date(x, format = "%Y-%m-%d")),
  ifelse(str_detect(x,"^\\d{1,2}-\\d{1,2}-\\s?\\d{2}$"),
         year(as.Date(x, format = "%m-%d-%Y")),
  ifelse(str_detect(x,"^\\d{4}-\\d{1,2}$"),
         # 2 elements do not make a date, need to paste to make a day
         year(as.Date(paste(x,1,sep="-"), format = "%Y-%m-%d")),
  ifelse(str_detect(x,"^\\d{1,2}\\/\\d{1,2}\\/\\d{4}$"),
         year(as.Date(x, format = "%m/%d/%Y")),
  ifelse(str_detect(x,"^\\d{4}\\/\\d{1,2}\\/\\d{1,2}$"),
         year(as.Date(x, format = "%Y/%m/%d")),
  ifelse(str_detect(x,"^\\d{1,2}\\/\\d{1,2}\\/\\s?\\d{2}$"),
         year(as.Date(x, format = "%m/%d/%Y")),
  ifelse(str_detect(x,"^\\d{1,2}\\/\\d{4}$"),
         year(as.Date(paste(x,1,sep="/"), format = "%m/%Y/%d")),
  ifelse(str_detect(x,"^\\d{4}$"),
         year(as.Date(x, format = "%Y")), "no pattern")))))))))
}

doi_url_pattern <- c("doi\\..*|doi:")
doi_pattern <- c("10\\.\\d{1,5}.*")

extract_doi_url <- function(x){
  ifelse(str_detect(x, doi_url_pattern), str_extract(x, doi_pattern), NA)
}

rm_url_pattern <- paste(c("^[Aa]t:?\\s?",
                          "^[Aa]ccessed:?\\s?",
                          "^[Ff]rom:?\\s?",
                          "^[Ww]ebsite:?\\s?",
                          "^https://$"),
                        collapse = "|")
rm_url <- function(x){
  ifelse(str_detect(x, rm_url_pattern), str_replace(x, rm_url_pattern, NA_character_), x)
}

url_pattern <- paste(c("^https?:\\/\\/.*",
                       "^ftp:\\/\\/\\.",
                       "^www\\."), collapse = "|")

keep_urls <- function(x){
  ifelse(str_detect(x, url_pattern), x, NA_character_)
  ifelse(str_detect(x, doi_url_pattern), NA_character_, x)
}

#agencies <- data.table::fread("~/Box/truckee/data/eia_data/agency_list.csv", fill = T)
#usethis::use_data(agencies)
data("agencies", envir=environment())

org.words <- c("Administration", "Agency", "Association", "Associates", "Authority",  "Board", "Bureau", "Center", "Datacenter", "^Consult[a-z]+$",  "Commission", "Council", "County",  "Department", "District", "Foundation", "Government[s]*", "LLC", "Group", "Geological Survey", "Survey","Application","U\\.S\\.","Laboratory", "Service", "Society", "Univeristy", "\\bUS\\b",'Letter','County','Coordinating',"Collaborate")
org.words <- paste(org.words, collapse = "|")
agency.pattern <- paste(agencies$Agency, collapse = "\\b|\\b")

extract_agency_titles <- function(x){
  ifelse(str_detect(x, org.words) | str_detect(x, agency.pattern),
         str_extract(x, doi_pattern), x)
}

rm_title_pattern <- paste(c("[Pp]ersonal [Cc]ommunication:?",
                            "^\\d*$"), collapse = "|")

rm_titles <- function(x){
  ifelse(str_detect(x, org.words) | str_detect(x, agency.pattern) |
           str_detect(x, rm_title_pattern) | str_detect(x, rm.row) |
           nchar(x) < 10 | nchar(x) > 250, NA, x)
}

rm_row <- function(x){
  ifelse(str_detect(x, rm.row) | nchar(x) > 250, NA, x)
}

extract_doi <- function(x){
  ifelse(str_detect(x, doi_pattern), str_extract(x, doi_pattern), NA)
}

unlist_authors <- function(x){
  paste(unlist(x), collapse='; ')
}
unlist_others <- function(x){
  paste(unlist(x), collapse=' ')
}

match_doi1 <- function(dt){
  doiurl <- ifelse(is.na(dt[,"doiurl1"]), "", dt[,"doiurl1"])
  doi <- ifelse(is.na(dt[,"doi1"]), "", dt[,"doi1"])
  dt[,"doiurl1"] <- ifelse(doi == doiurl, NA_character_, dt[,"doiurl1"])
  dt[,"doi1"] <- ifelse(doi != doiurl & doiurl != "", dt[,"doiurl1"], dt[,"doi1"])
  return(df)
}

collapse_column <- function(x, column_name){
  string.dt <- data.table(matrix(nrow = nrow(x), ncol = 2))
  cols <- grep(paste0(column_name, "\\d+"), colnames(x))
  for(a in 1:nrow(x)){
    if(x$lengths[a] > length(cols)){ # This should be right, for when some things get deleted via filtering
      x$lengths[a] <- length(cols)
    }
    new.cols <- 2:cols[x$lengths[a]]
    indices.l <- c()
    for(b in 1:length(new.cols)){
      yr <- x[[a, new.cols[b]]]
      if(length(yr)>0){
        indices.l[b] <- yr
      } else {indices.l[b] <- NA}
    }
    string.dt$V1[a] <- list(c(indices.l))
    string.dt$V2[a] <- x[[a,1]]
  }
  assign("string.dt", string.dt, envir = .GlobalEnv)
}

encoding_change <- function(x){
  iconv(x, from = "latin1", to = "ASCII", sub = "")
}

separate_author2 <- function(dt){
  possible_vars <- c('family','given','particle','literal','suffix','others')
  author_sub <- dt[!sapply(dt$author,function(x) identical(x,list())),.(author,ID)]
  author_sub <- author_sub[!is.na(author),]
  author_stack <- rbindlist(author_sub$author,fill = T)
  cols_to_add <- possible_vars[!possible_vars  %in% colnames(author_stack)]
  for(add in cols_to_add){
    author_stack[[add]]<-NA_character_
  }
  author_stack$ID <- rep(author_sub$ID,sapply(author_sub$author,nrow))

  author_stack$others[is.na(author_stack$others)] <- NA
  author_stack$author.clean <- NA
  org_detect <- str_detect(author_stack$family,org.words)|str_detect(author_stack$given,org.words)

  acols <- colnames(author_stack)[1:{which(colnames(author_stack)=='ID')-1}]
  notna_stack <- data.table(!is.na(author_stack[,..acols]))
  clean_paste <- {!org_detect} & rowSums(notna_stack[,.(family,given)]) == 2 & rowSums(notna_stack[,acols[!acols %in% c('family','given')],with = F])==0
  author_stack$author.clean[clean_paste] <- paste(author_stack$family[clean_paste], author_stack$given[clean_paste], sep = ", ")

  # If you have suffix but one or two others, take just family and given
  clean_paste <- {!org_detect} & rowSums(notna_stack[,.(family,given,suffix)]) == 3 & rowSums(notna_stack[,acols[!acols %in% c('family','given','suffix')],with = F])<=1
  author_stack$author.clean[clean_paste] <- paste(author_stack$family[clean_paste], author_stack$given[clean_paste], sep = ", ")

  # If there is just suffix, keep it
  org_detect <- str_detect(author_stack$family,org.words)|str_detect(author_stack$suffix,org.words)
  clean_paste <- {!org_detect} & rowSums(notna_stack[,.(family,suffix)]) == 2 & rowSums(notna_stack[,acols[!acols %in% c('family','suffix')],with = F])==0
  author_stack$author.clean[clean_paste] <- paste(author_stack$family[clean_paste], author_stack$suffix[clean_paste], sep = ", ")

  # Has particle (and maybe suffix, but ignoring it)
  org_detect <- str_detect(author_stack$family,org.words)|str_detect(author_stack$given,org.words)
  clean_paste <- rowSums(notna_stack[,.(family,given,particle)]) == 3 & rowSums(notna_stack[,acols[!acols %in% c('family','given','particle')],with = F])<=1
  author_stack$author.clean[clean_paste] <- paste(author_stack$given[clean_paste], author_stack$particle[clean_paste], author_stack$family[clean_paste])

  # Straightforward agency
  clean_paste <- {org_detect} & rowSums(notna_stack[,.(family,given)]) == 2 & rowSums(notna_stack[,acols[!acols %in% c('family','given')],with = F])==0
  author_stack$author.clean[clean_paste] <- paste(author_stack$given[clean_paste],  author_stack$family[clean_paste])

  # Others, want to get rid of it, but assuming that it has family and given
  clean_paste <- rowSums(notna_stack[,.(family,given,others)]) == 3 & rowSums(notna_stack[,acols[!acols %in% c('family','given','others')],with = F])<=1
  author_stack$author.clean[clean_paste] <- paste(author_stack$family[clean_paste],author_stack$given[clean_paste], sep = ", ")

  clean_paste <- rowSums(notna_stack[,.(family,given,literal)]) == 3 & rowSums(notna_stack[,acols[!acols %in% c('family','given','literal')],with = F])==0
  author_stack$author.clean[clean_paste] <- paste(author_stack$family[clean_paste],author_stack$given[clean_paste], sep = ", ")

  clean_paste <- rowSums(notna_stack[,.(family,given,literal,particle)]) == 4 & rowSums(notna_stack[,acols[!acols %in% c('family','given','literal','particle')],with = F])==0
  author_stack$author.clean[clean_paste] <- paste(author_stack$given[clean_paste], author_stack$particle[clean_paste], author_stack$family[clean_paste])

  clean_paste <- rowSums(notna_stack[,.(family,particle)]) == 2 & rowSums(notna_stack[,acols[!acols %in% c('family','particle')],with = F])==0
  author_stack$author.clean[clean_paste] <- paste(author_stack$particle[clean_paste], author_stack$family[clean_paste])

  # if just given + others or literal, just set as given
  clean_paste <- rowSums(notna_stack[,.(given,others)]) == 2 & rowSums(notna_stack[,acols[!acols %in% c('given','others')],with = F])==0
  author_stack$author.clean[clean_paste] <-  author_stack$given[clean_paste]
  clean_paste <- rowSums(notna_stack[,.(given,literal)]) == 2 & rowSums(notna_stack[,acols[!acols %in% c('given','literal')],with = F])==0
  author_stack$author.clean[clean_paste] <-  author_stack$given[clean_paste]

  # if everything, just family and given
  clean_paste <- rowSums(notna_stack[,acols,with = F])==5
  author_stack$author.clean[clean_paste] <-  paste(author_stack$family[clean_paste],author_stack$given[clean_paste], sep = ", ")

  # if one thing, just that thing
  clean_paste <- rowSums(notna_stack[,acols,with = F])==1
  author_stack$author.clean[clean_paste] <-  paste(author_stack$family[clean_paste],author_stack$given[clean_paste], sep = ", ")

  which.var <- colnames(author_stack)[unlist(apply(author_stack[clean_paste,],1,function(x) min(which(!is.na(x))),simplify = F))]
  author_stack$author.clean[clean_paste] <- sapply(seq_along(which.var),function(x) author_stack[clean_paste,][x,][[which.var[x]]])
  author_stack$author.clean[is.na(author_stack$author.clean)]<- "check"
  # This is because one of the authors was named "TRUE", I think if T is name
  author_stack$author.clean <- ifelse(author_stack$author.clean  == T, "", author_stack$author.clean)
  return(author_stack)
}



separate_author <- function(x, y){
  author <- data.frame(x)
  #author <- lapply(author, function(x) ifelse(is.na(x), "", x))
  if(ncol(author) == 0){
    author <- author
  } else {
    author <- data.frame(lapply(author, function(x) ifelse(is.na(x), "", x)))
  }
  #author[,1:ncol(author)] <- lapply(author[,1:ncol(author)],
  #                                  function(x) ifelse(is.na(x), "", x))
  # Make the matrix into a dataframe

  if (ncol(author) == 0){
    author.clean <- NA_character_
    # Straightforward
  } else if (ncol(author) == 2 &&
             colnames(author)[1] == "family" &&
             colnames(author)[2] == "given" &&
             str_detect(author$family, org.words) == F &&
             str_detect(author$given, org.words) == F){
    author.clean <- paste(author$family, author$given, sep = ", ")
    # Straightforward reverse
  } else if (ncol(author) == 2 &&
             colnames(author)[1] == "given" &&
             colnames(author)[2] == "family" &&
             str_detect(author$family, org.words) == F &&
             str_detect(author$given, org.words) == F){
    author.clean <- paste(author$family, author$given, sep = ", ")
    # If there is just suffix, keep it
  } else if (ncol(author) == 2 &&
             colnames(author)[1] == "family" &&
             colnames(author)[2] == "suffix" &&
             str_detect(author$family, org.words) == F &&
             str_detect(author$suffix, org.words) == F){
    author.clean <- paste(author$family, author$suffix, sep = ", ")
    # If you have suffix but two others, take just family and given (I might need to specify the column names)
  } else if(ncol(author) == 3 && (colnames(author)[2] == "suffix" |
                                  colnames(author)[3] == "suffix")){
    author.clean <- paste(author$family, author$given, sep = ", ")
    # If you have suffix but two others, take just family and given (I might need to specify the column names)
  } else if(ncol(author) == 4 && (colnames(author)[2] == "suffix" |
                                  colnames(author)[3] == "suffix" |
                                  colnames(author)[4] == "suffix")){
    author.clean <- paste(author$family, author$given, sep = ", ")


    # Has particle (and maybe suffix, but ignoring it)
  } else if(ncol(author) == 3 &&
            (colnames(author)[2] == "particle" |
             colnames(author)[3] == "particle")){
    author.clean <- paste(author$given, author$particle, author$family)
    # Has particle (and maybe suffix, but ignoring it)
  } else if(ncol(author) == 4 &&
            (colnames(author)[2] == "particle" |
             colnames(author)[3] == "particle" |
             colnames(author)[4] == "particle")){
    author.clean <- paste(author$given, author$particle, author$family)
    # Straightforward agency
  } else if(ncol(author) == 2 &&
            colnames(author)[1] == "family" &&
            colnames(author)[2] == "given" &&
            (str_detect(author$family, org.words) |
             str_detect(author$given, org.words))){
    author.clean <- paste(author$given, author$family)
  } else if(ncol(author) == 2 &&
            colnames(author)[1] == "given" &&
            colnames(author)[2] == "family" &&
            (str_detect(author$family, org.words) |
             str_detect(author$given, org.words))){
    author.clean <- paste(author$given, author$family)
    # Others, want to get rid of it, but assuming that it has family and given
  } else if(ncol(author) == 3  &&
            (colnames(author)[2] == "others" |
             colnames(author)[3] == "others")){
    author.clean <- paste(author$family, author$given, sep = ", ")
    # Others, want to get rid of it, but assuming that it has family and given
  } else if(ncol(author) == 4  &&
            (colnames(author)[2] == "others" |
             colnames(author)[3] == "others" |
             colnames(author)[4] == "others")){
    author.clean <- paste(author$family, author$given, sep = ", ")
  } else if(ncol(author) == 3 &&
            (colnames(author)[1] == "literal" |
             colnames(author)[3] == "literal" )){
    author.clean <- paste(author$family, author$given, sep = ", ")
  } else if(ncol(author) == 4 &&
            (colnames(author)[3] == "particle" |
             colnames(author)[4] == "particle") &&
            (colnames(author)[1] == "literal" |
             colnames(author)[3] == "literal" )){
    author.clean <- paste(author$given, author$particle, author$family)
  } else if(ncol(author) == 2 &&
            colnames(author)[1] == "family" &&
            colnames(author)[2] == "particle"){
    author.clean <- paste(author$particle, author$family)
  } else if(ncol(author) == 2 &&
            colnames(author)[1] == "given" &&
            colnames(author)[2] == "others"){
    author.clean <- author$given
  } else if(ncol(author) == 2 &&
            colnames(author)[1] == "literal" &&
            colnames(author)[2] == "given"){
    author.clean <- author$given
  } else if(ncol(author) == 2 &&
            colnames(author)[1] == "given" &&
            colnames(author)[2] == "literal"){
    author.clean <- author$given
  } else if(ncol(author) == 5){
    author.clean <- paste(author$family, author$given, sep = ", ")
  } else if (ncol(author) == 1 &&
             nrow(author) == 1){
    author.clean <- author[,1]
  } else if (ncol(author) == 1 &&
             nrow(author) > 1){
    author.clean <- paste(author[,1], collapse = " & ")
  } else {
    author.clean <- "check"
  }
  # This is because one of the authors was named "TRUE", I think if T is name
  author.clean <- ifelse(author.clean == T, "", author.clean)
  author <- data.frame(
    "author" = author.clean,
    "ID" = y
  )
  return(author)
}

rm.word <- c( '^[a-z]\\.\\s',
              "^,\\s", # Dealing with if NAs were in the start
              "^,$", # If it pasted 2 empties
              "_{2,6}", # If there are multiple underscores
              "^\\/\\s?", #Revmoe the forward slash and space to start
              "^[:punct:]+$", # only punctuation
              "[~><■✔►]+", # only punctuation
              "^:\\s",
              "[Aa] [Rr]eport by (the )?",
              "[Aa] [Rr]eport for (the )?",
              "[Aa] [Rr]eport of (the )?",
              "[Aa] [Rr]eport to (the )?",
              "[Aa] [Rr]eport prepared by (the )?",
              "Accessed,?",
              "^Bull$|^Bulletin$",
              "Internet [Ww]ebsite",
              "^Fiscal [Yy]ears?$",
              "^Final [Dd]ecision",
              "Prepared by",
              "Prepared for (the )?",
              "Final [Rr]eport to (the )?",
              "^[Mm][Oo][Nn][Dd][Aa][Yy]$",
              "^[Tt][Uu][Ee][Ss][Dd][Aa][Yy]$",
              "^[Ww][Ee][Dd][Nn][Ee][Ss][Dd][Aa][Yy]$",
              "^[Tt][Hh][Uu][Rr][Ss][Dd][Aa][Yy]$",
              "^[Ff][Rr][Ii][Dd][Aa][Yy]$",
              "^[Ss][Aa][Tt][Uu][Rr][Dd][Aa][Yy]$",
              "^[Ss][Uu][Nn][Dd][Aa][Yy]$",
              "^[Jj][Aa][Nn][Uu][Aa][Rr][Yy]$",
              "^[Ff][Ee][Bb][Rr][Uu][Aa][Rr][Yy]$",
              "^[Mm][Aa][Rr][Cc][Hh]$",
              "^[Aa][Pp][Rr][Ii][Ll]$",
              "^[Mm][Aa][Yy]$",
              "^[Jj][Uu][Nn][Ee]$",
              "^[Jj][Uu][Ll][Yy]$",
              "^[Aa][Uu][Gg][Uu][Ss][Tt]$",
              "^[Ss][Ee][Pp][Tt][Ee][Mm][Bb][Ee][Rr]$",
              "^[Oo][Cc][Tt][Oo][Bb][Ee][Rr]$",
              "^[Nn][Oo][Vv][Ee][Mm][Bb][Ee][Rr]$",
              "^[Dd][Ee][Cc][Ee][Mm][Bb][Ee][Rr]$")
rm.word <- paste(rm.word, collapse="|")

rm_word <- function(x){
  str_remove_all(base::trimws(x), rm.word)
}

rpl_na <- function(x){
  ifelse(x == "NA" | x == "" | x == 'Na', NA, x)
}

rm.auth.word <- c( '^[a-z]\\.\\s', # Many authors begind with a. b. or c. etc as if its a list.
                   "^,\\s", # Dealing with if NAs were in the start
                   "^,$", # If it pasted 2 empties
                   "^[:punct:]+$",
                   "[~><■✔►]+",
                   "^+\\.?",
                   "^&\\s",
                   "•\\s",
                   "^Appendix.{0,3}$",
                   "^[Mm][Oo][Nn][Dd][Aa][Yy],?\\s?",
                   "^[Tt][Uu][Ee][Ss][Dd][Aa][Yy],?\\s?",
                   "^[Ww][Ee][Dd][Nn][Ee][Ss][Dd][Aa][Yy],?\\s?",
                   "^[Tt][Hh][Uu][Rr][Ss][Dd][Aa][Yy],?\\s?",
                   "^[Ff][Rr][Ii][Dd][Aa][Yy],?\\s?",
                   "^[Ss][Aa][Tt][Uu][Rr][Dd][Aa][Yy],?\\s?",
                   "^[Ss][Uu][Nn][Dd][Aa][Yy],?\\s?",
                   "^[Jj][Aa][Nn][Uu][Aa][Rr][Yy],?\\s?",
                   "^[Ff][Ee][Bb][Rr][Uu][Aa][Rr][Yy],?\\s?",
                   "^[Mm][Aa][Rr][Cc][Hh],?\\s?",
                   "^[Aa][Pp][Rr][Ii][Ll],?\\s?",
                   "^[Mm][Aa][Yy],?\\s?",
                   "^[Jj][Uu][Nn][Ee],?\\s?",
                   "^[Jj][Uu][Ll][Yy],?\\s?",
                   "^[Aa][Uu][Gg][Uu][Ss][Tt],?\\s?",
                   "^[Ss][Ee][Pp][Tt][Ee][Mm][Bb][Ee][Rr],?\\s?",
                   "^[Oo][Cc][Tt][Oo][Bb][Ee][Rr],?\\s?",
                   "^[Nn][Oo][Vv][Ee][Mm][Bb][Ee][Rr],?\\s?",
                   "^[Dd][Ee][Cc][Ee][Mm][Bb][Ee][Rr],?\\s?")
rm.auth.word <- paste(rm.auth.word, collapse="|")

rm.row <- c( "^[0-9]+$", # only digits
             "^_\\d", # Starting with an underscore then number indicates it is probably a file
             "^_[A-Za-z]", # Same does with lower case letter
             "^_{2,}",
             "^\\d{1,2}\\:\\d{2}", # time
             "\\d{1,2}\\:\\d{2}\\s?[Aa].?[Mm].?", # time am
             "\\d{1,2}\\:\\d{2}\\s?[Pp].?[Mm].?", # time pm
             "^\\d{1,2}\\/\\d{1,2}\\/\\d{2,4}",
             "\\bP\\.?O\\.? Box", # address
             "Box, P\\.?O\\.?", # address
             "^Address",
             "Ave\\s@\\s|Rd\\s@\\s|Dr\\s@\\s|Blvd\\s@\\s|Ln\\s@\\s", #address
             "\\d{1,2}\\syears\\sof\\sexperience", #resume
             "^Experience\\:", # resume
             "\\bB\\.?Sc\\.?\\b",
             "\\bP\\.?[Hh]\\.?[Dd]\\.?", # Various honorariums
             "^[[:punct:]]$", #punctuation only
             "^Contact\\sfor\\sMore", # Emails
             "^Attachment$",
             '^C:\\\\', # website of sorts
             '[Bb]oard [Mm]eeting',
             "^Date$",
             "^Ibid$",
             "^Note[sd]?$",
             "^[Nn]otes, [Mm]eeting$",
             "^[Nn]ote[sd]?, [Cc]omment$",
             "^Comments?$",
             "^Regulations?$",
             "^Rehabilitation$",
             "^Section\\.{0,3}",
             "^[Mm][Oo][Nn][Dd][Aa][Yy],?\\s?",
             "^[Tt][Uu][Ee][Ss][Dd][Aa][Yy],?\\s?",
             "^[Ww][Ee][Dd][Nn][Ee][Ss][Dd][Aa][Yy],?\\s?",
             "^[Tt][Hh][Uu][Rr][Ss][Dd][Aa][Yy],?\\s?",
             "^[Ff][Rr][Ii][Dd][Aa][Yy],?\\s?",
             "^[Ss][Aa][Tt][Uu][Rr][Dd][Aa][Yy],?\\s?",
             "^[Ss][Uu][Nn][Dd][Aa][Yy],?\\s?")
rm.row <- paste(rm.row, collapse="|")


columns4fx <- c("date", "url", "title", "container", "publisher",
                           "doi", "author")
reassign_value <- function(dt, i){
  df <- as.data.frame(dt)
  if(columns4fx[i] == "date"){
    df$date <- ifelse(df$lengths == 0, NA,
                      ifelse(df$lengths == 1,
                             df[,paste0(columns4fx[i], "1")],
                             df[,columns4fx[i]]))
  } else if(columns4fx[i] == "url"){
    df$url <- ifelse(df$lengths == 0, NA,
                     ifelse(df$lengths == 1,
                            df[,paste0(columns4fx[i], "1")],
                            df[,columns4fx[i]]))
  } else if(columns4fx[i] == "title"){
    df$title <- ifelse(df$lengths == 0, NA,
                       ifelse(df$lengths == 1,
                              df[,paste0(columns4fx[i], "1")],
                              df[,columns4fx[i]]))
  } else if(columns4fx[i] == "container"){
    df$container <- ifelse(df$lengths == 0, NA,
                           ifelse(df$lengths == 1,
                                  df[,paste0(columns4fx[i], "1")],
                                  df[,columns4fx[i]]))
  } else if(columns4fx[i] == "publisher"){
    df$publisher <- ifelse(df$lengths == 0, NA,
                           ifelse(df$lengths == 1,
                                  df[,paste0(columns4fx[i], "1")],
                                  df[,columns4fx[i]]))
  } else if(columns4fx[i] == "doi"){
    df$doi <- ifelse(df$lengths == 0, NA,
                     ifelse(df$lengths == 1,
                            df[,paste0(columns4fx[i], "1")],
                            df[,columns4fx[i]]))
  } else if(columns4fx[i] == "author"){
    df$author <- ifelse(df$lengths == 0, NA,
                        ifelse(df$lengths == 1,
                               df[,paste0(columns4fx[i], "1")],
                               df[,columns4fx[i]]))
  }
  return(df)
}


reassign_value2 <- function(dt, colname, min_col){
  df <- as.data.frame(dt)
  if(colname == "date"){
    df$date <- ifelse(df$lengths == 0, NA,
                      ifelse(df$lengths == 1,
                             df[,min_col],
                             df[,colname]))
  } else if(colname == "url"){
    df$url <- ifelse(df$lengths == 0, NA,
                     ifelse(df$lengths == 1,
                            df[,min_col],
                            df[,colname]))
  } else if(colname == "title"){
    df$title <- ifelse(df$lengths == 0, NA,
                       ifelse(df$lengths == 1,
                              df[,min_col],
                              df[,colname]))
  } else if(colname == "container"){
    df$container <- ifelse(df$lengths == 0, NA,
                           ifelse(df$lengths == 1,
                                  df[,min_col],
                                  df[,colname]))
  } else if(colname == "publisher"){
    df$publisher <- ifelse(df$lengths == 0, NA,
                           ifelse(df$lengths == 1,
                                  df[,min_col],
                                  df[,colname]))
  } else if(colname == "doi"){
    df$doi <- ifelse(df$lengths == 0, NA,
                     ifelse(df$lengths == 1,
                            df[,min_col],
                            df[,colname]))
  } else if(colname == "author"){
    # CHANGING THIS ti redefine min
    df$author <- ifelse(df$lengths == 0, NA,
                        ifelse(df$lengths == 1,
                               df[,min_col],
                               df[,colname]))

  }
  return(df)
}


matching_fx <- function(jl, pl, tl, yl, ul, dl, z) {
  data.frame(
    nested = ifelse(tl > 1 &
                      (jl == 0) &
                      (pl == 0) &
                      (tl == yl) &
                      (tl == ul) &
                      (tl == dl), "tyud_even_unn",
              ifelse(tl > 1 &
                       (jl == 0) &
                       (pl == 0) &
                       (tl == yl) &
                       (ul == 0) &
                       (tl == dl), "tyd_even_unn",
              ifelse(tl > 1 &
                       (jl == 0) &
                       (pl == 0) &
                       (yl == 0) &
                       (tl == ul) &
                       (tl == dl), "tud_even_unn",
               ifelse(tl > 1 &
                        (jl == 0) &
                        (pl == 0) &
                        (yl == 0) &
                        (ul == 0) &
                        (tl == dl), "td_even_unn",
                ifelse(tl > 1 &
                         (jl == 0) &
                         (pl == 0) &
                         (yl == 0) &
                         (tl == ul) &
                         (dl == 0), "tu_even_unn",
                 ifelse(tl > 1 &
                          (jl == 0) &
                          (pl == 0) &
                          (tl == yl) &
                          (ul == 0) &
                          (dl == 0), "ty_even_unn",
                  ifelse(tl > 1 &
                           (jl == 0) &
                           (pl == 0) &
                           (yl == 0) &
                           (ul == 0) &
                           (dl == 0), "t_even_unn",
                 #ifelse(yl > 1 &
                 #         (jl %in% c(0,1)) &
                 #         (pl %in% c(0,1)) &
                 #         (tl %in% c(0,1)) &
                 #         (ul %in% c(0,1)) &
                 #         (dl %in% c(0,1)), "y_uneven",
                 #ifelse(tl > 1 &
                 #         (jl %in% c(0,1)) &
                 #         (pl %in% c(0,1)) &
                 #         (yl %in% c(0,1)) &
                 #         (ul %in% c(0,1)) &
                 #         (dl %in% c(0,1)), "t_uneven",
                 #ifelse(pl > 1 &
                 #         (jl %in% c(0,1)) &
                 #         (tl %in% c(0,1)) &
                 #         (yl %in% c(0,1)) &
                 #         (ul %in% c(0,1)) &
                 #         (dl %in% c(0,1)), "p_uneven",
                 #ifelse(jl > 1 &
                 #         (pl %in% c(0,1)) &
                 #         (tl %in% c(0,1)) &
                 #         (yl %in% c(0,1)) &
                 #         (ul %in% c(0,1)) &
                 #         (dl %in% c(0,1)), "j_uneven",
                  ifelse(pl == 1 &
                           (pl == tl | tl == 0) &
                           (pl == jl | jl == 0) &
                           (pl == yl | yl == 0) &
                           (pl == ul | ul == 0) &
                           (pl == dl | dl == 0), "even",
                  ifelse(jl == 1 &
                           (jl == tl | tl == 0) &
                           (jl == pl | pl == 0) &
                           (jl == yl | yl == 0) &
                           (jl == ul | ul == 0) &
                           (jl == dl | dl == 0), "even",
                   ifelse(tl == 1 &
                           (tl == pl | pl == 0) &
                           (tl == jl | jl == 0) &
                           (tl == yl | yl == 0) &
                           (tl == ul | ul == 0) &
                           (tl == dl | dl == 0), "even",
                   ifelse(yl == 1 &
                          (yl == pl | pl == 0) &
                          (yl == jl | jl == 0) &
                          (yl == tl | tl == 0) &
                          (yl == ul | ul == 0) &
                          (yl == dl | dl == 0), "even",
                   ifelse(dl == 1 &
                          (dl == pl | pl == 0) &
                          (dl == jl | jl == 0) &
                          (dl == yl | yl == 0) &
                          (dl == ul | ul == 0) &
                          (dl == tl | tl == 0), "even",
                   ifelse(ul == 1 &
                          (ul == pl | pl == 0) &
                          (ul == jl | jl == 0) &
                          (ul == yl | yl == 0) &
                          (ul == ul | tl == 0) &
                          (ul == dl | dl == 0), "even",
                   ifelse((tl == pl | pl == 0) &
                          (tl == jl | jl == 0) &
                          (tl == yl | yl == 0) &
                          (tl == ul | ul == 0) &
                          (tl == dl | dl == 0), "even","uneven")))))))))))))),
    ID = z
  )
}


# For after the separate function, want to remove the commas and parentheses
rpl.sep <- function(x){
  ifelse(str_detect(x, '\\"\\)'), str_remove(x, '\\"\\)'), x)
}

collpse <- function(id, auth, yr, ti, c, p, doi, url, File, nested){
  data.frame(
    ID = id,
    author = paste(unlist(auth), collapse=', '),
    year = paste(unlist(yr), collapse=', '),
    title = paste(unlist(ti), collapse=', '),
    container = paste(unlist(c), collapse=', '),
    publisher = paste(unlist(p), collapse=', '),
    doi = paste(unlist(doi), collapse=', '),
    url = paste(unlist(url), collapse=', '),
    File = File,
    nested = nested)
}



rm.row2 <- c("^ACTION", "^Accession$", "^Additionally",  "\\!", "^Also", "^AM$", "^Avenue$", "^BE IT FURTHER RESOLVED", "PUBLIC COMMENT MEETING", "^Rd$", "Responses to comment", "^Assembly Bill$", "^BEFORE THE", "^Attachment$", "^E\\-?mail", "^Email [Cc]ommunication$", "^Executive Director", "^Experience$", "^Express$", "^Expwy$", "^Fax$", "^FAX", "^FROM\\:", "^Further Information", "^Given the adequacy", "^Homepage$", "^However", "^I‐\\d\\d", "^I\\‐\\d\\d", "^I\\d\\d", "^In addition", "^In compliance", "^In other words",  "^Informational Meeting", "^In preparation", "^In press", "In progress", "In submission", "^Last Accessed", "^Last [Aa]mended", "^Last [Mm]odified", "^Last [Rr]eviewed", "^Last [Rr]evised", "^Last [Uu]pdated", "^Location$", "^NOTICE|^Notice", "^[Pp]ersonal [Cc]ommunication", "^[Pp]ersonal [Ii]nterview", "^Phone$",  "^Please", "^Photo", "^Image", "^Public [Mm]eeting",  "^Recieved$",  "^Release$",  "^Response$", "^Responses to Comment$",  "^Retrieved$", "^Review$", "^Reporting Form$", "^Rept$", "^Research$", "^Resolution$", "^Review$", "^Revised", "^Revision$", "^Review Period$",  "^Rule$",  "^St$",  "^SUBJECT\\:|^Subject\\:",  "^Senate [Bb]ill$",   "^South$",  "^Study$",  "^Tel$", "^Telephone$",  "^The$",  "^Therefore", "^These", "^This", "^Thus", "^To\\s",  "^Wkdy$",  "^WHEREAS") #"^And\\b",
rm.row2 <- paste(rm.row2, collapse="|")

conference <- paste(c("[Cc]onference(?!\\sCenter)", "[Cc]onference(?!\\sHall)", "[Ss]ymposium"), collapse = "|")

final_clean <- function(x){
    x <- trimws(str_to_title(str_remove_all(x,'\\.|,|;|\\*|-|"|\\(|\\)|\\+|\\-|\\/|\\\\|:|\\[|\\]')))
    x <- str_remove_all(x, "'")
    x <- str_replace_all(x, "\\&", "and")
    str_squish(x)
}

