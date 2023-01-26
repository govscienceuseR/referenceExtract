#' Clean Anystyle output
#'
#' Runs through columns of the Anystyle output (c("date", "url", "title", "container", "publisher", "doi")) and cleans them. Steps include identifying the lengths of different lists in each reference to unlist and unnest them to create sensible references.
#'
#' @param dt data table from the reference_compile() function
#'
#' @return data table
#' @import data.table
#' @import magrittr
#' @import tools
#' @importFrom stringr str_detect str_remove_all str_extract
#' @import dplyr
#' @importFrom purrr pmap_dfr
#' @importFrom tidyr separate
#'
#' @examples cleaned_dt <- reference_clean(dt)
#'
#' @export

reference_clean <- function(dt){
  #source("R/clean_functions.R") these are now below
  # Add ID and replace NAs
  dt <- dt %>%
    data.table() %>%
    rename("container" = "container-title") %>%
    select(author, title, date, publisher, container,
           doi, url, File)  %>%
    mutate(ID = 1:nrow(.),
           title = na_if(title, "NULL"),
           container = na_if(container, "NULL"),
           publisher = na_if(publisher, "NULL"),
           date = na_if(date, "NULL"),
           doi = na_if(doi, "NULL"),
           url = na_if(url, "NULL"))

  # Get list lengths of each of the columns
  y = select(dt, ID)
  columns <- c("date", "url", "title", "container", "publisher", "doi")
  for(i in columns){
    x = dt[,..i]
    lengthdt <- pmap_dfr(list(x, y), index.lengths)
    colnames(lengthdt)[1] <- paste0(i, ".lengths")
    dt <- left_join(dt, lengthdt, by = "ID")
  }

  # Identify longest length in any column and filter to get get rid
  # of anything with more than the title max? [[POSSIBLE EDIT]]
  column.lengths <- paste0(columns,".lengths")
  MAX <- max(sapply(dt[, ..column.lengths], max))
  TITLE_MAX <- max(dt$title.lengths)

  dt <- dt %>% filter(date.lengths <= TITLE_MAX,
                      url.lengths <= TITLE_MAX,
                      container.lengths <= TITLE_MAX,
                      publisher.lengths <= TITLE_MAX,
                      doi.lengths <= TITLE_MAX)

  # Look for congruent cases using matching_fx
  lengths <- list()
  for(i in 1:(length(columns)+1)){
    if(i <= length(columns)){
      col <- column.lengths[i]
      lengths[i] <- dt[,..col]
    } else {
      lengths[i] <- dt[,"ID"]
    }
  }
  match.test <- pmap_dfr(lengths, matching_fx)
  dt <- left_join(dt, match.test, by = "ID")

  # have to do this because the columns of DT do not cooperate well with some
  # note that I htink that's fixed by calling dt[,col,with = F]
  df <- data.frame(dt)
  MAX_OG <- c()
  # Separate lists within cells
  for(i in 1:length(columns)){
    if(max(df[, column.lengths[i]]) > 1){
      df <- suppressWarnings(separate(df, columns[i],
                                      into =  paste0(columns[i],seq(1:max(df[, column.lengths[i]]))),
                                      sep = '\\"\\,\\s\\"'))
      col1 <- paste0(columns[i], "1")
      df[,col1] <- str_remove_all(df[,col1], 'c\\(\\"')
      cols <- grep(paste0(columns[i],"\\d+"), colnames(df))
      last <- length(df[,cols])
      MAX_OG[i] <- colnames(df[,cols])[last]
      df[,cols] <- lapply(df[,cols], rpl.sep)
    } else {
      MAX_OG[i] <- columns[i]
    }
  }

  # Specific rules for filtering out
  ## Dates: Extracting dates from pre-specified formats and take only year
  cols = which(str_detect(colnames(df), "date(?!\\.)|date\\d+"))
  # Before extracting I should see if it matches a DOI format
  df[,cols] <- lapply(df[,cols], extract_date_formats)
  df[,cols] <- lapply(df[,cols], assign_year)
  df[,cols] <- lapply(df[,cols], rm_yrs)
  df[,cols] <- lapply(df[,cols], as.numeric)
  ## URLS: If DOI, assign it to a new column, otherwise keep valid URL pattern
  cols = which(str_detect(colnames(df), "url(?!\\.)|url\\d+"))
  ## New columns for DOIs
  if(length(cols) == 1){
    df$doiurl <- extract_doi_url(df[,cols])
    df[,cols] <- sapply(df[,cols], rm_url) # using s versus l apply messed with me
    df[,cols] <- sapply(df[,cols], keep_urls)
    doiurlcols <- which(colnames(df) == "doiurl")
  } else {
    newcols <- c()
    for(i in 1:length(cols)){
      newcols[i] <- paste0("doiurl", i)
      vector <- data.frame(rep(NA, nrow(df)))
      names(vector) <- newcols[i]
      df <- cbind(df, vector)
    }
    doiurlcols = which(str_detect(colnames(df), "doiurl\\d+"))
    df[,doiurlcols] <- lapply(df[,cols], extract_doi_url)
    df[,cols] <- lapply(df[,cols], rm_url) # using s versus l apply messed with me
    df[,cols] <- lapply(df[,cols], keep_urls)
  }
  # Specific filtering: Titles
  cols = which(str_detect(colnames(df), "title(?!\\.)|title\\d+"))
  df[,cols] <- data.frame(lapply(df[,cols], rm_word))
  agency.titles <- data.frame(lapply(df[,cols], extract_agency_titles))
  df[,cols] <- lapply(df[,cols], rm_titles)
  #if(sum(!is.na(agency.titles$title1.agency.in.title)) > 0){
  #  df[,cols] <- lapply(df[,cols], rm_titles)
  #}
  # Specific filtering: CONTAINER
  cols = which(str_detect(colnames(df), "container(?!\\.)|container\\d+"))
  df[,cols] <- data.frame(lapply(df[,cols], rm_word))
  df[,cols] <- data.frame(lapply(df[,cols], rm_row))
  # Specific filtering: PUBLISHER
  cols = which(str_detect(colnames(df), "publisher(?!\\.)|publisher\\d+"))
  df[,cols] <- data.frame(lapply(df[,cols], rm_word))
  df[,cols] <- data.frame(lapply(df[,cols], rm_row))
  # Specific filtering: DOI
  cols = which(str_detect(colnames(df), "doi(?![\\.|url])|doi\\d+"))
  # Need to compare df[,cols] to df[,doiurlcols]
  if(length(cols) == 1){
    df[,cols] <- sapply(df[,cols], extract_doi)
    if(length(df[,doiurlcols]) == 1){
      df <- match_doi1(df) # could be modified to mash them up
    }
  } #else {
  #df[,cols] <- lapply(df[,cols], extract_doi)}
  # Specific filtering: Authors
  authordf <- pmap_dfr(list(df[,"author"], df[,"ID"]), separate_author)
  authordf <- authordf %>%
    mutate(author = str_remove_all(base::trimws(author), rm.auth.word)) %>%
    mutate(author = ifelse(str_detect(base::trimws(author), rm.row),
                           NA_character_, base::trimws(author))) %>%
    # Get rid of all non-word characters except: spaces, commas, &s -- got this from the internet
    mutate(author = str_remove_all(author, '[\\p{P}\\p{S}&&[^,& ]]')) %>%
    mutate(author = base::trimws(author)) %>%
    mutate(author = ifelse(author == "", NA_character_, author)) %>%
    # Then run these again...
    mutate(author = str_remove_all(base::trimws(author), rm.auth.word)) %>%
    mutate(author = ifelse(str_detect(base::trimws(author), rm.row),
                           NA_character_, base::trimws(author))) %>%
    ### Longer than 75 or shorter than 3 characters
    mutate(author = ifelse(nchar(author) > 75 | nchar(author) < 3, NA, author)) %>%
    filter(!is.na(author))

  # Removing duplicate and NAs, so lengthening and then widening (probably inefficient)
  # so that we can recount lengths and re-run the matching logic
  columns <- c(columns, "author")
  column.lengths <- c(column.lengths, "author.lengths")
  authorlengths <- authordf %>% group_by(ID) %>% count() %>% rename(author.lengths = n)
  df <- df %>% left_join(authorlengths, by = "ID")
  df$author.lengths <- ifelse(is.na(df$author.lengths), 0, df$author.lengths)
  if(max(df$author.lengths) > 1){
    MAX_OG[length(MAX_OG)+1] <- paste0("author", max(df$author.lengths))
  } else { MAX_OG[length(MAX_OG)+1] <- "author"}

  for(i in 1:length(columns)){
    coldetect <- paste0(columns[i], "(?![\\.|url])|", columns[i], "\\d+")
    cols = which(str_detect(colnames(df), coldetect))
    cols = cols[!(str_detect(colnames(df)[cols], "doiurl"))]
    if(max(df[, column.lengths[i]]) > 1){
      if(columns[i] != "author"){
        abbr.df <- df %>%
          filter(!(is.na(paste0(columns[i], "1")) & is.na(paste0(columns[i], "2")))) %>%
          select(ID, all_of(cols)) %>%
          pivot_longer(cols = 2:ncol(.),
                       names_to = paste0(columns[i],".number"),
                       values_to = columns[i]) %>%
          select(-paste0(columns[i],".number"))
      } else {abbr.df <- authordf}

      wide.df <- abbr.df %>%
        #unique() %>%
        #filter(!is.na(eval(parse(text = colnames(.)[2])))) %>% longer than needed
        filter(!is.na(.[2])) %>%
        group_by(ID) %>%
        mutate(number = paste0(columns[i], row_number())) %>%
        pivot_wider(names_from = number,
                    values_from = columns[i]) %>%
        ungroup()

      cols.wide <- grep(paste0(columns[i], "\\d+"), colnames(wide.df))
      last <- length(wide.df[,cols.wide])
      MAX <- colnames(wide.df[,cols.wide])[last]

      # Re-number the lengths
      wide.df$lengths = NA
      for(m in 1:nrow(wide.df)){
        for(j in cols.wide){
          if(is.na(wide.df$lengths[m]) & is.na(wide.df[m,j])){
            wide.df$lengths[m] <- j-cols.wide[1]
          } else {next}
        }
      }
      wide.df$lengths <- ifelse(is.na(wide.df$lengths), length(cols),
                                wide.df$lengths)

      ## Collapsing columns into lists again
      run.df <- wide.df %>% filter(lengths > 1)
      if(nrow(run.df) > 0){
        string.dt <- collapse_column(run.df, columns[i])
        colnames(string.dt) <- c(columns[i], "ID")
        lengthcol <- paste0(columns[i], ".lengths")
        df <- df %>%
          select(-c(colnames(.)[cols], all_of(lengthcol))) %>%
          left_join(wide.df, by = "ID") %>%
          mutate(lengths = case_when(
            is.na(lengths) ~ 0,
            T ~ as.double(lengths))) %>%
          left_join(string.dt, by = "ID")
      } else { # UNSURE ABOUT THIS
        df <- df %>%
          select(-c(colnames(.)[cols], lengthcol)) %>%
          left_join(wide.df, by = "ID") %>%
          mutate(lengths = case_when(
            is.na(lengths) ~ 0,
            T ~ as.double(lengths)))
      }
      # I cannot get this to work in a reproducible way...
      #df <- data.table(df)
      #colname.number <- paste0(columns[i], "1")
      #colname <- columns[i]
      #out <- ifelse(df$lengths == 0, NA,
      #                  ifelse(df$lengths == 1,
      #                    df[,..colname.number],
      #                    df[,..colname]))
      # The issue is filling this into here even though it is the exact same size
      # df[,..colname] <- out

      df <- reassign_value(df, i)
      colnames(df)[which(colnames(df) == "lengths")] <- lengthcol
      MIN <- paste0(columns[i], "1")
      df <- df %>%
        select(-c(MIN:MAX), -column.lengths[i])
      # Not MAX_OG, it seems

      y = select(dt, ID)
      dt <- data.table(df)
      colname <- columns[i]
      x = dt[,..colname]
      lengthdt <- pmap_dfr(list(x, y), index.lengths)
      colnames(lengthdt)[1] <- paste0(columns[i], ".lengths")
      dt <- left_join(dt, lengthdt, by = "ID")
      df <- data.frame(dt)
    }
  }

  dt <- data.table(df) %>% select(-nested)
  # Look for congruent cases using matching_fx
  lengths <- list()
  for(i in 1:(length(columns)+1)){
    if(i <= length(columns)){
      col <- column.lengths[i]
      lengths[i] <- dt[,..col]
    } else {
      lengths[i] <- dt[,"ID"]
    }
  }
  lengths <- lengths[-7] # exclude author
  match.test <- pmap_dfr(lengths, matching_fx)
  dt <- left_join(dt, match.test, by = "ID")

  # Go ahead and collapse authors before unnesting
  dt$author.new <- rep("", nrow(dt))
  dt$date.new <- rep("", nrow(dt))
  for(i in 1:nrow(dt)){
    dt[i,"author.new"] <- paste(unlist(dt[i,author]), collapse='; ')
    dt[i,"date.new"] <- suppressWarnings(str_extract(dt[i,date], "\\d{4}"))
  }


  dt <- select(dt, ID, author.new, date.new, title, container, publisher, doi, url, #doiurl,
               File,  author.lengths, date.lengths, title.lengths,
               container.lengths, publisher.lengths, doi.lengths, url.lengths, nested) %>%
    rename(author = author.new, date = date.new)

  run.dt <- dt %>% filter(nested == "tu_even_unn" | nested == "ty_even_unn")
  paste.dt <- anti_join(dt, run.dt)

  # For everything else that won't be unnested, collapse
  # Most of this is only length of 1 and so it won't matter, but it will collapse whatever hasn't yet been collapsed
  id = paste.dt[,ID]
  auth = paste.dt[,author]
  yr = paste.dt[,date]
  ti = paste.dt[,title]
  c = paste.dt[,container]
  p = paste.dt[,publisher]
  doi = paste.dt[,doi]
  url = paste.dt[,url]
  File = paste.dt[,File]
  nested = paste.dt[,nested]

  paste.un <- pmap_dfr(list(id, auth, yr, ti, c, p, doi, url, File, nested), collpse)
  paste.un <- select(paste.un, -ID)

  if(nrow(run.dt) > 0){
    #for(i in 1:nrow(run.dt)){
    #if (run.dt$nested[i] == "uneven") {
    #  runcols <- c("title", "container", "publisher", "doi", "url")
    #  run.dt[i,runcols] <- sapply(run.dt[i,runcols], function(x) paste(x, collapse=' '))
    #  run.dt[i,"date"] <- suppressWarnings(str_extract(run.dt[i,"date"], "\\d{4}"))
    #}}
    run.un <- data.table()
    for(i in 1:nrow(run.dt)){
      if (run.dt$nested[i] == "tu_even_unn") {
        un <- unnest(run.dt[i,], cols = c(title, url))
      } else if (run.dt$nested[i] == "ty_even_unn") {
        un <- unnest(run.dt[i,], cols = c(title, year))
      } else { next
      }
      run.un <- rbind(un, run.un)
    }
    run.un <- select(run.un, author, year, title, container,
                     publisher, doi, url, File, nested)
    dt <- rbind(run.un, paste.un) %>% select(-nested)
  } else {
      dt <- paste.un %>% select(-nested)
    }

  # Reassign ID since we have lost some values

  df <- data.frame(dt)
  # Squish together lists
  squish.cols <- c("title", "container", "publisher", "author")
  squish.cols <- which(colnames(df) %in% squish.cols)
  df[,squish.cols] <- suppressWarnings(sapply(df[,squish.cols], str_squish))
  df[,squish.cols] <- suppressWarnings(sapply(df[,squish.cols], encoding_change))
  df[,squish.cols[2:4]] <- suppressWarnings(sapply(df[,squish.cols[2:4]], final_clean))

  # Make sure there are
  df <- data.frame(sapply(df, rpl_na))
  # Remove anything that has basically no data in any column
  df <- filter(df, !(is.na(title) & is.na(author) & is.na(publisher) & is.na(doi) & is.na(url)))
  df <- unique(df)
  df$ID <- 1:nrow(df)
  dt <- data.table(df)
  return(dt)

}

# Supplemental functions:

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

org.words <- c("Administration", "Agency", "Association", "Associates", "Authority",  "Board", "Bureau", "Center", "Datacenter", "^Consult[a-z]+$",  "Commission", "Council", "County",  "Department", "District", "Foundation", "Government[s]*", "LLC", "Group", "Geological Survey", "Laboratory", "Service", "Society", "Univeristy", "\\bUS\\b")
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

match_doi1 <- function(df){
  doiurl <- ifelse(is.na(df[,"doiurl"]), "", df[,"doiurl"])
  doi <- ifelse(is.na(df[,"doi"]), "", df[,"doi"])
  df[,"doiurl"] <- ifelse(doi == doiurl, NA_character_, df[,"doiurl"])
  df[,"doi"] <- ifelse(doi != doiurl & doiurl != "", df[,"doiurl"], df[,"doi"])
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
      indices.l[b] <- yr
    }
    string.dt$V1[a] <- list(c(indices.l))
    string.dt$V2[a] <- x[[a,1]]
  }
  assign("string.dt", string.dt, envir = .GlobalEnv)
}

encoding_change <- function(x){
  iconv(x, from = "latin1", to = "ASCII", sub = "")
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
  ifelse(x == "NA" | x == "", NA, x)
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


columns4fx <- columns <- c("date", "url", "title", "container", "publisher",
                           "doi", "author")
reassign_value <- function(df, i){
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
    df$pubisher <- ifelse(df$lengths == 0, NA,
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
  x <- trimws(tools::toTitleCase(str_remove_all(x,'\\.|,|;|\\*|-|"|\\(|\\)|\\+|\\-|\\/|\\\\|:|\\[|\\]')))
  x <- str_remove_all(x, "'")
  x <- str_replace_all(x, "\\&", "and")
  str_squish(x)
}


