#' Clean Anystyle output
#'
#' Runs through columns of the Anystyle output (c("date", "url", "title", "container", "publisher", "doi")) and cleans them. Steps include identifying the lengths of different lists in each citation to unlist and unnest them to create sensible citations.
#'
#' @param dt data table from the citation_compile() function
#'
#' @return data table
#' @import data.table
#' @import magrittr
#' @import stringr
#' @import
#' @examples cleaned_dt <- citation_clean(dt)
#'
#' @export

citation_clean <- function(dt){
  source("R/clean_functions.R")
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

