#' Clean Anystyle output
#'
#' Version 2,runs through columns of the Anystyle output (c("date", "url", "title", "container", "publisher", "doi")) and cleans them. Steps include identifying the lengths of different lists in each reference to unlist and unnest them to create sensible references.
#'
#' @param dt data.table from the reference_compile() function
#'
#' @return data table
#' @import data.table
#' @import dplyr
#' @import magrittr
#' @import tools
#' @import stringr
#' @importFrom purrr pmap_dfr
#' @import tidyr
#' @examples cleaned_dt <- reference_clean3(dt)
#' @export

reference_clean3 <- function(dt){
  if(all(class(dt)!='data.table')){stop('please provide a data.table object')}
  #source("R/clean_functions.R") these are now below
  # Add ID and replace NAs
  if(any(colnames(dt)=='container-title')){setnames(dt,'container-title','container')}
  vars <- c('author','title','date','publisher','container','doi','url','File')
  vars <- vars[vars %in% colnames(dt)]
  dt <- dt[,colnames(dt) %in% vars,with = F]
  dt$ID = 1:nrow(dt)
  #### replace NULL with NA to act as placeholder
  dt[ , (vars) := lapply(.SD, null2NA), .SDcols = vars]

  columns <- c("date", "url", "title", "container", "publisher", "doi")
  lt <- dt[,lapply(.SD,function(x)sapply(x,length)),.SDcols = columns]
  colnames(lt) <- paste0(colnames(lt),'.lengths')
  dt <- cbind(dt,lt)
  # Identify longest length in any column and filter to get get rid
  # of anything with more than the title max? [[POSSIBLE EDIT]]
  column.lengths <- paste0(columns,".lengths")
  MAX <- max(sapply(dt[, ..column.lengths], max))
  TITLE_MAX <- max(dt$title.lengths)
  ### all entries for a given row have fewer entries than max title entries observed
  dt <- dt[apply(dt[,..column.lengths] <= TITLE_MAX,1,all),]

  ##### NOTE THAT matching_fx right now breaks when real column names are used
  ##### this can (should?) be fixed and then setting names as NULL would not be needed
  lengths <- as.list(dt[,..column.lengths])
  lengths$ID <- dt$ID
  names(lengths) <- NULL
  # Look for congruent cases using matching_fx
  match.test <- pmap_dfr(lengths, matching_fx)

  dt <- merge(dt, match.test, all.x = T,by = "ID")

  spread_columns <- lapply(columns,function(x){
    x.length <- paste0(x,'.lengths')
    ml <- max(dt[,x.length,with = F])
    col_spread <- data.table(do.call(rbind,lapply(dt[[x]],function(x) {c(x,rep(NA,ml - length(x)))})))
    names(col_spread) <- paste0(x,1:ncol(col_spread))
    col_spread
  })


  dt <- cbind(dt[,!colnames(dt) %in% columns,with = F],data.table(do.call(cbind,spread_columns)))

  # Specific rules for filtering out
  ## Dates: Extracting dates from pre-specified formats and take only year
  date_cols <- colnames(dt)[str_detect(colnames(dt), "date[0-9]")]
  # Before extracting I should see if it matches a DOI format
  dt[,(date_cols):=lapply(.SD,extract_date_formats),.SDcols = date_cols]
  dt[,(date_cols):=lapply(.SD,assign_year),.SDcols = date_cols]
  dt[,(date_cols):=lapply(.SD,rm_yrs),.SDcols = date_cols]
  dt[,(date_cols):=lapply(.SD,as.numeric),.SDcols = date_cols]

  ## URLS: If DOI, assign it to a new column, otherwise keep valid URL pattern
  cols = str_subset(colnames(dt), "url(?!\\.)|url\\d+")
  ## New columns for DOIs
  if(length(cols) == 1){
    dt$doiurl1 <- extract_doi_url(dt[[cols]])
    dt[[cols]] <- sapply(dt[[cols]], rm_url) # using s versus l apply messed with me
    dt[[cols]] <- sapply(dt[[cols]], keep_urls)
    doiurlcols <- which(colnames(dt) == "doiurl")
  } else {
    newcols <- c()
    for(i in 1:length(cols)){
      newcols[i] <- paste0("doiurl", i)
      vector <- data.frame(rep(NA, nrow(dt)))
      names(vector) <- newcols[i]
      dt <- cbind(dt, vector)
    }
    doiurlcols = which(str_detect(colnames(dt), "doiurl\\d+"))
    dt[,(doiurlcols):=lapply(.SD,extract_doi_url),.SDcols = cols]
    #dt[,doiurlcols,with = F] <- lapply(dt[,cols,with = F], extract_doi_url)
    dt[,(cols):=lapply(.SD,rm_url),.SDcols = cols]
    dt[,(cols):=lapply(.SD,keep_urls),.SDcols = cols]
    #df[,cols] <- lapply(df[,cols], rm_url) # using s versus l apply messed with me
    #df[,cols] <- lapply(df[,cols], keep_urls)
  }

  # Specific filtering: Titles
  cols = str_subset(colnames(dt), "title(?!\\.)|title\\d+")
  dt[,(cols):=lapply(.SD,rm_word),.SDcols=cols]

  agency.titles <- dt[,lapply(.SD,extract_agency_titles),.SDcols = cols]
  #agency.titles <- data.frame(lapply(dt[,..cols], extract_agency_titles))
  dt[,(cols):=lapply(.SD,rm_titles),.SDcols = cols]
  #df[,cols] <- lapply(df[,cols], rm_titles)

  #if(sum(!is.na(agency.titles$title1.agency.in.title)) > 0){
  #  df[,cols] <- lapply(df[,cols], rm_titles)
  #}
  # Specific filtering: CONTAINER
  cols = str_subset(colnames(dt), "container(?!\\.)|container\\d+")
  dt[,(cols):=lapply(.SD,rm_word),.SDcols = cols]
  dt[,(cols):=lapply(.SD,rm_row),.SDcols = cols]
  #df[,cols] <- data.frame(lapply(df[,..cols], rm_word))
  #df[,cols] <- data.frame(lapply(df[,cols], rm_row))
  # Specific filtering: PUBLISHER
  cols = str_subset(colnames(dt), "publisher(?!\\.)|publisher\\d+")
  dt[,(cols):=lapply(.SD,rm_word),.SDcols = cols]
  dt[,(cols):=lapply(.SD,rm_row),.SDcols = cols]
  #df[,cols] <- data.frame(lapply(df[,cols], rm_word))
  #df[,cols] <- data.frame(lapply(df[,cols], rm_row))
  # Specific filtering: DOI
  cols = str_subset(colnames(dt), "doi(?![\\.|url])|doi\\d+")

  # Need to compare df[,cols] to df[,doiurlcols]
  if(length(cols) == 1){
    dt[,(cols):=lapply(.SD,extract_doi),.SDcols = cols]
    #dt[,..cols] <- sapply(df[,cols], extract_doi)
    if(ncol(dt[,..doiurlcols]) == 1){
      dt <- match_doi1(dt) # could be modified to mash them up
    }
  } #else {
  #df[,cols] <- lapply(df[,cols], extract_doi)}
  # Specific filtering: Authors
  authordt <- separate_author2(dt)
  authordt$author.clean <- str_remove_all(base::trimws(authordt$author.clean), rm.auth.word)
  authordt$author.clean <- ifelse(str_detect(base::trimws(authordt$author.clean), rm.row), NA_character_, base::trimws(authordt$author.clean))
  # Get rid of all non-word characters except: spaces, commas, &s -- got this from the internet

  authordt$author.clean <- str_remove_all( authordt$author.clean, '[\\p{P}\\p{S}&&[^,& ]]')
  authordt$author.clean <- base::trimws(authordt$author.clean)
  authordt$author.clean <- ifelse(authordt$author.clean == "", NA_character_, authordt$author.clean)
  # Then run these again... [TYLER NOTE -- IF WE STRIP FIRST AND THEN DO THIS DO WE NEED TO RE-RUN?]
  authordt$author.clean <- str_remove_all(base::trimws(authordt$author.clean), rm.auth.word)
  authordt$author.clean <- ifelse(str_detect(base::trimws(authordt$author.clean), rm.row),
                                  NA_character_, base::trimws(authordt$author.clean))
  ### Longer than 75 or shorter than 3 characters
  authordt$author.clean <- ifelse(nchar(authordt$author.clean) > 75 | nchar(authordt$author.clean) < 3, NA, authordt$author.clean)
  authordt <- authordt[!is.na(authordt$author.clean),]
  authordt$author <- authordt$author.clean
  # Removing duplicate and NAs, so lengthening and then widening (probably inefficient)
  # so that we can recount lengths and re-run the matching logic
  columns <- c(columns, "author")
  column.lengths <- c(column.lengths, "author.lengths")
  authorlengths <- authordt[,.N,by=.(ID)]
  setnames(authorlengths,'N','author.lengths')
  dt <- merge(dt,authorlengths,by = 'ID',all.x = T)
  dt$author.lengths <- ifelse(is.na(dt$author.lengths), 0, dt$author.lengths)

  #### note that change here to key on length of cols and then rename cols at bottom of length cols == 1 is because
  ### the if the code only finds lenght = 1, it still makes column lke "doi1" but then there is no "doi2". So
  ### then it's desirable to just rename again as "doi", which normally happens in the for() call below
  ### such that doi1 and doi2 are combined to doi and merged back in again
  for(i in 1:length(columns)){
    cols <- str_subset(colnames(dt),paste0('^',columns[i],'($|[0-9])'))
    # LIZA REMOVED this conditional because it stopped the adding of the new author data frame
      if(columns[i] != "author"){
        abbr.dt <- dt[!(is.na(paste0(columns[i], "1")) & is.na(paste0(columns[i], "2"))),]
        abbr.dt <- abbr.dt[,c('ID',cols),with = F]
        abbr.dt <- melt(abbr.dt,id.vars = 'ID',variable.name = paste0(columns[i],".number"),value.name =  columns[i])
        abbr.dt <- abbr.dt[,(paste0(columns[i],".number")):=NULL]
      } else {abbr.dt <- authordt}

      abbr.dt <- abbr.dt[!is.na(abbr.dt[[columns[i]]]),]
      newname <- 'number'
      abbr.dt[,(newname):=lapply(.SD,rank,ties.method = 'first'),by=.(ID),.SDcols = c('ID')]
      abbr.dt[[newname]] <- paste0(columns[i],abbr.dt[[newname]])
      wide.dt <- dcast(abbr.dt,ID ~ number,value.var =  columns[[i]] )
      cols.wide <- grep(paste0(columns[i], "\\d+"), colnames(wide.dt))
      last <- length(wide.dt[,cols.wide,with = F])
      MAX <- colnames(wide.dt[,cols.wide,with = F])[last]

      wide.dt$lengths <- rowSums(!is.na(wide.dt[,cols.wide,with = F]))

      ## Collapsing columns into lists again
      run.dt <- wide.dt[lengths>1,]
      if(nrow(run.dt) > 0){
        string.dt <- collapse_column(run.dt, columns[i])
        colnames(string.dt) <- c(columns[i], "ID")
        lengthcol <- paste0(columns[i], ".lengths")
        dt <- merge(dt[,-c(cols,lengthcol),with = F],wide.dt,all.x = T,by = 'ID')
        dt$lengths[is.na(dt$lengths)]<-0
        dt <- merge(dt,string.dt,all.x = T,by = 'ID')
      } else { # UNSURE ABOUT THIS
        dt[,-c(cols,lengthcol),with = F]
        dt <- merge(dt[,-c(cols,lengthcol),with = F],wide.dt,all.x = T,by = 'ID')
        dt$lengths[is.na(dt$lengths)]<-0
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

      dt <- reassign_value(dt, i)
      dt <- as.data.table(dt)
      colnames(dt)[which(colnames(dt) == "lengths")] <- lengthcol
      MIN <- paste0(columns[i], "1")

      dt <- dt[,-c(which(colnames(dt)==MIN):which(colnames(dt)==MAX),which(colnames(dt)==column.lengths[i])),with = F]

      # Not MAX_OG, it seems
      y <- dt[,'ID']
      colname <- columns[i]
      x = dt[,..colname]
      lengthdt <- pmap_dfr(list(x, y), index.lengths)
      colnames(lengthdt)[1] <- paste0(columns[i], ".lengths")
      dt <- merge(dt, lengthdt, all.x = T,by = "ID")
    }
  dt[,nested:=NULL]
  #dt <- data.table(df) %>% select(-nested)
  # Look for congruent cases using matching_fx
  ### exclude author
  lengths <- as.list(dt[,..column.lengths[column.lengths!='author.lengths']])
  lengths$ID <- dt$ID
  names(lengths) <- NULL
  # Look for congruent cases using matching_fx
  match.test <- pmap_dfr(lengths, matching_fx)

  dt <- merge(dt, match.test, all.x = T,by = "ID")

  #### NOTE TYLER ADDED THIS SIMPLE TOGGLE -- SOLUTION IS TO NOT HAVE 'DOI1' CREATED ###

  # Go ahead and collapse authors before unnesting
  dt$author.new <- rep("", nrow(dt))
  dt$date.new <- rep("", nrow(dt))

  dt$author.new <- sapply(dt$author,function(x) paste(na.omit(x),collapse = '; '))
  dt$date.new <- sapply(dt$date,function(x) str_extract(x,"\\d{4}"))
  dt$author.new[dt$author.new==''] <- NA

  #### making character vector allows for cases with missing entries (vs. select() method)
  vars <- c('ID','author.new','date.new','title','container','publisher','doi','url',
            'File',' author.lengths','date.lengths','title.lengths',
            'container.lengths','publisher.lengths','doi.lengths','url.lengths','nested')

  dt <- dt[,colnames(dt) %in% vars,with = F]
  setnames(dt,c('author.new','date.new'),c('author','date'),skip_absent = T)
  run.dt <- dt[nested %in% c("tu_even_unn","ty_even_unn"),]
  #run.dt <- dt %>% filter(nested == "tu_even_unn" | nested == "ty_even_unn")
  #paste.dt <- anti_join(dt, run.dt)
  #anti_join in R
  paste.dt <- dt[!run.dt, on = .(ID)]

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

    run.un <- run.un[,c('author','year','title','container','publisher','doi','url','File','nested')]

    dt <- rbind(run.un, paste.un) %>% select(-nested)
  } else {
    dt <- paste.un %>% select(-nested)
  }

  # Reassign ID since we have lost some values

  dt <- data.table(dt)

  # Squish together lists
  squish.cols <- c("title", "container", "publisher", "author")
  #squish.cols <- which(colnames(df) %in% squish.cols)
  dt[,(squish.cols):=lapply(.SD,str_squish),.SDcols = squish.cols]
  dt[,(squish.cols):=lapply(.SD,encoding_change),.SDcols = squish.cols]
  # LIZA ADD: Added author to this because it was collapsing
  dt[,(squish.cols[squish.cols!=c('title', 'author')]):=lapply(.SD,final_clean),.SDcols = squish.cols[squish.cols!=c('title', 'author')]]

  #### FLAG FOR FIX -- NA'S ARE TREATED LIKE STRINGS, SO str_tot_title makes NA --> Na.
  #### for now, added that to rpl_na code to catch, but need to make sure NAs are treated properly throughout

  # Make sure there are
  dt <- data.table(sapply(dt, rpl_na))
  # Remove anything that has basically no data in any column
  nothing <- is.na(dt$title) & is.na(dt$author) & is.na(dt$publisher) & is.na(dt$doi) & is.na(dt$url)
  dt <- dt[!nothing,]
  dt <- unique(dt)
  dt$ID <- 1:nrow(dt)
  return(dt)
}
