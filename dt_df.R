library(data.table)
# Start with a dt
dt <- fread("~/Box/citation_classifier/data/gsp_references_clean.csv")
class(dt)
# And let's write it as a df, also
df <- as.data.frame(dt)
class(df)

# The columns I want to be able to subset and perform a function on
columns <- c("year", "url", "title", "container", "publisher", "doi")

# Typically with a df I would subset like this to look at a column
df[,columns[1]]
# This does not work with a dt, it only prints the column name
dt[,columns[1]]
# Though directly inputting the name of the column prints the column itself.
dt[,"year"]
# One workaround I've been using that works, for some reason, is assigning
# the iterated value to an object, then subsetting that object
single_col <- columns[1]
# Again, this works the same for df
df[,single_col]
# And now it does work for dt, if you include include ..
dt[,..single_col]
# But this add an extra line to the coding you are iterating through with a
# for loop. But for apply functions, I haven't figured out what to do

# A simple function
rpl_na <- function(x){
  ifelse(x == "NA" | x == "", NA, x)
}

# I would typically do this with a df
df[,columns] <- sapply(df[,columns], rpl_na)

# But I don't know the equivalent of what works with a dt
# This has a length error
dt[,columns] <- sapply(dt[, ..columns], rpl_na)
# Assigning new values to ..columns doesn't work
dt[,..columns] <- sapply(dt[, ..columns], rpl_na)
# I suppose you can assign it to something new, but I don't like that
new <- sapply(dt[, ..columns], rpl_na)
# Is it that I have to wrap it in data.table?
dt[,columns] <- data.table(sapply(dt[, ..columns], rpl_na))

# Maybe that was it, so maybe we can pick some kind of standard for subsetting
# and for iteration, but for now I am jumping all over the place switching
# between these methods

