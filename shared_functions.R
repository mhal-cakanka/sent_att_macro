# COMMON HELPER FUNCTIONS USED THROUGHOUT THE PROJECT

# Load necessary libraries
if (!require("pacman")) install.packages("pacman")
# Check whether these libraries are installed, install if not and load all of them
pacman::p_load(readr,dplyr,rvest,textclean,rjson,quantmod)

# Save working directory
my_wd <- getwd()


# Source libraries and functions from tw_funct.R
source(here::here("./twitter/tw_funct.R"))

################################## WD ########################################## 

# Create a working directory if it doesn't exist
create_wd <- function(wd){
  if(!dir.exists(wd)){
    dir.create(wd)
    print("Directory created.")
  } else {
    # If the directory already exists, print a message
    print("Directory already exists. Using existing directory.")
  }
}


####################### RENAME AND EDIT DATASET ################################


####################### trading_days ###################################

# Function: download S&P500 data from Yahoo finance and extract trading days
# Input: my_wd = working directory, dataset_wd = working directory for dataset
# Output: tdd = vector of trading days

trading_days <- function(my_wd,dataset_wd = "./dataset"){
  # Load trading days if they dont already exist in working directory dataset
  # Set working directory to the parent directory
  setwd('..')
  if (!file.exists(paste0(dataset_wd,"/X_GSPC.csv"))) {
    # Download S&P500 data from yahoo finance
    X_GSPC <- getSymbols("^GSPC", src = "yahoo", auto.assign = FALSE)
    #  Convert to data frame
    X_GSPC <- data.frame(Date = index(X_GSPC), coredata(X_GSPC))
    # Save to csv
    write.csv(X_GSPC, file = paste0(dataset_wd,"/X_GSPC.csv"), row.names = FALSE)
    print("S&P500 data downloaded and saved to csv.")
  } else {
    # Read S&P500 data from csv
    X_GSPC <- read_csv(paste0(dataset_wd,"/X_GSPC.csv"), 
                       col_types = cols(Date = col_date(format = "%Y-%m-%d")))
    print("S&P500 data read from csv.")
  }
  # Set working directory back to the original
  setwd(my_wd)
  # Get trading days
  tdd<-as.character(X_GSPC$Date)
  
  return(tdd)
}


################################## max.weekends ###############################

# Function: average friday+saturday+sunday, assign to friday
# Input: td = vector of trading days, df = dataframe with data
# Output: df = dataframe with data

max.weekends <- function(td, df){
  # vector 'istd' = is the day a trading day? (0,1)
  istd<-get.istd(td, df)
  # Loop over columns
  for (k in 1:ncol(df)){
    print(paste("Column no.", k))
    # loop over rows
    for (j in 1:nrow(df)){
      # if the day is trading day
      if(istd[j]==1){
        # remember the day as "friday" = the last day before nontrading days
        friday <-j
        # get the value
        values <-df[j,k]
        # now lets check whether the following day is also trading day
        next.istd <- istd[j+1]
        # while the following day is not a trading day and it is still within the dataframe
        while((next.istd<1) & ((j+1)<=nrow(df))){
          j <- j+1
          values <- c(values,df[j,k])
          next.istd <- istd[j+1]
        }
        df[friday,k]<-max(values)
        # else if the day is not a trading day (weekend or holiday)
      } else {
        # assign NA
        df[j,k]<-NA
      }
    }
  }
  return(df)
}


################################## get.istd ####################################

# Function: create vector 'istd' = is the day a trading day? (0,1)
# Used in function max.weekends
get.istd<-function(td, df){
  istd<-rep(NA, nrow(df))
  for (i in 1:nrow(df)){
    # get Date
    x <- rownames(df)[i]
    # find out if it is in bloomberg dataset (only trading days)
    if (x %in% td){
      istd[i]<-1 # assign 1 for trading day
    }else{
      istd[i]<-0
    }
  }
  print("istd done")
  return(istd)
}


######################## split data by groups of keywords ######################

# Input: list of keywords, dataframe with all results in columns
# Output: list of dataframes 
split.events<-function(keywords_list,df){
  events<-keywords_list
  
  for (e in 1:length(keywords_list)){
    
    # Specify Event
    ev<-names(keywords_list)[e]
    print(ev)
    
    # Subset
    event_kw<-keywords_list[[ev]]
    ind<-which(names(df) %in% event_kw)
    
    # Fix names
    colnms<-names(df)[ind]
    rownms<-rownames(df)
    event<-as.data.frame(df[ , ind])
    colnames(event)<-colnms
    rownames(event)<-rownms
    
    # assign
    events[[e]]<-event
  }
  return(events)
}


######################### remove negative correlations #########################
neg.cor<-function(full){
  if (ncol(full)<=2){
    return(full)
  } else {
    # Correlation matrix
    x<-cor(full,use="pairwise.complete.obs")
    # Find the highest number of negative correlations per variable
    max.neg<-max(apply(x, 2, function(cols) sum(cols < 0)))
    
    # One by one drop variables with the max number of negative correlations
    # While the max.neg is above zero
    while(max.neg>0){
      print(max.neg)
      rem<-names(which.max(apply(x, 2, function(cols) sum(cols < 0))))
      print(paste("Dropped var",rem))
      full<-full[ , -which(names(full) %in% c(rem))]
      x<-cor(full,use="pairwise.complete.obs")
      max.neg<-max(apply(x, 2, function(cols) sum(cols < 0)))
    }
    return(full)
  }
  
}




######################### OTHER DATASET CHANGES ################################# 

############################### replace_nan_with_last ##########################

# Used in function reduce.dataset
# Replaces NAN with the previous value in a vector
replace_nan_with_last<-function(x,p=is.nan,d=0)c(d,x)[cummax(seq_along(x)*(!p(x)))+1]


############################### replace_nan_with_last ##########################

# Replaces NA with the previous value in a vector
replace_na_with_last<-function(x,p=is.na,d=0)c(d,x)[cummax(seq_along(x)*(!p(x)))+1]


############################### add.events ##########################


# Function: access name of list component via element - used in add.events
access <- function(lstData, elem) {
  i1 <- sapply(lstData, function(x) elem %in% x)
  names(lstData[i1])
}


# Function takes dataframe as input, subsets columns in nms,
# and then adds a column called "event" which is extracted from dictionary in json, the keywords_list
# Used for twitter dataset and newspaper proquest dataset
add.events <- function(df, keywords_list, nms = c("Date", "keyword", "event", "id", "sent", 
                                                  "anger", "anticipation", "disgust", "fear", 
                                                  "joy", "sadness", "surprise", "trust")) {
  # Subset columns based on supplied vector
  df <- df[, nms]
  
  # Use the `access` function to vectorize the event assignment
  df$event <- sapply(df$keyword, function(kwrd) {
    event <- access(keywords_list, kwrd)
    if (length(event) == 0) NA else event  # Handle cases where `access` returns no match
  })
  
  return(df)
}



############################### duplicates ####################################

# Filter data: subset by events and remove duplicates within each subset
# Used for twitter and newspapers (proquest) dataset
duplicates<-function(df){
  
  # Extract unique values in column "event"
  events<-unique(df$event)
  
  # Loop over these events
  for (e in 1:length(events)){
    
    # Subset all rows with the eth event
    df.event<-subset(df, df$event==events[e])
    
    # Count original number of tweets/articles
    r<-nrow(df.event)
    
    # Remove duplicated tweets/articles based on text, keep only unique/distinct rows
    df.event <- df.event %>% distinct(df.event$id, .keep_all = TRUE)
    
    # Count tweets after filtering
    rn<-nrow(df.event)
    
    # Report how many tweets/articles were removed
    print(paste(events[e],"removed", r-rn, "ids"))
    
    
    # This creates a redundant column - remove it
    df.event <- df.event[,-c(ncol(df.event))]
    
    # Bind results
    if (e ==1){
      df.fin<-df.event
    } else {
      df.fin<-rbind(df.fin,df.event)
    }
  }
  return(df.fin)
}




# Function: aggregate attention or sentiment into daily measures based on keywords or events
aggregate_daily <- function(df, agreg = "events", start = "2007-12-10", end = "2021-07-07",
                            opt = "volume", all.kwrd.order, col_select = c("anger", "disgust", "fear")) {
  
  # Assign values to column category from column keyword or column event based on agreg method
  if (agreg == "keywords") {
    df$category <- df$keyword
  } else {
    df$category <- df$event
  }
  
  # Prepare sequence of dates
  dts <- as.character(seq(as.Date(start), as.Date(end), by = "day"))
  
  # Initialize final result
  fin_result <- NULL
  
  # Loop through unique categories
  for (category in unique(df$category)) {
    print(category)
    
    # Subset documents for the current category
    subs <- df[df$category == category,]
    subs$Date <- as.character(subs$Date)
    
    # Filter out dates from subs$Date, that are not in dts
    subs <- subs[subs$Date %in% dts,]
    
    # Prepare empty result dataframe
    
    # ATTENTION
    # Attention == volumne of tweets etc., 
    # Hence if you want to aggregate attention, choose opt==volume
    if (opt == "volume") {
      result <- data.frame(Date = dts, Count = 0)
      
      # Count the number of rows for each date
      counts <- table(subs$Date)
      # Fill the result dataframe with counts
      result$Count[result$Date %in% names(counts)] <- as.numeric(counts)

      # Convert to matrix format for merging
      result <- setNames(data.frame(result$Count), category)
      
    } else {
      # SENTIMENT
      # Prepare dataframe for sentiment aggregation
      result <- data.frame(matrix(ncol = length(col_select), nrow = length(dts),
                                  dimnames = list(dts, col_select)))
      
      # Aggregate sentiment by selected option
      for (col in col_select) {
        daily_values <- tapply(subs[[col]], subs$Date, switch(opt,
                                                              max = max,
                                                              mean = mean,
                                                              sum = sum,
                                                              sd = sd,
                                                              NULL), na.rm = TRUE)
        result[[col]][dts %in% names(daily_values)] <- daily_values
      }
      
      # Handle multiple columns in col_select by taking row means
      result <- data.frame(rowMeans(result, na.rm = TRUE))
      colnames(result) <- category
    }
    
    # Fill NaN values with the last known value
    result <- apply(result, 2, replace_nan_with_last) %>% data.frame()
    print(summary(result))
    
    # Merge results
    fin_result <- if (is.null(fin_result)) result else cbind(fin_result, result)
  }
  
  # Assign rownames and reorder columns
  rownames(fin_result) <- dts
  fin_result <- fin_result[, all.kwrd.order, drop = FALSE]
  
  return(fin_result)
}

################################## EMOJIS ######################################

# Function to load the emoji list table into a dataframe
load_emoji_list <- function(url = "http://unicode.org/emoji/charts/full-emoji-list.html") {
  
  
  # Read the HTML content of the page
  webpage <- read_html(url)
  
  # Extract the table
  emoji_table <- webpage %>%
    html_node("table") %>% # Find the first table on the page
    html_table(fill = TRUE) # Convert it to a dataframe
  
  # COnvert to a dataframe
  emoji_table<-as.data.frame(emoji_table)
  # Set the values of the second row as column names and drop the first row
  colnames(emoji_table) <- emoji_table[2, ]
  emoji_table <- emoji_table[-c(1,2), ]
  # Drop rows with mid-table headers
  emoji_table[,1]<-as.numeric(emoji_table[,1])
  emoji_table<-emoji_table[!is.na(emoji_table[,1]),]
  
  # Select and rename desired columns
  emoji_df <- emoji_table %>%
    select(code = `Code`, description = `CLDR Short Name`) # Rename columns
  
  print("Succesfully loaded unicode emojis")
  
  return(emoji_df)
}


############################### replace.emoji ##################################

# Dealing with emojis
# A custom alternative to the textclean::replace_emoji function, currently not in use
replace.emoji <- function(df, textcol="alltext", dataset_wd = "./dataset") {
  
  # Remember the current working directory and move up one level
  this_wd <- getwd()
  print(this_wd)
  setwd('..')
  
  # Try to load emoji.csv from dataset_wd folder, if it exists
  if (file.exists(paste0(dataset_wd, "/emoji.csv"))) {
    emojis <- read_csv(paste0(dataset_wd, "/emoji.csv"), col_types = cols())
    print("Emoji list loaded from csv")
  } else {
    # If it doesn't exist, load the emoji list from the web
    emojis <- load_emoji_list()
    # Save it to a csv file for future use
    write_csv(emojis, paste0(dataset_wd, "/emoji.csv"))
    print("Emoji list saved to csv")
  }
  
  # Make sure this is a dataframe
  emojis<-data.frame(emojis)
  
  # Set working directory back to the original
  setwd(this_wd)
  
  # Replace emojis in the text with their descriptions
  df[, textcol] <- mgsub(pattern=emojis$code,
                      replacement=paste0(' ', emojis$description,' '),
                      x=df[, textcol])
  return(df)
}


# A very simple version of cleancorpus function (only removes urls)
# this is how we clean tweets or news articles before running finbert
removeurl <- function(corpus) {
  clean_text <- content_transformer(function(x) {
    x <- str_replace_all(x, "@rm_twitter_url|@rm_url", " ")
    x <- str_replace_all(x, "[/@|]", " ")
    x
  })
  corpus <- tm_map(corpus, clean_text)
  corpus <- tm_map(corpus, removePunctuation, 
                   preserve_intra_word_contractions = TRUE, 
                   preserve_intra_word_dashes = TRUE)
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}



# Helper function for loading and renaming our input data
loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}
