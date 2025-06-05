# HELPER FUNCTIONS FOR SCRIPT "get_proquest.R"


# Load necessary libraries
if (!require("pacman")) install.packages("pacman")
# Check whether these libraries are installed, install if not and load all of them
pacman::p_load(readtext,stringr,textclean)


# Save working directory
my_wd <- getwd()



################################# PROQUEST #####################################

################## get_texts ###################################################

# Read in articles from Proquest in a text file and split them
# Used in function get_proquest_df
get_texts <- function(proquest_file){
  # Read in articles from Proquest in a large text file
  texts <- readtext(proquest_file)
  # Split the Text by unique character into list of unique stories
  texts <-str_split(texts$text, "Database: ")
  # Unlist the list for easier manipulation
  texts <- unlist(texts[[1]])
  return(texts)
}

################## extract_metadata ############################################

# Extract metadata from a proquest text file
# Used in function get_proquest_df
extract_metadata <- function(texts){
  # Empty dataframe
  metadata <- data.frame(matrix(ncol = 16, nrow = 0))
  colnames(metadata) <- c("article_id", "author", "abstract", "subject_terms",
                          "title","location", "pub_title", "pages", "pub_year", "pub_date",
                          "section", "publisher", "place", "issn", "source", "url")
  tokens <- c("\nProQuest document ID:", "\nAuthor:", "\nAbstract:", "\nSubject:",
              "\nTitle:", "\nLocation:", "\nPublication title:", "\nPages:", 
              "\nPublication year:", "\nPublication date:", "\nSection:", "\nPublisher:", 
              "\nPlace of publication:", "\nISSN:", "\nSource type:", "\nDocument URL:")
  
  TS <- length(texts)
  # Loop over columns
  for (i in 1:ncol(metadata)){
    # Loop over rows/articles
    for (j in 1:TS){
      # Pattern to be matched in each article texts[j]
      pattern <- paste(tokens[i], " (.*)\n", sep = '')
      # Assign matched text string to its rightful place
      metadata[j,i] <- str_match(texts[j], pattern)[,2]
    }
  }
  return(metadata)
}

################## clean_articles ##############################################

# Clean Full text of each article
# Used in function get_proquest_df
clean_articles <- function(texts){
  # Empty dataframe
  text <- data.frame(matrix(ncol = 1, nrow = 0))
  colnames(text) <- "text"
  TS <- length(texts)
  for (i in 1:TS){
    clean_text <- gsub("\n", "", texts[i])
    clean_text <- gsub('"', '', clean_text)
    clean_text <- gsub('\\*', '', clean_text)
    text[i,1] <- str_match(clean_text, "Full text:(.*?)Title:")[,2]
  }
  return(text)
}

############################## get_proquest_df #################################

# One function that combines all 3 previous functions
get_proquest_df <- function(proquest_file, keyword) {
  # Read text file, split into articles
  texts  <- get_texts(proquest_file = proquest_file)
  # Get rid of last split that is 'garbage'
  texts <- texts[-length(texts)]
  # Extract metadata into a dataframe
  metadata <- extract_metadata(texts = texts)
  # Extract only text of the article into a dataframe
  text <- clean_articles(texts = texts)
  # Put into one dataframe
  df <- cbind(text, metadata)
  # column with keyword
  df$keyword<-keyword
  # paste title, abstract and text into one column
  df$alltext <- paste(df$title, df$abstract, df$text)
  # make empty columns for results
  results <- data.frame(matrix(ncol=9,nrow=nrow(df), 
                               dimnames=list(NULL, c("sent","anger","anticipation", 
                                                     "disgust","fear","joy","sadness", 
                                                     "surprise","trust"))))
  df <- cbind(df, results)
  
  return(df)
}


############################## process_proquest_df #################################

process_proquest_df <- function(df){
  # Financial Times publishes the same article 4 times:
  # normal, Usa Region, Asia Region and Europe Region, this is in the title
  # let's delete these
  df <- df[-(grep("Region ", df$title, ignore.case=TRUE)),]
  
  # Replace emojis with their description
  df$alltext <- textclean::replace_emoji(df$alltext)
  
  # Alternative if the above does not work
  # df <- replace.emoji(df, textcol="alltext", dataset_wd = "./dataset")   # this function is loaded in shared_functions.R
  
  # Convert time, change time zone 
  df <- convert.time.pq(df)
}


############################## get_allpq #######################################

get_allpq <- function(keywords, publication_titles, location, from, to,data_wd){
  current_wd<-getwd()
  setwd(data_wd)
  
  # READ IN articles in a text file, extract text of articles and metadata, save into a dataframe
  # Loop over all keywords
  for (i in 1:length(keywords)){
    keyword <- keywords[i]
    print(keyword)
    # Name of the ith file
    pt <- paste(paste(keyword, publication_titles, location, from, to, sep = '_'), 
                ".txt", sep='')
    
    # Try to load this file
    try_error = tryCatch(get_texts(pt), error = function(e) e)
    
    if (!inherits(try_error, 'error')){
      # In case of no error, load and process this file
      df.i <- get_proquest_df(pt,keyword)
      print("no error")
      # Bind files into one dataframe
      if (i==1){
        df <-df.i 
      } else {
        df <- rbind(df, df.i)
      }
    }
  }
  
  # Set working directory back to original
  setwd(current_wd)
  
  # PROCESS proquest dataframe (remove duplicates, replace emojis, convert time)
  df <- process_proquest_df(df)
  
  # Add empty column for cleaned text
  df$cleantext<-NA
  
  # Add empty column for events
  df$event<-NA
  
  
  return(df)
}  


############################### convert.time.pq ################################

# Converting time format, changing time zones FOR PROQUEST  
convert.time.pq <- function(df){
  # Change time zone and date format  
  #save current locale
  original_locale<-Sys.getlocale(category = "LC_TIME")
  #change it to english
  Sys.setlocale("LC_ALL","English")
  #transform dates
  df$pub_date <- as.POSIXct(df$pub_date, format = '%b %d, %Y')
  df$pub_date <- as.Date(df$pub_date)
  #change it back to the original setting
  Sys.setlocale(category = "LC_TIME", locale = original_locale)
  
  return(df)
}



