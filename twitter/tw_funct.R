# Load necessary libraries
if (!require("pacman")) install.packages("pacman")
# Check whether these libraries are installed, install if not and load all of them
pacman::p_load("tm",qdapRegex,tidytext,tidyverse,tidyr,dplyr,broom,lexicon)

#library(qdap)

# Save working directory
my_wd <- getwd()
  

#################################### PRE-PROCESSING TEXT #######################
# Helper functions for pre-processing

#################################### tryTolower ################################
  
# Try to change words into lower case
# Used in cleanCorpus
tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}
  
  
#################################### cleanCorpus ###############################
  
# Clean text, input: corpus + vector of stop words
# Used in get_sent
cleanCorpus<-function(corpus, customStopwords){
  # change some special characters to space
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  corpus <- tm_map(corpus, toSpace, "/") 
  corpus <- tm_map(corpus, toSpace, "@") 
  corpus <- tm_map(corpus, toSpace, "\\|")
  # remove "normal" urls and "twitter urls"
  rm_twitter_n_url <- rm_(pattern=pastex("@rm_twitter_url", "@rm_url"))
  corpus <- tm_map(corpus, content_transformer(rm_twitter_n_url))
  # corpus <- tm_map(corpus, content_transformer(qdap::replace_contraction)) 
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}


  
  
################################# SENTIMENT ####################################

################################# get_sent #####################################
  
# Define function that extracts sentiment values and emotions 
get_sent <- function(keywords,df){
  # specify stop words
  stops <- c(stopwords('SMART'))
  # extract stop words from keywords
  keyword.stops <- keywords.to.stops(keywords, stops)
  stops <- c(stops, keyword.stops)
  
  # loop over rows
  for (i in 1:nrow(df)){
    print(paste("row number",i))
    # create volatile corpus for each row (tweet or news article)
    corp <- VCorpus(VectorSource(df$alltext[i]))
    # clean corpus
    corp.clean <- cleanCorpus(corp, stops)
    # make a DTM, convert to matrix
    corp.dtm <- DocumentTermMatrix(corp.clean)
    
    # Sentiment
    # convert to tidy format
    tidycorp <- tidy(corp.dtm) 
    # for an empty tidy corpus (there were no words left after cleaning, happens for short tweets etc.), 
    # assign zeros to sent and all emotions
    if (nrow(tidycorp)==0){
      x<-which(colnames(df)=="sent")
      df[i,x:(x+8)]<-0
    } else {
      # get the nrc lexicon (only positive/negative)
      sent.nrc <- hash_sentiment_nrc
      names(sent.nrc) <- c('word', 'value')
      # inner join with the nrc lexicon (find common terms from data and lexicon)
      nrcPol <- inner_join(tidycorp,sent.nrc, by=c('term' = 'word'))
      #print(nrcPol)
      # if there are no terms with assigned sentiment
      if (nrow(nrcPol)==0){ 
        #print("no terms with assigned sentiment")
        df$sent[i] <- 0
      } else {
        # sum up
        nrcpol <- sum(nrcPol$value*nrcPol$count)
        df$sent[i] <- nrcpol
      }
      
      # get the nrc lexicon (only 8 emotions)
      nrc <- nrc_emotions
      # Pivot the data for joining 
      nrcLex <- pivot_longer(nrc, c(-term))
      nrcLex <- subset(nrcLex, nrcLex$value>0)
      nrcLex$value <- NULL
      # inner join with the nrc lexicon (find common terms from data and lexicon)
      nrcSent <- inner_join(tidycorp,nrcLex, by=c('term' = 'term'))
      
      # group results by emotions
      # if there are no terms with assigned emotions
      nams <- names(nrc[-1])
      if (nrow(nrcSent)==0){ 
        #print("no terms with assigned emotions")
        emotions <- data.frame(Var1=nams, Freq=0)
      } else{
        # else, group results by emotions
        emotions <- as.data.frame(table(nrcSent$name)) 
      }
      
      # assign each emotion to its corresponding column by name 
      for (j in 1: length(nams)){
        #print(nams[j])
        emo <- filter(emotions, Var1==nams[j])[,2]
        # if there is just one or more emotions missing, assign 0
        if ((identical(emo, numeric(0))) | (identical(emo, integer(0)))){
          emo <- 0
        }
        
        # # divide by number the total number of terms with assigned emotions
        # emo <- emo/sum(emotions$Freq)
        df[i,nams[j]]<-emo
      }
    }
  }
  return(df)
}


########################## keywords.to.stops ###################################
  
# Function: create a custom vector of stop words from the vector of keywords
keywords.to.stops <- function(keywords,stops){
  # create volatile corpus
  corp <- VCorpus(VectorSource(keywords))
  # clean corpus
  corp.clean <- cleanCorpus(corp, stops)
  # make a DTM, convert to matrix
  corp.dtm <- DocumentTermMatrix(corp.clean)
  corp.dtm.m <- as.matrix(corp.dtm)
  # create word frequency matrix
  wfm <- make.wfm(corp.dtm.m)
  # extract terms
  keyword.stops <- wfm$term
  return(keyword.stops)
}



########################## add.clean.text ###################################
  
add.clean.text <- function(keywords,df){
  # specify stop words
  stops <- c(stopwords('SMART'))
  # extract stop words from keywords
  keyword.stops <- keywords.to.stops(keywords, stops)
  stops <- c(stops, keyword.stops)
  
  # Add column with cleaned text
  # create volatile corpus
  corp <- VCorpus(VectorSource(df$alltext))
  # clean corpus
  st <- Sys.time()
  corp.clean <- cleanCorpus(corp, stops)
  Sys.time() - st
  # get and save cleaned text
  cleaned.text <-unlist(sapply(corp.clean, `[`, "content"))
  cleaned.text<-as.data.frame(cleaned.text)
  df$cleantext<-cleaned.text$cleaned.text
  
  return(df)
}  



############################### make.wfm #######################################

# Function to create word frequency matrix
make.wfm <- function(dtm.m){
  wfm <- data.frame(term = names(colSums(dtm.m)),
                    freq = colSums(dtm.m))
  # Order the WFM & remove the row names
  wfm <- wfm[order(wfm$freq, decreasing = T),]
  rownames(wfm) <- NULL
  
  return(wfm)
}