# HELPER FUNCTIONS FOR SCRIPTS "get_twitter.R"


# Load necessary libraries
if (!require("pacman")) install.packages("pacman")
# Check whether these libraries are installed, install if not and load all of them
pacman::p_load(academictwitteR,dplyr,lubridate,stringr,rtweet,purrr,qdapRegex,
               tm,plyr,vader)


# Save working directory
my_wd <- getwd()



############################### TWITTER ########################################

############################### full_count #####################################

# Function that counts tweets to be downloaded - does not use Twitter API quota
full_count<-function(keywords,start_tweets = "2010-01-01T00:00:00Z",
                     end_tweets = "2021-07-07T00:00:00Z",country = "US",lang = "en",
                     granularity = "day",n=5000, download_wd="./partial results"){
  # Empty list
  counts<-list()
  # Loop over keywords
  for (i in 1:length(keywords)){
    keyword<-keywords[i]
    nam <- keyword
    res <- count_all_tweets(query = keyword,
                            start_tweets = start_tweets, #month, day
                            end_tweets = end_tweets,
                            country = country, 
                            lang = lang,
                            bearer_token = get_bearer(), # load your key
                            granularity = granularity,
                            n=n)
    print(sum(res$tweet_count))
    assign(nam, res)
    # Save results into a list of dataframes
    counts[[nam]]<-res
    # Save the list after each loop (since the function sometimes stops/fails 
    # and like this, we can continue, with the next keyword)  
    save(counts, file=paste0(download_wd,"/counts.RData"))
  }
  return(counts)
}


############################### full_download ##################################

# Function for downloading multiple keywords via Twitter API
full_download<-function(counts.only,start_tweets = "2010-01-01T00:00:00Z",
                        end_tweets = "2021-07-07T00:00:00Z",country = "US",
                        lang = "en",granularity = "day",data_path = "data",
                        download_wd="./partial results"){
  st <- Sys.time()
  # Empty list
  tweets<-list()
  # Kewords
  keywords<-counts.only$keyword
  
  # Loop over keywords to download tweets
  for (i in 1:length(keywords)){
    keyword<-keywords[i]
    print(keyword)
    nam <- keyword #get the keyword name
    if (nam == "EUR/USD") nam="EUR_USD"
    n <- counts.only[i,2]+10 #get the estimated number of tweets plus 10 
    twts <- get_all_tweets(query = keyword,
                           start_tweets = start_tweets, #month, day
                           end_tweets = end_tweets,
                           country = country, 
                           lang = lang,
                           bearer_token = get_bearer(), # load your key
                           n=n,
                           file = nam, # saves each dataframe for given keyword as RDS under this name
                           data_path = data_path, # fetched data stored as json just in case 
                           bind_tweets = FALSE)
    print(paste(nam, nrow(twts), sep="_"))
    # Save results into a list of dataframes
    tweets[[nam]]<-twts
    # Save the list after each loop
    save(tweets, file=paste0(download_wd,"/tweets.RData"))
  }
  Sys.time() - st
  
  return(tweets)
}


############################### join.tweets ####################################

# Used in function prepare_tweetsdf
join.tweets <- function(tweets,keywords){
  # Add column of keyword to each dataframe
  # for (i in 1:length(keywords)){
  #   tweets[[i]]$keyword<-keywords[i]
  # }
  
  for (i in 1:length(names(tweets))){
    tweets[[i]]$keyword<-names(tweets)[i]
  }
  
  # Bind dataframes from list into one dataframe
  tweets.df <- bind_rows(tweets, .id = "keyword")
  
  return(tweets.df)
}





############################### convert.time.tw ################################

# Converting time format, changing time zones FOR TWITTER
convert.time.tw <- function(df){
  # convert date time 
  df$created_at<-as.POSIXct(strptime(df$created_at, 
                                     format = '%Y-%m-%dT%H:%M:%S'))
  # change time-zone (set correct original tz UTC, then change to UTC-5)
  df$created_at <- force_tz(df$created_at, "Etc/GMT")
  df$created_at <- with_tz(df$created_at, "Etc/GMT+5")
  # store only Date separately
  df$Date<-as.Date(df$created_at)
  
  return(df)
}


############################### prepare_tweetsdf ###############################

# Bind tweets from a list into one dataframe, add several columns 
prepare_tweetsdf <- function(tweets,keywords,keywords_list){
  # Add column of keyword to each dataframe
  # Bind dataframes from list into one dataframe
  tweets.df <- join.tweets(tweets,keywords)
  
  # Rename text column 
  textcolumn <- which(names(tweets.df)=="text")
  names(tweets.df)[textcolumn]<-c("alltext")
  
  # Add empty column for cleaned text
  tweets.df$cleantext<-NA
  
  # Add columns for sentiment variables
  results <- data.frame(matrix(ncol=9,
                               nrow=nrow(tweets.df), 
                               dimnames=list(NULL, c("sent","anger","anticipation", 
                                                     "disgust","fear","joy","sadness",
                                                     "surprise","trust"))))
  tweets.df <- cbind(tweets.df, results)
  
  # Replace emojis with their description
  tweets.df$emojitext <- textclean::replace_emoji(tweets.df$alltext)
  
  # Alternative if the above does not work
  # tweets.df <- replace.emoji(df=tweets.df, textcol = "alltext", dataset_wd = "./dataset")   # this function is loaded in shared_functions.R
  
  # Convert time, change time zone 
  tweets.df <- convert.time.tw(tweets.df)
  
  return(tweets.df)
  
}




