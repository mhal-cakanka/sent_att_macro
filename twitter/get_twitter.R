# DOWNLOAD TWITTER DATA WITH ACADEMIC API
# Disclaimer: Academic API is no longer available, code showcases, how was data acquaired

# Source the helper functions script to load custom functions and load libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(here)
source(here::here('shared_functions.R'))

# Dataset working directory (where the final dataset will be saved)
dataset_wd = "./dataset"

# Create a working directory if it doesn't exist
create_wd(dataset_wd)


# Define the working directory
twitter_wd = "./twitter"

# Create twitter_wd if it doesn't exist
create_wd(twitter_wd)

# Set the working directory
setwd(twitter_wd)

# Source the helper functions script to load custom functions and load libraries
source('twitter_functions.R')

# Run the following command to add bearer token to the .Renviron file
# set_bearer()


############################## Set up ##########################################

# Download working directory
download_wd = "./partial results"
# Create the working directory if it doesn't exist
create_wd(download_wd)

# Load trading days if they don't already exist in working directory dataset
tdd <- trading_days(my_wd,dataset_wd)

# Specify keywords 
keywords_list <- fromJSON(file = "kwrds_tw.json")
keywords <- unique(unlist(keywords_list, use.names = F))


# Parameters
start_tweets = "2010-01-01T00:00:00Z"
end_tweets = "2021-07-07T00:00:00Z" 
country = "US"
lang = "en"
granularity = "day" # granularity for twitter search counts results
n=5000              # upper limit of tweet counts to be fetched
data_path = "data"  # fetched data stored as json just in case will be saved here, 
                    # will be created automatically, if it does not exist


########################## COUNT TWEETS ########################################  

# Run function count_all_tweets to find out the number of results 
# This function does not pull tweets --> does not use up the quota
# Produces dataframes with Date and Count of available tweets (for granularity "day")
# Very slow function, may take several hours!!

counts<-full_count(keywords,start_tweets,end_tweets,country,lang,granularity,n,download_wd)

# Sum up the counts per keyword into one dataframe
# We will use this information when downloading the actual data
# Empty dataframe
counts.only <- data.frame(matrix(ncol=2,nrow=length(counts), 
                                 dimnames=list(NULL, c("keyword", "results"))))


# Loop over the results 
for (i in 1: length(counts)){
  counts.only[i,1] <-names(counts)[i]            # first column keywords
  counts.only[i,2]<-sum(counts[[i]]$tweet_count) # second column sum of counts
}

# Save as RData file to download_wd working directory
save(counts.only, file = paste0(download_wd,"/counts.only.RData"))


########################## DOWNLOAD TWEETS ##################################### 

# The function get_all_tweets inside full_download is very fast
# Produces dataframes with text of tweets and a lot of metadata

tweets<-full_download(counts.only,start_tweets,end_tweets,country,lang,
                      granularity,data_path,download_wd)


############################## Process data #################################### 

# Bind tweets into 1 dataframe, add and rename several columns, replace emojis 
# with their description, convert time plus change time zone
tweets.df <- prepare_tweetsdf(tweets,keywords,keywords_list);rm(tweets);gc()

# Save   
setwd(my_wd) 
save(tweets.df, file = paste0(download_wd,"/tweets.df.RData"))


#### DATASET ADJUSTMENTS ####

# We are going to aggregate the data by events and by day
# Function takes dataframe as input, subsets columns defined in object nms,
# and then adds a column called "event" which is extracted from dictionary in json, the keywords_list
nms= c("Date","keyword","event","id","sent","anger","anticipation",
       "disgust","fear","joy","sadness","surprise","trust","alltext","cleantext")
# Add events to the dataframe
tw.df<-add.events(tweets.df,keywords_list,nms)

# Remove empty tweets
tw.df<-tw.df[!is.na(tw.df$alltext),]

# Save
save(tw.df, file = paste0(download_wd,"/tw.df.RData"))


