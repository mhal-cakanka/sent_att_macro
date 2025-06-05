# NEWSPAPER DATA
# Clean text files with newspaper data from ProQuest (proprietary data)


# Source the helper functions script to load custom functions and load libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(here)
source(here::here('shared_functions.R'))

# Dataset working directory (where the final dataset will be saved)
dataset_wd = "./dataset"

# Create a working directory if it doesn't exist
create_wd(dataset_wd)


# Define the working directory
proquest_wd = "./proquest"

# Create proquest_wd if it doesn't exist
create_wd(proquest_wd)

# Set the working directory
setwd(proquest_wd)

# Source the helper functions script to load custom functions and load libraries
source('proquest_functions.R')


############################## Set up ##########################################

# Download working directory
download_wd = "./partial results"
# Create the working directory if it doesn't exist
create_wd(download_wd)

# Load trading days if they don't already exist in working directory dataset
tdd <- trading_days(my_wd,dataset_wd)

# Specify keywords 
keywords_list <- fromJSON(file = "kwrds_pq.json")
keywords <- unique(unlist(keywords_list, use.names = F))

# Parameters for loading txt files from a folder
publication_titles <- "Financial Times_OR_Wall Street Journal (Online)"
location <- "US"
from="20100310"
to="20210224"
data_wd="./data"


############################## Load and process data ###########################

# READ IN articles in a text file, extract text of articles and metadata, 
# remove duplicates, replace emojis, convert time, save into a dataframe 
PQ<-get_allpq(keywords, publication_titles, location, from, to,data_wd)

# Save
save(PQ, file = paste0(download_wd,"/PQ.RData"))


#### DATASET ADJUSTMENTS ####

# We are going to aggregate the data by events and by day
# First, add column with the name of the corresponding event
# (proquest has columns "pub_date" --> rename to "Date"
#                       "article_id" --> rename to "id")
names(PQ)[names(PQ) == 'pub_date'] <- 'Date'
names(PQ)[names(PQ) == 'article_id'] <- 'id'

# Save
save(PQ, file = paste0(download_wd,"/PQ.RData"))

# Add events, keep only important columns
nms= c("Date","keyword","event","id","sent","anger","anticipation",
       "disgust","fear","joy","sadness","surprise","trust","alltext","cleantext")
pq.df<-add.events(PQ,keywords_list,nms)

# Save   
save(pq.df, file = paste0(download_wd,"/pq.df.RData"))




