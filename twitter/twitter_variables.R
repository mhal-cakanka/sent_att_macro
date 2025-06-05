# PROCESS TWITTER DATA ACCESSED VIA ACADEMIC API INTO ATTENTION AND SENTIMENT VARS
# Disclaimer: Academic API is no longer available, code showcases, how was data acquired and processed
# Download and simple processing of Twitter data was done in "get_twitter.R" script

# This script uses 3 methods to extract sentiment 
# You will have to go back and forth to run other scripts in the middle of this script
# In the end, we will aggregate all variables into one dataframe


# Source the helper functions script to load custom functions and load libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(here)
source(here::here('shared_functions.R'))

# Dataset working directory (where the final dataset will be saved)
dataset_wd = "./dataset"

# Create a working directory if it doesn't exist
create_wd(dataset_wd)


# Set the working directory
twitter_wd = "./twitter"

# Create twitter_wd if it doesn't exist
create_wd(twitter_wd)

# Set the working directory
setwd(twitter_wd)

# Save working directory
my_wd <- getwd()


############################## Load data #######################################

# Download working directory
download_wd = "./partial results"
# Create the working directory if it doesn't exist
create_wd(download_wd)

# Load pre-processed twitter data
load(paste0(download_wd,"/tw.df.RData"))


# Specify keywords 
keywords_list <- fromJSON(file = "kwrds_tw.json")
keywords <- unique(unlist(keywords_list, use.names = F))


############################## Sentiment analysis ##############################

############################## EMOLEX MANUAL ###################################

# Takes a few hours/days!
# Comment out the lines below if you want to run the script locally
#   # Extract sentiment and 8 emotions (by document)
#   st <- Sys.time()
#   tw.df.emo <- get_sent(keywords,tw.df)
#   Sys.time() - st
#   
# # Add column with cleaned text
#   tw.df.emo <- add.clean.text(keywords,tw.df.emo)
# 
# # Save   
#   save(tw.df.emo, file = paste0(download_wd,"/tw.df.emo.RData"))


############################## EMOLEX METACENTRUM ##############################


# Go and run the script tw_sent.R in batches using script tw.sent.sh
# You can use something like the script submit.sh, to run batches of 10 thousand rows at a time


# Come back and bind files that were downloaded via the metacentrum script tw_sent.R
# Go to the folder where the results are saved
sent_wd="./data/sent"; setwd(sent_wd)

# Load all files
result_files=str_sort(list.files(pattern = paste("df","_",sep='')), numeric = TRUE)
tw.df.emo <- result_files %>% map(readRDS) %>% bind_rows();gc()
setwd(my_wd)

# Save   
save(tw.df.emo, file = paste0(download_wd,"/tw.df.emo.RData"))

# Bind to tw.df if ids are identical
if (!identical(tw.df$id,tw.df.emo$id)){
  stop("IDs are not identical, make sure all rows are present in both datasets")
} else {
  print("IDs are identical, binding emolex results to tw.df dataset")
  tw.df.fin <- cbind(tw.df[,c("Date","keyword","event","id")], 
                 tw.df.emo[,c("alltext","cleantext","sent", "anger", "anticipation", 
                              "disgust","fear","joy","sadness","surprise","trust")])
  rm(tw.df.emo);gc()
}

# Save   
save(tw.df.fin, file = paste0(download_wd,"/tw.df.fin.RData"))


############################# PREPARE FOR FINBERT ##############################

# Create volatile corpus
corp <- VCorpus(VectorSource(tw.df$alltext)); gc()
save(corp, file = paste0(download_wd,"/corp.RData"))

# Clean corpus
corp.clean <- removeurl_opt(corp); rm(corp); gc()
save(corp.clean, file = paste0(download_wd,"/corp.clean.RData"))

# Get and save cleaned text
cleaned.text <-unlist(sapply(corp.clean, `[`, "content"))

# Add column with cleaned text
tw.df$clean<-cleaned.text; rm(corp.clean,cleaned.text); gc()

# Add index
tw.df$idx<-seq(1:nrow(tw.df))

# Select only necessary columns
tw.df.finbert<-tw.df[,c("idx","id","clean")];colnames(tw.df.finbert)=c("idx","id","text")

# Save
save(tw.df.finbert, file = paste0(download_wd,"/tw.df.finbert.RData"))
write.csv(tw.df.finbert,file = paste0(download_wd,"/tw_text_clean.csv"),row.names = F)


# Set working directory to the parent directory
setwd('..')
#  And save the current working directory
parent_wd <- getwd()
# Finbert working directory
finbert_wd = "./finbert"
# Create finbert_wd if it doesn't exist
create_wd(finbert_wd)
# Also save the csv to a finbert folder
write.csv(tw.df.finbert,file = paste0(finbert_wd,"/tw_text_clean.csv"),row.names = F)
# Set wd back to twitter
setwd(my_wd)


############################## LOAD FINBERT RESULTS ############################

# Set working directory to the parent directory
setwd(parent_wd)
# Set this option to be able to work with long ids in the dataset
options(scipen=999)

# Load list of csv files
list_of_files <- list.files(path = "finbert/twitter_out", recursive = TRUE,
                            pattern = "\\.csv$",full.names = TRUE)
# Check the list is not empty
if (length(list_of_files) == 0) {
  stop("No CSV files found in the specified directory.")
}

# Read all CSV files into a dataframe
finbert.sent <- ldply(list_of_files, read_csv, col_types = cols(id = col_character()), show_col_types = FALSE)

# Remove the first column (unnecessary)
finbert.sent <- finbert.sent[,-1]
finbert.sent <- finbert.sent[order(finbert.sent$idx),]
finbert.sent$Sentiment=(finbert.sent$Positive-finbert.sent$Negative)/(finbert.sent$Positive+finbert.sent$Negative)
finbert.sent<-as.data.frame(finbert.sent)

# Ids and Idxs got mixed up, the rownames end up being the idx
# We need to fix that
if (identical(as.numeric(rownames(tw.df.finbert)),finbert.sent$idx)){
  finbert.sent$correct_idx<-tw.df.finbert$idx
  finbert.sent$correct_id<-tw.df.finbert$id
}
  

# Save
setwd(my_wd)
save(finbert.sent, file = paste0(download_wd,"/finbert.sent.RData"))
write.csv(finbert.sent,"finbert.sent.csv")

# Bind with existing dataset
tw.df.fin<-cbind(tw.df.fin,finbert.sent[,c("Positive","Negative","Neutral","Sentiment")])
# Save
save(tw.df.fin, file = paste0(download_wd,"/tw.df.fin.RData"))
rm(tw.df.finbert,finbert.sent);gc()


############################## VADER ###########################################

# Apply vader lexicon, bind with dataset
tw.df.vader = vader_df(tw.df$cleantext, incl_nt = T, neu_set = T, rm_qm = F);gc()
# Replace NAs with zeros
tw.df.vader[which(is.na(tw.df.vader$compound)),c("compound","pos","neu","neg","but_count")]<-0
# Bind with existing dataset
tw.df.fin=cbind(tw.df.fin,tw.df.vader[,c("compound","pos","neu","neg")])

# Save
save(tw.df.vader, file = paste0(download_wd,"/tw.df.vader.RData"))
save(tw.df.fin, file = paste0(download_wd,"/tw.df.fin.RData"))
rm(tw.df.vader);gc()



############################## Aggregate ####################################### 

# Next, remove duplicated tweets within each event
tw.df.unique<-duplicates(tw.df.fin)

# Save   
save(tw.df.unique, file = paste0(download_wd,"/tw.df.unique.RData"))
rm(tw.df,tw.df.fin);gc()


# Load names
# Set working directory to the parent directory
setwd(parent_wd)
#  Load the names of keywords
nams <- read_delim(paste0(dataset_wd,"/nams.csv"), ";", 
                   escape_double = FALSE, trim_ws = TRUE)
nams<-as.data.frame(nams)
kwrd.order<-nams$Keyword[1:10]
all.kwrd.order<-c("General",kwrd.order)
new.kwrd.order=gsub(' ', '.',gsub('[(]', '.',gsub('[)]', '.', all.kwrd.order)))
all.kwrd.order=new.kwrd.order
# Set working directory back to the original
setwd(my_wd)



# Aggregate results using a loop
agreg <- "events"
start <- "2010-03-10"
end <- "2021-02-24"

# Define configurations for aggregation
configs <- list(
  list(name = "tw.attentions", opt = "volume", col_select = c("anger", "disgust", "fear")),
  list(name = "tw.neg", opt = "mean", col_select = c("anger", "disgust", "fear")),
  list(name = "tw.pos", opt = "mean", col_select = c("joy", "trust")),
  list(name = "tw.sent", opt = "mean", col_select = c("sent")),
  list(name = "tw.vsent", opt = "mean", col_select = c("compound")),
  list(name = "tw.vpos", opt = "mean", col_select = c("pos")),
  list(name = "tw.vneg", opt = "mean", col_select = c("neg")),
  list(name = "tw.fsent", opt = "mean", col_select = c("Sentiment")),
  list(name = "tw.fpos", opt = "mean", col_select = c("Positive")),
  list(name = "tw.fneg", opt = "mean", col_select = c("Negative"))
)

# Initialize an empty list to store results
results <- list()

# Loop through configurations and aggregate results
for (config in configs) {
  print(config$name)
  results[[config$name]] <- aggregate_daily(
    df = tw.df.unique,
    agreg = agreg,
    start = start,
    end = end,
    opt = config$opt,
    all.kwrd.order = all.kwrd.order,
    col_select = config$col_select
  )
}

# Combine all results into one dataframe
tw <- do.call(cbind, results)

# Rename columns
colnames(tw) <- c(
  "tg.1", paste("te", seq(1, 10), sep = '.'),   # volume
  "tg.2", paste("te", seq(11, 20), sep = '.'),  # sentiment
  "tg.3", paste("te", seq(21, 30), sep = '.'),  # pos emotions
  "tg.4", paste("te", seq(31, 40), sep = '.'),  # neg emotions
  "tg.5", paste("te", seq(41, 50), sep = '.'),  # vader compound sentiment
  "tg.6", paste("te", seq(51, 60), sep = '.'),  # vader pos sentiment
  "tg.7", paste("te", seq(61, 70), sep = '.'),  # vader neg sentiment
  "tg.8", paste("te", seq(71, 80), sep = '.'),  # finbert sentiment
  "tg.9", paste("te", seq(81, 90), sep = '.'),  # finbert pos sentiment
  "tg.10", paste("te", seq(91, 100), sep = '.') # finbert neg sentiment
)

# Load trading days if they don't already exist in working directory dataset
tdd <- trading_days(my_wd,dataset_wd)

# Get the maximum value from weekend and assign to friday
tw <-max.weekends(td=tdd,tw)
tw <- na.omit(tw)

# Save
setwd(parent_wd)
save(tw, file = paste0(dataset_wd,"/tw.RData"))
