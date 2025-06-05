# NEWSPAPER DATA
# Process newspaper data from ProQuest (proprietary data)
# into attention and sentiment variables (3 methods for sentiment analysis)
# Loading and simple processing of Newspaper data was done in "get_proquest.R" script


# Source the helper functions script to load custom functions and load libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(here)
source(here::here('shared_functions.R'))   # tw_funct.R will be called inside the script as well


# Dataset working directory (where the final dataset will be saved)
dataset_wd = "./dataset"

# Create a working directory if it doesn't exist
create_wd(dataset_wd)


# Set the working directory
proquest_wd = "./proquest"

# Create twitter_wd if it doesn't exist
create_wd(proquest_wd)

# Set the working directory
setwd(proquest_wd)

# Save working directory
my_wd <- getwd()


############################## Load data #######################################

# Download working directory
download_wd = "./partial results"
# Create the working directory if it doesn't exist
create_wd(download_wd)

# Load pre-processed newspaper data
load(paste0(download_wd,"/pq.df.RData"))

# Specify keywords 
keywords_list <- fromJSON(file = "kwrds_pq.json")
keywords <- unique(unlist(keywords_list, use.names = F))


############################## Sentiment analysis ##############################

############################## EMOLEX MANUAL ###################################


# Extract sentiment and 8 emotions (by document)
pq.df.emo <- get_sent(keywords,pq.df)

# Add column with cleaned text
pq.df.emo <- add.clean.text(keywords,pq.df.emo)

# Save   
save(pq.df.emo, file = paste0(download_wd,"/pq.df.emo.RData"))



# Bind to pq.df if ids are identical
if (!identical(pq.df$id,pq.df.emo$id)){
  stop("IDs are not identical, make sure all rows are present in both datasets")
} else {
  print("IDs are identical, binding emolex results to pq.df dataset")
  pq.df.fin <- cbind(pq.df[,c("Date","keyword","event","id")], 
                     pq.df.emo[,c("alltext","cleantext","sent", "anger", "anticipation", 
                                  "disgust","fear","joy","sadness","surprise","trust")])
  rm(pq.df.emo);gc()
}

# Save   
save(pq.df.fin, file = paste0(download_wd,"/pq.df.fin.RData"))


############################# PREPARE FOR FINBERT ##############################

# Create volatile corpus
corp <- VCorpus(VectorSource(pq.df$alltext)); gc()
save(corp, file = paste0(download_wd,"/corp.RData"))

# Clean corpus
corp.clean <- removeurl(corp); rm(corp); gc()
save(corp.clean, file = paste0(download_wd,"/corp.clean.RData"))

# Get and save cleaned text
cleaned.text <-unlist(sapply(corp.clean, `[`, "content"))

# Add column with cleaned text
pq.df$clean<-cleaned.text; rm(corp.clean,cleaned.text); gc()

# Add index
pq.df$idx<-seq(1:nrow(pq.df))

# Remove duplicated news within each event
pq.df<-duplicates(pq.df)
pq.df.unique<-duplicates(pq.df.fin)
# Save   
save(pq.df.unique, file = paste0(download_wd,"/pq.df.unique.RData"))


# Select only necessary columns
pq.df.finbert<-pq.df[,c("idx","id","clean")];colnames(pq.df.finbert)=c("idx","id","text")


# Save
save(pq.df.finbert, file = paste0(download_wd,"/pq.df.finbert.RData"))
write.csv(pq.df.finbert,file = paste0(download_wd,"/pq_text_clean.csv"),row.names = F)


# Set working directory to the parent directory
setwd('..')
#  And save the current working directory
parent_wd <- getwd()
# Finbert working directory
finbert_wd = "./finbert"
# Create finbert_wd if it doesn't exist
create_wd(finbert_wd)
# Also save the csv to a finbert folder
write.csv(pq.df.finbert,file = paste0(finbert_wd,"/pq_text_clean.csv"),row.names = F)
# Set wd back to proquest
setwd(my_wd)



############################## LOAD FINBERT RESULTS ############################

# Set working directory to the parent directory
setwd(parent_wd)
# Set this option to be able to work with long ids in the dataset
options(scipen=999)

# Load list of csv files
list_of_files <- list.files(path = "finbert/proquest_out", recursive = TRUE,
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
if (identical(as.numeric(rownames(pq.df.finbert)),finbert.sent$idx)){
  finbert.sent$correct_idx<-pq.df.finbert$idx
  finbert.sent$correct_id<-pq.df.finbert$id
}


# Save
setwd(my_wd)
save(finbert.sent, file = paste0(download_wd,"/finbert.sent.RData"))
write.csv(finbert.sent,"finbert.sent.csv")

# Bind with existing dataset
pq.df.unique<-cbind(pq.df.unique,finbert.sent[,c("Positive","Negative","Neutral","Sentiment")])
# Save
save(pq.df.unique, file = paste0(download_wd,"/pq.df.unique.RData"))
rm(pq.df.finbert,finbert.sent);gc()


############################## VADER ###########################################

# Apply vader lexicon, bind with dataset
pq.df.vader = vader_df(pq.df.unique$cleantext, incl_nt = T, neu_set = T, rm_qm = F);gc()
# Replace NAs with zeros
pq.df.vader[which(is.na(pq.df.vader$compound)),c("compound","pos","neu","neg","but_count")]<-0
# Bind with existing dataset
pq.df.unique=cbind(pq.df.unique,pq.df.vader[,c("compound","pos","neu","neg")])

# Save
save(pq.df.vader, file = paste0(download_wd,"/pq.df.vader.RData"))
save(pq.df.unique, file = paste0(download_wd,"/pq.df.unique.RData"))
rm(pq.df.vader,pq.df,pq.df.fin);gc()


############################## Aggregate ####################################### 


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
  list(name = "pq.attentions", opt = "volume", col_select = c("anger", "disgust", "fear")),
  list(name = "pq.neg", opt = "mean", col_select = c("anger", "disgust", "fear")),
  list(name = "pq.pos", opt = "mean", col_select = c("joy", "trust")),
  list(name = "pq.sent", opt = "mean", col_select = c("sent")),
  list(name = "pq.vsent", opt = "mean", col_select = c("compound")),
  list(name = "pq.vpos", opt = "mean", col_select = c("pos")),
  list(name = "pq.vneg", opt = "mean", col_select = c("neg")),
  list(name = "pq.fsent", opt = "mean", col_select = c("Sentiment")),
  list(name = "pq.fpos", opt = "mean", col_select = c("Positive")),
  list(name = "pq.fneg", opt = "mean", col_select = c("Negative"))
)

# Initialize an empty list to store results
results <- list()

# Loop through configurations and aggregate results
for (config in configs) {
  results[[config$name]] <- aggregate_daily(
    df = pq.df.unique,
    agreg = agreg,
    start = start,
    end = end,
    opt = config$opt,
    all.kwrd.order = all.kwrd.order,
    col_select = config$col_select
  )
}

# Combine all results into one dataframe
pq <- do.call(cbind, results)

# Rename columns
colnames(pq) <- c(
  "pq.1", paste("pe", seq(1, 10), sep = '.'),   # volume
  "pq.2", paste("pe", seq(11, 20), sep = '.'),  # sentiment
  "pq.3", paste("pe", seq(21, 30), sep = '.'),  # pos emotions
  "pq.4", paste("pe", seq(31, 40), sep = '.'),  # neg emotions
  "pq.5", paste("pe", seq(41, 50), sep = '.'),  # vader compound sentiment
  "pq.6", paste("pe", seq(51, 60), sep = '.'),  # vader pos sentiment
  "pq.7", paste("pe", seq(61, 70), sep = '.'),  # vader neg sentiment
  "pq.8", paste("pe", seq(71, 80), sep = '.'),  # finbert sentiment
  "pq.9", paste("pe", seq(81, 90), sep = '.'),  # finbert pos sentiment
  "pq.10", paste("pe", seq(91, 100), sep = '.') # finbert neg sentiment
)

# Load trading days if they don't already exist in working directory dataset
tdd <- trading_days(my_wd,dataset_wd)

# Get the maximum value from weekend and assign to friday
pq <-max.weekends(td=tdd,pq)
pq <- na.omit(pq)

# Save
setwd(parent_wd)
save(pq, file = paste0(dataset_wd,"/pq.RData"))

