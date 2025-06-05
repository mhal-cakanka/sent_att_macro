# GET SENTIMENT FOR TWEETS via the EMolex dictionary method
# Run in batches of rows defined by the following command line arguments

# Read in the arguments listed at the command line
  arg <- commandArgs(TRUE)
  from <- as.numeric(arg[1])
  by <- as.numeric(arg[2])
  
  print("arguments loaded")
  
  
############################## Setup #######################################
  
# Source the shared functions script to load custom functions and load libraries
  # We need to load the create_wd function 
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(here)
  source(here::here('shared_functions.R'))  # tw_funct.R will be called inside the script as well
  
# Move to "./twitter" folder
  # Define the working directory
  twitter_wd = "./twitter"
  
  # Create twitter_wd if it doesn't exist
  create_wd(twitter_wd)
  
  # Set the working directory
  setwd(twitter_wd)
  
# Define download_wd (where the data will be saved)
  download_wd="./data/sent"
  
# Create download_wd if it doesn't exist
  create_wd(download_wd)
  
  print("functions and libraries loaded")
  
  
# Load the full dataset with tweets  
  load("./partial results/tw.df.RData")
  keywords<-unique(tw.df$keyword)
  
  print("data loaded")
  
  
############################## Run sentiment extraction ########################
  
# Subset rows
  to=from+by-1
  if (to>nrow(tw.df)){
    to<-nrow(tw.df)
  }
  df<-tw.df[c(from:to),];rm(tw.df);gc()
  
  print(paste("data subset to",from,to,sep='_'))
  print("starting get_sent")
  
# Assign sentiment and emotions, add cleaned text  
  df <- get_sent(keywords,df)
  print("get_sent complete, starting add.clean.text")
  
  df <- add.clean.text(keywords,df)
  print("add.clean.text complete")
  
# Save
  saveRDS(df, file = paste0(download_wd,paste("/df",from,to, sep = "_")))
  print("end")

