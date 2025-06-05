# READ AND MERGE GT FILES
# This script reads Google Trends data, processes it, and saves the final dataset.
# Create a directory for the downloaded data if it doesn't exist.

# Source the helper functions script to load custom functions and load libraries
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(here)
  source(here::here('shared_functions.R'))
  
# Dataset working directory (where the final dataset will be saved)
  dataset_wd = "./dataset"
  
# Create a working directory if it doesn't exist
  create_wd(dataset_wd)

# Set the working directory
  google_wd = "./google"
  
# Create google_wd if it doesn't exist
  create_wd(google_wd)

# Set the working directory
  setwd(google_wd)

# Source the helper functions script to load custom functions and load libraries
  source('google_functions.R')
  
  
############################## Specify arguments ###############################
# Specify arguments for READING Google Trends
  
  # Download working directory (read files from this directory)
  download_wd="./data/Scale"
  
  # Create a working directory for the downloaded data if it doesn't exist
  create_wd(download_wd)
  
  # First day of interest
  start="2010-01-01"
  # Last day of interest
  end="2021-07-06"
  
  # Load trading days if they don't already exist in working directory dataset
  tdd <- trading_days(my_wd,dataset_wd)
  
  # Load names
  # Set working directory to the parent directory
  setwd('..')
  #  And save the current working directory
  parent_wd <- getwd()
  #  Load the names of keywords
  nams <- read_delim(paste0(dataset_wd,"/nams.csv"), ";", 
                     escape_double = FALSE, trim_ws = TRUE)
  nams<-as.data.frame(nams)
  kwrd.order<-nams$Keyword[1:10]
  all.kwrd.order<-c("General",kwrd.order)
  # Set working directory back to the original
  setwd(my_wd)
  
  
############################## Read  files #####################################
  
  # Read gt files (with given start and end dates), bind and tidy into dataframe
  GT <- gt_read(start=start, end=end, download_wd=download_wd)
  
  # Filter keywords
  keywords_list <- fromJSON(file = "kwrds_gt.json")
  kw<-unlist(keywords_list, use.names = F)
  GT<-GT[ , which(names(GT) %in% c(kw))]

  
############################## Process data #################################### 
# More changes
  
  # Rescale to 0-100
  GT <- normalize(GT)
  # Max value from friday to sunday assign to friday (also for holidays)
  GT<-max.weekends(td=tdd,df=GT)
  # Remove empty rows
  GT<-na.omit(GT)
  
  #  Working directory to save partial results
  partial_wd = "./partial results"
  # Create partial_wd if it doesn't exist
  create_wd(partial_wd)
  # Save file 
  save(GT, file = paste0(partial_wd,"/GT.RData"))

  
# Split data by events 
  events<-split.events(keywords_list,GT)
  # Order the events by all.kwrd.order
  events<-events[all.kwrd.order]
  
  # Remove keywords with negative correlation -> average 
  reduced<-lapply(events,neg.cor)
  # Take an average over remaining keywords per each event
  gt<-as.data.frame(lapply(reduced,rowMeans))
  
  
############################## Save final dataset ##############################  
  
  # Rename
  names(gt)<-c("gg.1",paste("ge",seq(1:10),sep='.'))
  
  # Save final file to a folder 'dataset'
  
  # Set working directory to the parent directory
  setwd(parent_wd)
  # Save
  save(gt, file = paste0(dataset_wd,"/gt.RData"))

  
  