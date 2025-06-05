# DOWNLOAD DATA FROM WIKI TRENDS


################################# Load functions ###############################

# Source the helper functions script to load custom functions and load libraries
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(here)
  source(here::here('shared_functions.R'))

# Dataset working directory (where the final dataset will be saved)
  dataset_wd = "./dataset"

# Create a working directory if it doesn't exist
  create_wd(dataset_wd)


# Set the working directory
  wiki_wd = "./wiki"

# Create wiki_wd if it doesn't exist
  create_wd(wiki_wd)

# Set the working directory
  setwd(wiki_wd)

# Source the helper functions script to load custom functions and load libraries
  source('wiki_functions.R')



############################## Specify arguments ###############################
# Specify arguments

  # First day of interest (available from "2007-12-10")
  # Note that there is no data prior to 2007-12-01
  start="2010-01-01"
  # Last day of interest
  end="2021-07-06"
  # Name of the final dataframe 
  file_name = "WIKI"
  # Download working directory
  download_wd = "./data"
  # Create a working directory for the downloaded data if it doesn't exist
  create_wd(download_wd)
  
  # Load trading days if they dont already exist in working directory dataset
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

# Keywords 
  
  # Load list of keywords and unlist them into a single vector
  keywords_list <- fromJSON(file = "kwrds_wiki.json")
  keywords_list <- lapply(keywords_list,tolower)
  keywords <- unique(unlist(keywords_list, use.names = F))
  
  
############################## Download  wiki ################################## 
# Download Wiki Trends
  
  # Wiki trends will be rearranged - any dates with missing values are filled with NAs
  WIKI <- download_wiki(keywords = keywords, lang = "en", start = start, end = end, 
                        download_wd = download_wd, wd = my_wd, file_name = file_name)
  # There are some issues with downloading wiki trends. 
  # Sometimes a half of the time period is not downloaded at all.
  
  
  #  Working directory to save partial results
  partial_wd = "./partial results"
  # Create partial_wd if it doesn't exist
  create_wd(partial_wd)
  # Save file 
  save(WIKI, file = paste0(partial_wd,"/WIKI.RData"))
  

############################## Clean data ###################################### 

  # Remove columns with a lot of missing data (some wiki pages dont have data after 2016)
  WIKI.r<-WIKI[ , colSums(is.na(WIKI)) <= 300]

  # Fill NAs with the last known value, loop over columns 
  for (i in 1:ncol(WIKI.r)){
    WIKI.r[,i] <-replace_na_with_last(WIKI.r[,i])
  }
  
  # Max value from friday to sunday assign to friday (also for holidays)
  WIKI.r<-max.weekends(td=tdd,df=WIKI.r)
  WIKI.r<-na.omit(WIKI.r)
  
  # Inspect the remaining data for obviously missing data/gaps 
  for (i in seq(1,ncol(WIKI.r),by=10)){
    print(i)
    plot.ts(WIKI.r[i:(i+9)])
  }
  
  
############################## Process data ####################################

  # Split by events
  events<-split.events(keywords_list,WIKI.r)
  # lets put general as first
  events<-events[all.kwrd.order]
  # Remove keywords with negative correlation
  events<-lapply(events,neg.cor)
  
  # Save files  
  save(WIKI, file = paste0(partial_wd,"/WIKI.RData"))
  save(WIKI.r, file = paste0(partial_wd,"/WIKI.r.RData"))
  save(events, file = paste0(partial_wd,"/events.RData"))
  
  # Average
  wiki<-as.data.frame(lapply(events,rowMeans))


############################## Save final dataset ##############################  
  
  # Rename
  names(wiki)<-c("wg.1",paste("we",seq(1:10),sep='.'))
  
  # Set working directory to the parent directory
  setwd(parent_wd)
  # Save final file to 'dataset'
  save(wiki, file = paste0(dataset_wd,"/wiki.RData"))

 
  