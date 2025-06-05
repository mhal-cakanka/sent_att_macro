# DOWNLOAD DATA FROM GOOGLE TRENDS

# The script downloads Google Trends data for a specific keyword and saves it in a specified directory.
# Creates a directory for the downloaded data if it doesn't exist.
# Disclaimer: Rate limits have been increased since, you may run into "Status code was not 200. Returned status code:429"
#             Make sure to not run too many requests per day, try a VPN etc.


# The script can be run in two ways:
# A) Run a job with the gt.sh script to download Google Trends data for a specific keyword
# We use the Metacentrum grid service to run a batch of jobs in parallel
# or B) run the gt.R script directly in RStudio to download data for a specific keyword


# Read in the arguments listed at the command line
  arg <- commandArgs(TRUE)
  i <- as.numeric(arg[1])
  opt <- arg[2]
  min.overlap <- as.numeric(arg[3]) # should be at least 10 days
  print(paste("Arguments loaded",i, min.overlap))
  
  
# Or run the script directly in RStudio
# Uncomment the following lines to run the script directly in RStudio
  # i <- 1            # Index of the keyword to download
  # opt <- "scale"
  # min.overlap <- 10
  
  # Source shared helper functions
  # source(here::here('shared_functions.R'))
  # Set the working directory
  # google_wd = "./google"
  # Create google_wd if it doesn't exist
  # create_wd(google_wd)
  # setwd(google_wd)

  
# Source the helper functions script to load custom functions and load libraries
  source('google_functions.R')
  print("Functions loaded.")
  

# Specify arguments for downloading Google Trends
  # First day of interest
  start="2010-01-01"
  # Last day of interest
  end="2021-07-06"
  # Download working directory
  if (opt=="scale"){
	download_wd="./data/Scale"
	}
  if (opt=="noscale"){
	download_wd="./data/Noscale"
  }
  # Create a working directory for the downloaded data if it doesn't exist
  create_wd(download_wd)
  
  
# Load list of keywords and unlist them into a single vector
  keywords_list <- fromJSON(file = "kwrds_gt.json")
  keywords <- unique(unlist(keywords_list, use.names = F))
  
  # Print the keyword to be downloaded
  print(paste("Downloading data for keyword:", keywords[i]))

  
# Run the gt_download function  
  gt_download(start=start, end=end,keyword=keywords[i],download_wd=download_wd, 
              wd=my_wd, opt=opt, min.overlap=min.overlap)
