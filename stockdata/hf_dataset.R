# PROCESS HIGH FREQUENCY PRICE DATA FROM FIRST RATE DATA files


# This script is for processing a batch of stocks into high frequency data
# Defined by from and to arguments and the list of symbols
# We run it in batches of 20 stocks at a time, due to memory and time constraints
# Comment out these lines and define "from", and "to" manually, to run locally

# Load command line arguments
arg <- commandArgs(TRUE)
from <- as.numeric(arg[1])
to <- as.numeric(arg[2])    


################################# Load functions ###############################

# Source the helper functions script to load custom functions and load libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(here)
source(here::here('shared_functions.R'))

# Set the working directory
stockdata_wd = "./stockdata"

# Create stockdata_wd if it doesn't exist
create_wd(stockdata_wd)

# Set the working directory
setwd(stockdata_wd)

# Source the helper functions script to load custom functions and load libraries
source('stockdata_functions.R')

print("functions loaded")


################################# Symbols and splits ###########################

# Download the web-page of a specific version of S&P500 constituents
url <- "https://en.wikipedia.org/w/index.php?title=List_of_S%26P_500_companies&oldid=1106065758"

# Load the file with symbols if it exists
if (file.exists("symbols.RData")) {
  load("symbols.RData")
} else {
  # If the file doesn't exist, create it
  symbols <- list_symbols(url)
  save(symbols, file = "symbols.RData")
}

# Create a list of splits for the stocks file from FirstRate Data
# Comes in a folder of txt files, is processed into a list of dataframes
directory_path = "stock_splits"
startdate = "2010-03-10"
enddate = "2021-02-24"


# Load the file with splits if it exists
if (file.exists("split.list.RData")) {
  load("split.list.RData")
} else {
  # If the file doesn't exist, create it
  split.list <- make_split_list(directory_path, symbols,startdate,enddate)
  save(split.list, file = "split.list.RData")
}


############################# Working directories ##############################

# Loop over these stocks, process into RV, join with attention & sentiment data


# Paths we will be using, create if they do not exist
path_dt="./data/dts/"  ; create_wd(path_dt)       # store 1 minute OHLC and returns
path_1hf="./data/1hfs/"; create_wd(path_1hf)      # store 1 minute volatility measures
path_5hf="./data/5hfs/"; create_wd(path_5hf)      # store 5 minute volatility measures
path_stocks="./stocks" ; create_wd(path_stocks)   # path to raw data 
 

################################# Process ######################################

# Measure time 
A = Sys.time()
print(paste(from,to, sep=':'))


# 1 min OHLC and returns
# Loads and processes raw stock price data from FirstRate Data
dts<-minute_hf(symbols[from:to], wd=path_stocks, my_wd=my_wd)
# Define name of the data file and assign
datName<-paste("dts",from,to, sep='_')
assign(datName, dts)
# Save the data to a file and clean up
save(list=datName, file = paste0(path_dt, paste(datName, "RData", sep = ".")))
rm(list=c(datName));gc()



# 1 minute volatility measures
hf1<-daily_hf(dts=dts,W=501,MR=1502,filenam="hf",savefile=FALSE, sampling=1, split.list=split.list)
# Define name of the data file and assign
hf1Name<-paste("hf1",from,to, sep='_')
assign(hf1Name, hf1)
# Save the data to a file and clean up
saveRDS(hf1, file=paste0(path_1hf, hf1Name))
rm(list=c(hf1Name));rm(hf1);gc()

Sys.time()-A



# 5 minute volatility measures
hf5<-daily_hf(dts=dts,W=501,MR=1502,filenam="hf",savefile=FALSE, sampling = 5,split.list=split.list)
# Define name of the data file and assign
hf5Name<-paste("hf5",from,to, sep='_')
assign(hf5Name, hf5)
# Save the data to a file and clean up
saveRDS(hf5, file=paste0(path_5hf, hf5Name))
rm(list=c(hf5Name));rm(dts,hf5);gc()

Sys.time()-A

# Print final message
print("Finished processing stocks")
