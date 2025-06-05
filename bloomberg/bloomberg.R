# ANNOUNCEMENTS OF SCHEDULED MACROECONOMIC EVENTS
# DATA FROM BLOOMBERG 2006-01-01_2021-02-24
# SCRIPT PRODUCES DUMMY VARIABLE PER EACH TYPE OF EVENT
# Disclaimer: Raw data accessible only through a Bloomberg terminal

# Source the helper functions script to load custom functions and load libraries
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(here)
  source(here::here('shared_functions.R'))
  
# Dataset working directory (where the final dataset will be saved)
  dataset_wd = "./dataset"
  
# Create a working directory if it doesn't exist
  create_wd(dataset_wd)
  
# Define the working directory for Bloomberg data
  bloom_wd = "./bloomberg"
  
# Create bloom_wd if it doesn't exist
  create_wd(bloom_wd)
  
# Set the working directory
  setwd(bloom_wd)
  
# Source more helper functions and load libraries
  source('bloomberg_functions.R') 
  
  
############################## Load and process data ###########################

  # Load excel from Bloomberg
  econ <- read_excel(path = "econ_releases_complete.xlsx", sheet = "fixed")
  
  # Convert first column to date time 
  econ <- convert_date(econ)
  # Convert to dataframe
  econ <- as.data.frame(econ)

  # Filter econ news by importance
  econ <- filter(econ, (S >= 86) & (S<=100))
  # Remove "MBA Mortgage Applications" - no data on forecasts/estimations
  econ <- filter(econ, Event != "MBA Mortgage Applications")
  # Convert some columns to numeric
  colnums=which(names(econ) %in% c("Actual","Surv(A)", "Surv(H)", "Surv(L)", "Surv(M)", "Std Dev", "Surprise"))
  econ[,colnums] <- as.data.frame(lapply(econ[,colnums], as.numeric))
  
  # Change time-zone (set correct original tz UTC+3, then change to UTC-5)
  econ[,1] <- force_tz(econ[,1], "Etc/GMT-3")
  econ[,1] <- with_tz(econ[,1], "Etc/GMT+5")

  econ[is.na(econ)]<-0
  
  #  Working directory to save partial results
  partial_wd = "./partial results"
  # Create partial_wd if it doesn't exist
  create_wd(partial_wd)
  # Save file 
  save(econ, file = paste0(partial_wd,"/econ.RData"))
  

############################## Create variables ################################

  # 1. Dummy variables
  # Note: used in model HAR-M
  dummies <- make_econ_dummy(econ=econ, calendar='UnitedStates/NYSE', 
                             from1="2005-01-01",to1="2021-02-24", 
                             from2="2005-12-30", to2="2021-02-24",
                             opn="09:30:00", cls="16:30:00",
                             opt="dummy")

  # Checked which dates of announcements were non-trading days and manually 
  # confirmed the stock market was closed that day

  # The following variables are all set to the same dates as the dummy variables
  
  # 2. Number of estimates/forecasts 
  # Note: used as an attention variable in attention models
  est <- make_econ_dummy(econ=econ, calendar='UnitedStates/NYSE', 
                               from1="2005-01-01",to1="2021-02-24", 
                               from2="2005-12-30", to2="2021-02-24",
                               opn="09:30:00", cls="16:30:00",
                               opt="est")
  
  # 3. Standard deviation
  # Note: currently not used in the paper
  std <- make_econ_dummy(econ=econ, calendar='UnitedStates/NYSE', 
                         from1="2005-01-01",to1="2021-02-24", 
                         from2="2005-12-30", to2="2021-02-24",
                         opn="09:30:00", cls="16:30:00",
                         opt="std")
  
  # 4. Uncertainty
  # Note: currently not used in the paper
  unc <- make_econ_dummy(econ=econ, calendar='UnitedStates/NYSE', 
                         from1="2005-01-01",to1="2021-02-24", 
                         from2="2005-12-30", to2="2021-02-24",
                         opn="09:30:00", cls="16:30:00",
                         opt="unc")
  
  # 5. Range
  # Note: currently not used in the paper
  range <- make_econ_dummy(econ=econ, calendar='UnitedStates/NYSE', 
                         from1="2005-01-01",to1="2021-02-24", 
                         from2="2005-12-30", to2="2021-02-24",
                         opn="09:30:00", cls="16:30:00",
                         opt="range")
  
  
############################## Save final dataseqt ##############################  
  # Bind and rename
  # (only the first 10 events)
  bloomberg<-cbind(dummies[1:10],est[1:10],std[1:10],unc[1:10],range[1:10])
  
  nams<-c(paste("b",seq(1:10),sep="."),paste("e",seq(1:10),sep="."),
          paste("s",seq(1:10),sep="."),paste("u",seq(1:10),sep="."),
          paste("r",seq(1:10),sep="."))
  colnames(bloomberg)<-nams
  
  # Save individual datasets
  # Set wd to partial_wd
  setwd(partial_wd)
  save(dummies, file="dummies.RData")
  save(est, file="est.RData")
  save(std, file="std.RData")  
  save(unc, file="unc.RData")
  save(range, file="range.RData")
  
  # Save the full dataset
  setwd(my_wd)
  # Set working directory to the parent directory
  setwd('..')
  save(bloomberg, file=paste0(dataset_wd,"./bloomberg.RData"))
  