# LOAD AND BIND PREDICTION RESULTS TO ONE FILE
# Load results for individual stocks produced by the script model_flow.R


################################## Setup #######################################

# Load the helper functions shared_functions.R to get the function create_wd()
if (!require("pacman")) install.packages("pacman")
pacman::p_load(here)
source(here::here('shared_functions.R'))

# Move to working directory for models
models_wd = "./models"
setwd(models_wd)
# Store the current working directory
my_wd = getwd()

# Set up a working directory for storing results
results_wd = "./results"
# Create the results directory if it doesn't exist
create_wd(results_wd)

# Define the versions and dependent variables
versions = c("5w","5s","1w","1s")
depnums = c(1,5,22)


############################# Load, bind and save files ########################

# Loop through each version and dependent variable
for (vers in versions) {
  for (depnum in depnums) {
    
    print(paste("Loading prediction results for version",vers,depnum))
    
    
    # Load the results for the current version and dependent variable
    load_wd = paste("./files/hf_market_",vers,"_H",depnum,sep="")
    
    # Set the load_wd if it exists
    if (!dir.exists(load_wd)) {
      print(paste("Directory does not exist:", load_wd))
      next
    } else {
      print(paste("Loading from", load_wd))
      setwd(load_wd)
    }
    
    # Get the list of files in the current directory
    flist <- list.files()
    
    # If there are no files for this version of results, skip to the next iteration
    if (length(flist) == 0) {
      print(paste("No files found in", load_wd))
      next
    } else {
      print(paste("Found", length(flist), "files in", load_wd))
    }
    
    # Load one by one and delete "Data" in each step
    results<-list()
    for (f in 1:length(flist)){
      results[[flist[f]]]<-readRDS(flist[f])
      results[[flist[f]]][["Data"]]<-NULL
      gc()
    }
    
    # Store the results
    setwd(my_wd)
    
    # Define the filename for saving
    filename<-paste0("results_",vers,"_H",depnum)
    
    # Set the working directory to store the results
    setwd(results_wd)
    
    # Save as an RDS file
    saveRDS(results, file = filename)
    
    # RData version 2
    save(results,file=paste0(filename,".RData"),version = 2)
    
    # Return to the original working directory
    setwd(my_wd)
    
    # Print a message indicating that the file has been saved
    print(paste("Saved results to", file.path(results_wd, filename)))
  }
}


