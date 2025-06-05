# HELPER FUNCTIONS FOR SCRIPTS "gt.R" and "google.R"

# Load packages
if (!require("pacman")) install.packages("pacman")
# Check whether these libraries are installed, install if not and load all of them
pacman::p_load(gtrendsR,dplyr,tidyverse,timeDate,rjson,tidyr,scales,lubridate,quantmod)



# Save working directory
my_wd <- getwd()

######################### GOOGLE TRENDS ########################################

##################################### get_GT ###################################

# Function for downloading Google Trends - only 1 keyword
get_GT <- function(start_date, end_date, keyword, country=c("US"),tz=0){  
  # First day of interest
  S0 = as.Date(start_date)
  # Last day of interest
  S1 = as.Date(end_date)
  # Retrieve search terms
  print(paste(keyword, S0, S1, sep='_'))
  # Try to download GT
  try_error = tryCatch(gtrends(keyword, gprop = "web", geo = country,time = paste(S0,S1),
                               low_search_volume = TRUE, onlyInterest =TRUE, tz=tz*60), 
                       error = function(e) e)
  
  # Let's wait a minute
  Sys.sleep(60)					   
  
  # For no error, download
  if (!inherits(try_error, 'error')){
    
    GT <- try_error
    Sys.sleep(60)
    print("downloaded")
    print(GT)
    
  } else {
    # If there is an error, print the error message
    print(try_error)
    print(paste("problem with", keyword, S0, S1, sep='_'))
    # Let's wait a long time
    Sys.sleep(300)	
    
    # Lets try again?
    print("Lets try again?")
    # Try to download GT
    try_error = tryCatch(gtrends(keyword, gprop = "web", geo = country,time = paste(S0,S1),
                                 low_search_volume = TRUE, onlyInterest =TRUE, tz=tz*60), 
                         error = function(e) e)
    # Let's wait a minute
    Sys.sleep(60)					   
    
    # For no error, download
    if (!inherits(try_error, 'error')){
      GT <- try_error
      Sys.sleep(60)
      print("downloaded on a second try")
      
    } else {
      print(try_error)
      print(paste("problem with", keyword, S0, S1, sep='_'))
      return(NULL)
    }
  }	 				   
  
  
  # Let's wait a minute
  Sys.sleep(60)
  # Check if we have results - sometimes the terms might not be available
  if (is.null(GT)){
    print("Stopped function get_GT. No gt results were available. First if.")
    return(NULL)
  }
  if (is.null(GT[[1]])){
    print("Stopped function get_GT. No gt results were available. Second if.")
    return(NULL)
  }
  # We work with what we need
  GT = GT[[1]][,1:3]
  # Low search intensity volumes are coded as "<1" - we set it to 1
  GT[,2] = as.numeric(GT[,2])
  # We need to have the same Date format
  GT[,1] = as.Date(GT[,1],format="%Y-%m-%d")
  # Whenever the value is NOT available we set the value to 0
  GT[is.na(GT[,2]),2] = 0
  
  # rename column "date" to "time", so that we can filter the file
  colnames(GT) <- c("time","hits","keyword")
  # print(GT)
  
  print("processed")
  
  return(GT)
}



##################################### noscale_download #########################

# Function that downloads 1 or 2 GT files, binds them together. 
# No scaling, no merging.
# Wrapping function get_GT
noscale_download <- function(start, end, int_end, keyword, GT=NULL){
  # If there is no GT file from previous iterations, download first GT.
  if (is.null(GT)){
    GT <- get_GT(start_date=start, end_date=int_end, keyword=keyword)
    # Sometimes there are no results available. get_GT will return NULL.
    if(is.null(GT)){
      print("Coming out of function download.")
      return(NULL)
    }
    print("Downloaded first GT.")
  }
  
  # Get the next GT
  new_int_start <- int_end + 1
  new_int_end <- new_int_start+30
  next_GT <- get_GT(start_date=new_int_start, 
                    end_date=new_int_end, keyword=keyword)
  
  # Sometimes there are no results available. get_GT will return NULL.
  if(is.null(next_GT)){
    print("No results for the second GT.")
    next
  }
  print("Downloaded second GT.")
  
  # Bind the first and second GT into one.
  GT <- rbind(GT, next_GT)
  rownames(GT) <- NULL
  
  return(GT)
}


##################################### download #################################

# Function that downloads 1 or 2 GT files, rescales them and merges together
# Wrapping function get_GT
download <- function(start, end, int_end, keyword, GT=NULL, min.overlap){
  
  # If there is no GT file from previous iterations, download first GT.
  if (is.null(GT)){
    GT <- get_GT(start_date=start, end_date=int_end, keyword=keyword)
    # Sometimes there are no results available. get_GT will return NULL.
    if(is.null(GT)){
      print("Coming out of function download.")
      return(NULL)
    }
    print("Downloaded first GT.")
  }
  
  # We need to find a pair of non-zero values overlapping between two GT files.
  # First, find the first non-zero value in the first GT (starting from the last date).
  found.pair = FALSE
  # We start from the last date in the first GT and go backwards
  for (i in (min.overlap-1):268){
    #print(found.pair)
    print(paste("i is ", i, sep = ' '))
    new_int_start <- int_end - i
    new_int_end <- new_int_start+269
    if (new_int_end > end){
      new_int_end <- end 
    }
    last_hit <- filter(GT, time==new_int_start)[,2]
    # print(last_hit)
    
    # Second, the date of the first non-zero value we found
    # will be the start_date for the second GT.
    if(last_hit != 0){
      print(paste("Found non-zero value on iteration ", i, ". The value is ", 
                  last_hit, " and the date is ", new_int_start,  sep = ''))
      # download second GT, starting at the date were we found the non-zero value
      next_GT <- get_GT(start_date=new_int_start, 
                        end_date=new_int_end, keyword=keyword)
      # Sometimes there are no results available. get_GT will return NULL.
      if(is.null(next_GT)){
        print("No results for the second GT.")
        next
      }
      print("Downloaded second GT.")
      first_hit <- next_GT[1,2]
      first_ten<-next_GT[c(seq(1, (i+1))),2]
      
      # Third, check whether any of the first ten values is non-zero
      
      # if(first_hit != 0){
      if (sum(first_ten)>0){
        # print(paste("The first value in the second GT is also non-zero. It is", 
        #             first_hit, ". Success we can scale and merge them!", sep = ' '))
        print(paste("There was at least one non-zero value in the overlap in second GT.", 
                    sum(first_ten), ". Success we can scale and merge them!", sep = ' '))
        
        # Calculate scaling ratio.
        # Overlap
        GT_overlap=GT[c(seq((nrow(GT)-i), nrow(GT))),2]
        next_GT_overlap=first_ten
        
        scaling <- mean(GT_overlap)/mean(next_GT_overlap)
        print(paste("scaling is ", scaling, sep = ' '))
        
        # Rescale the second GT.
        next_GT[,2] <- next_GT[,2] * scaling
        
        # Cut off overlapping values from the second GT.
        print(paste("Overlap is ", i, sep = ' '))
        next_GT <- next_GT[-c(seq(1, (i+1))),]
        
        # Bind the first and second GT into one.
        GT <- rbind(GT, next_GT)
        rownames(GT) <- NULL
        print("Scaled and merged GT.")
        
        found.pair = TRUE
        break
      } else {
        print(paste("First value in the second GT file was", first_hit, sep = ' '))
        next
      }
    } else {
      if ((i==268)&(found.pair == FALSE)){
        print(found.pair)
        print(paste("Was not able to find a pair of non-zero values. Try other keyword."))
        return(GT)
        next
      }
    }
  }
  return(GT)
}


##################################### gt_download ############################## 

# Function that downloads google trends
# input: only 1 keyword, start and end, country
# Wrapping function download

gt_download <- function(start, end, keyword, download_wd, wd=my_wd, opt="scale",
                        min.overlap=1, GT=NULL){
  start=as.Date(start)
  end=as.Date(end)
  
  # If we have preexisting data, set int_end date as the last in this dataset
  if (!is.null(GT)){
    int_end=max(GT$time)
    int_end=as.Date(int_end)
  } else {
    int_end=start+269
  }
  
  while(int_end < end){
    # Do you want to scele and merge?
    if (opt=="scale"){
      GT <- download(start, end, int_end, keyword, GT,min.overlap)
    }
    if (opt=="noscale"){
      GT <- noscale_download(start, end, int_end, keyword, GT)
    }
    
    if (is.null(GT)){
      print("Coming out of While loop.")
      break
    } else {
      int_end <- GT[length(GT[,1]),1]
      print(paste(keyword, start, int_end, "done", sep = '_'))
    }
  }
  
  setwd(download_wd)
  saveRDS(GT, file = paste(keyword,start,end, sep = "_"))
  setwd(wd)
  return(GT)
}


##################################### gt_read ##################################

# Function for reading gt files. The output is a dataframe.
gt_read <- function(start, end, download_wd, wd=my_wd){
  # Set working directory to the folder with downloaded files
  setwd(download_wd)
  # Read all files that include a pattern "start_end" and bind them
  GT <- list.files(pattern = paste(start, end, sep = '_')) %>%
    map_dfc(readRDS)
  # Return to the original wd
  setwd(my_wd)
  # Tidy the dataframe
  col_names <- GT[1,] %>% 
    dplyr::select(starts_with("keyword")) %>% 
    unlist(use.names = F)
  row_names <- as.Date((GT[,1]), origin="1970-01-01")
  # Select google trends "hits" =  number of searches per each keyword
  GT <- GT %>% dplyr::select(starts_with("hits"))
  # Change column names to keywords and row names to dates
  colnames(GT) <- col_names
  row.names(GT) <- row_names
  return(GT)
}


######################## normalize #############################################

# Normalize google trends to a scale of 0:100
normalize <- function(GT_file){
  for (column in seq(1,length(GT_file))){
    GT_file[,column] <- scales::rescale(GT_file[,column], to = c(0, 100)) 
  }
  return(GT_file)
}






  
