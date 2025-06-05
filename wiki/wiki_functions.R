# HELPER FUNCTIONS FOR SCRIPT "wiki.R"


# Load necessary libraries
if (!require("pacman")) install.packages("pacman")
# Check whether these libraries are installed, install if not and load all of them
pacman::p_load(wikipediatrend,dplyr,tidyverse,timeDate,rjson,tidyr,scales,lubridate,quantmod)



# Save working directory
my_wd <- getwd()


######################### WIKI TRENDS ##########################################

##################################### download_wiki ############################

# Define function for downloading wiki trends 
# Uses function rearange
download_wiki <- function(keywords, lang = "en", start, end, 
                          download_wd, wd = my_wd, file_name){
  # Set start and end as date
  start <- as.Date(start) 
  end <- as.Date(end) 
  # Replace spaces ' ' in keywords with underscores '_' 
  keywords <- gsub(" ", "_", keywords)
  
  # Download wiki trends
  wp_trends <- wp_trend(page = keywords,
                        from = start,
                        to = end,
                        warn = FALSE,
                        lang = "en")
  

  # Rearrange downloaded wiki trends
  wp_trends <- rearange(wp_trends, start, end)
  
  # Remove the underscores from the colnames
  names(wp_trends)<-gsub("_", " ", names(wp_trends)) 
  
  # save the result
  setwd(download_wd)
  saveRDS(wp_trends, file = file_name)
  setwd(wd)
  return(wp_trends)
}


######################### rearange #############################################

# Function that rearranges downloaded file of wiki trends 
# into a dataframe with dates as rows and keywords as columns
rearange <- function(wp_trends, start = "2006-01-01", end = "2021-01-31") {
  # Convert start and end to date format
  start_date <- as.Date(start)
  end_date <- as.Date(end)
  
  # Generate all dates in the range
  all_dates <- seq(from = start_date, to = end_date, by = "day")
  
  # Rename columns for clarity
  colnames(wp_trends) <- c("language", "article", "time", "views")
  
  # Ensure `time` column is in Date format
  wp_trends$time <- as.Date(wp_trends$time)
  
  # Reshape data: Make `article` columns and fill missing dates with NA
  Trends <- wp_trends %>%
    select(article, time, views) %>%
    pivot_wider(names_from = article, values_from = views) %>%
    complete(time = all_dates) # Fill missing dates with NA
  
  # Set rownames to dates
  index <- as.numeric(Trends$time)
  index <- as.Date(index,"1970-01-01")
  Trends <- as.data.frame(Trends)
  rownames(Trends) <- index
  Trends$time <- NULL
  
  return(as.data.frame(Trends))
}

