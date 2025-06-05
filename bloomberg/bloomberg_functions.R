# HELPER FUNCTIONS FOR SCRIPT "bloomberg.R"


# Load necessary libraries
if (!require("pacman")) install.packages("pacman")
# Check whether these libraries are installed, install if not and load all of them
pacman::p_load(dplyr,lubridate,bizdays,readxl,stringr)


# Save working directory
my_wd <- getwd()


############################ BLOOMBERG - ECON DUMMY ############################

############################ convert_date ######################################

# Function to convert 3 different types of dates in the bloomberg dataset
convert_date <- function(toconvert = econ){
  # Convert 3 types of date time values based on their length
  for (i in seq(1,length(toconvert$`Date Time`))){
    len = nchar(toconvert[i,1])
    print(paste(i, len, sep = '-'))
    if (len == 14){
      d <- strptime(toconvert$`Date Time`[i], format='%m/%d/%y %H:%M')
    } else if (len == 8){
      d <- strptime(toconvert$`Date Time`[i], format='%m/%d/%y')
    } else {
      d <- strptime(toconvert$`Date Time`[i], format='%m/%d/%y-%m/%d/%y')
    }
    if (i == 1){
      DateTime <- c(d)
    } else {
      DateTime <- c(DateTime, d)
    }
  }
  toconvert$`Date Time` <- as.POSIXct(DateTime)
  return(toconvert)
}



############################## get_date_time ###################################

# Function for trading hours - used in make_econ_dummy
get_date_time <- function(td, times) {
  sort(as.POSIXct(outer(td, times, paste, sep = " ")))
}


############################## make_econ_dummy #################################

# Optimized function to make dummy variables from the econ dataset
make_econ_dummy <- function(econ, calendar, from1, to1, from2, to2, opn, cls, opt = "dummy") {
  # Extract unique events
  events <- unique(econ$Event)
  
  # Load NYSE calendar and define trading days
  load_quantlib_calendars(c(calendar), from = from1, to = to1)
  full_calendar <- paste('QuantLib/', calendar, sep = '')
  td <- bizseq(from2, to2, full_calendar)
  
  # Initialize dummies data frame
  dummies <- as.data.frame(matrix(0, ncol = length(events) + 1, nrow = length(td),
                                  dimnames = list(NULL, c("td", events))))
  dummies$td <- td
  
  # Precompute trading day adjustments
  econ$`Date Time` <- as.POSIXct(econ$`Date Time`, tz = "Etc/GMT+5") # Ensure consistent timezone
  
  # Define helper function for processing individual events
  process_event <- function(event_name) {
    event_data <- econ[econ$Event == event_name, c(1, 3, 9, 10, 11, 12)]
    event_data$open <- get_date_time(as.Date(event_data[, 1]), opn) %>% force_tz("Etc/GMT+5")
    event_data$close <- get_date_time(as.Date(event_data[, 1]), cls) %>% force_tz("Etc/GMT+5")
    
    event_data$status <- ifelse(event_data$`Date Time` > event_data$close, "after",
                                ifelse(event_data$`Date Time` < event_data$open, "before", "during"))
    
    # Adjust dates for non-trading days and trading hours
    adjusted_days <- sapply(1:nrow(event_data), function(idx) {
      day <- as.Date(event_data[idx, 1])
      
      if (event_data$status[idx] %in% c("before", "during")) {
        day <- day - 1
      }
      
      if (!day %in% td) {
        day <- adjust.previous(day, full_calendar)
      }
      
      return(day)
    })
    
    # Assign values to dummies
    rows <- match(adjusted_days, td)
    if (opt == "dummy") {
      dummies[rows, event_name] <<- 1
    } else if (opt == "est") {
      dummies[rows, event_name] <<- event_data$`# Ests.`
    } else if (opt == "std") {
      dummies[rows, event_name] <<- as.numeric(event_data$`Std Dev`)
    } else if (opt == "unc") {
      dummies[rows, event_name] <<- as.numeric(event_data$Uncertainty)
    } else if (opt == "range") {
      dummies[rows, event_name] <<- as.numeric(event_data$Range)
    }
  }
  
  # Process each event
  for (event_name in events) {
    print(paste("Processing event:", event_name))
    process_event(event_name)
  }
  
  # Clean up and return
  dummies[is.na(dummies)] <- 0
  rownames(dummies) <- dummies$td
  dummies <- dummies[, -1] # Remove the "td" column
  return(dummies)
}
