# HELPER FUNCTIONS FOR SCRIPTS "hf_dataset.R" and bind_data.R

# Load necessary libraries
if (!require("pacman")) install.packages("pacman")
# Check whether these libraries are installed, install if not and load all of them
pacman::p_load(stringr,dplyr,readr,xml2,rvest,quantmod)

# Save working directory
my_wd <- getwd()


######################### SYMBOLS AND SPLITS ###################################

# Process data from FirstRate Data on splits that happened to indiv. stocks
# Output: list of dataframes or NAs, if no data is found
make_split_list <- function(directory_path, symbols, startdate="2010-03-10", enddate="2021-02-24") {
  # Create an empty list to store the resulting dataframes
  split.list <- vector("list", length(symbols))
  names(split.list) <- symbols
  
  # List the files in the directory
  files <- list.files(directory_path, full.names = TRUE)
  
  # Filter files based on the symbols vector
  for (symbol in symbols) {
    # File name pattern to match
    file_path <- files[grepl(paste0("^", symbol, "\\.txt$"), basename(files))]
    
    if (length(file_path) == 0) {
      # If no matching file is found, assign NA
      split.list[[symbol]] <- NA
    } else {
      # Read the file into a dataframe
      data <- read.csv(file_path, header = FALSE, col.names = c("Date", "Value"))
      
      # Transform the data
      data[[paste0(symbol, ".spl")]] <- 1 / data$Value
      data <- data[c(paste0(symbol, ".spl"), "Date")]
      rownames(data) <- data$Date
      
      # Filter rows by date
      data <- data[data$Date >= startdate & data$Date <= enddate, ]
      
      # Assign the dataframe to the list under the symbol's name
      split.list[[symbol]] <- data
    }
  }
  
  # Return the resulting list
  return(split.list)
}


# Read the list of symbols from the Wikipedia page and process them into a vector
list_symbols <- function(url){
  # Download the web-page of current constituents
  url <- 'https://en.wikipedia.org/w/index.php?title=List_of_S%26P_500_companies&oldid=1086770611'
  tbl <- read_html(url) %>% html_nodes(css = 'table')
  tbl <- tbl[1] %>% html_table() %>% as.data.frame()
  tbl$Symbol <- gsub(pattern = '\\.', '-', tbl$Symbol) # BRK.B -> BRK-B (yahoo uses '-')
  head(tbl$Symbol)
  
  # List of stocks to load
  symbols <- tbl$Symbol
  symbols <- gsub("-", ".", symbols)
  
  return(symbols)
}




######################### HF RV DATA ###################################

# Load and process minute data into returns and OHLC prices
minute_hf<-function(symbols, wd="./input/hfstocks", my_wd){
  
  # Set wd
  setwd(wd)
  
  # List of file names to download
  filenames <- paste(paste(symbols, "1min", "UNADJUSTED",sep = '_'), ".txt", sep='')
  NS<-length(filenames)
  
  # Prepare an empty list
  dts<-list()
  
  # Loop over symbols to load and process minute prices into returns
  for (s in 1:NS){
    print(paste("Symbol",s,symbols[s],"start."))
    
    # Try to load this file
    try_error = tryCatch(read.csv(filenames[s]), error = function(e) e)
    
    # Check if there was an error
    if (!inherits(try_error, 'error')){
      
      # In case of no error, load and process this file
      dt <- read.csv(filenames[s])
      colnames(dt)<-c("Date_time","O","H","L","C","Volume")
      
      # Extract dates
      dt$Date<-as.Date(dt$Date_time)
      
      # Calculate log returns on minute data
      dt$ret<-NA
      dt$ret[-1] <- log(dt$C[-1]/dt$C[-nrow(dt)])
      
      # Save minute data into a list
      dts[[symbols[s]]] <- dt
      
      print(paste("Symbol",symbols[s],"minute data complete."))
      
    } else {
      print(paste("Symbol",symbols[s],"was not available."))
    }
    
    # Call garbage collection - free up memory
    gc()
  }
  
  # Restore working directory
  setwd(my_wd)
  
  
  return(dts)  
  
}


# Patton Sheppard (PS) 5 min sub-sampling
PS.subsample <- function(subs,dat){
  
  # Prepare dataframes
  datmins <- as.data.frame(matrix(NA,nrow=78,ncol=5,
                                  dimnames=list(NULL,paste("Datetime",seq(0,4),sep = "_"))))
  
  fives <- as.data.frame(matrix(NA,nrow=78,ncol=5,
                                dimnames=list(NULL,paste("5min",seq(0,4),sep = "_"))))
  
  rets <- as.data.frame(matrix(NA,nrow=78-1,ncol=5,
                               dimnames=list(NULL,paste("ret",seq(0,4),sep = "_"))))
  
  # Loop over 5 possible offsets to create 5 different series of 5-min returns
  for (offs in seq(0,4)){
    
    # Dates
    start <- as.POSIXlt(paste(dat,"09:30:00"))
    start <- start + as.difftime(offs, units="mins")
    interval <- 5
    end <- as.POSIXlt(paste(dat,"16:00:00"))
    ts <- seq(from=start, by=interval*60, to=end)
    if (offs==0){ts<-ts[-length(ts)]}
    ts <- as.character(ts)
    datmins[,offs+1] <- ts
    
    # Prices 
    for (t in 1:length(ts)){
      ind <- which(subs$Date_time == ts[t])
      if (identical(ind, integer(0))) next
      fives[t,offs+1] <- subs$C[ind]
    }
    
    # Returns
    rets[,offs+1] <- log(fives[-1,offs+1]/fives[-nrow(fives),offs+1])
    
  }
  
  return(rets)
}



# Median RV
MedRV = function(rdata){
  rv <- sum(rdata^2, na.rm=T)
  
  rdata<-rdata[complete.cases(rdata)]
  q = abs(as.numeric(rdata)) #absolute value
  q = as.numeric(rollmedian(q, k=3, align="center"))
  N = length(q) + 2
  medrv = (pi/(6-4*sqrt(3)+pi))*(N/(N-2))*sum(q^2)
  return(medrv)
}


# Median RQ
MedRQ = function(rdata){
  rdata<-rdata[complete.cases(rdata)]
  
  q = abs(as.numeric(rdata)) #absolute value
  q = as.numeric(rollmedian(q, k=3, align="center"))
  N = length(q) + 2
  medrq = (3*pi/(9*pi + 72 -52*sqrt(3)))*(N/(N-2))*sum(q^4)
  return(medrq)
}


# Jump test for MedRV
MedRVJumptest = function(rv, medrv, medrq, m){ 
  zstat=sqrt(m)*(1-(medrv/rv))/(sqrt(0.96*max(1,(medrq/medrv^2))))
  return(zstat)
}


# Median RV and RQ -> Continuous and jump components with jump test
cj<-function(rets,alpha,rv){
  medrv <- MedRV(rets)
  medrq <- MedRQ(rets)
  z <- MedRVJumptest(rv,medrv,medrq,m=length(rets))
  jind <- z > qnorm(1-alpha)
  jc <-max(0,(rv-medrv))
  jc <- sum(jind)*jc
  cc <- sum(jind)*medrv + sum(!jind)*rv
  
  return(c(jc,cc))
}


aggreg<-function(L1, nam, lags=c(1,5,22), ahead=NULL){
  
  # prepare output
  l1.nam<-paste0(paste(nam,"L",sep="."),lags[1])
  N=length(L1)
  
  ldf<-data.frame(matrix(NA, nrow = N, ncol=1))
  colnames(ldf)<-l1.nam
  ldf[,1]<-L1
  
  # Lagged values
  if (length(lags)>=2){
    for (l in 2:length(lags)){
      ldf[,paste0(paste(nam,"L",sep="."),lags[l])] = NA; for (k in lags[l]:N) ldf[k,paste0(paste(nam,"L",sep="."),lags[l])] = mean(L1[(k-lags[l]+1):(k-1)],na.rm=T)
    }
  }
  
  # Next day's Intraday RV
  if (length(ahead)>0){
    ldf[,paste0(paste(nam,"H",sep="."),ahead[1])]=c(L1[-1],NA)
  }
  
  # Ahead values
  if (length(ahead)>=2){
    for (h in 2:length(ahead)){
      ldf[,paste0(paste(nam,"H",sep="."),ahead[h])] = NA; for (k in 1:(N-(ahead[h]-1))) ldf[k,paste0(paste(nam,"H",sep="."),ahead[h])] = mean(L1[(k+1):(k+(ahead[h]-1))],na.rm=T)
    }
  }
  return(ldf)
}

# Function produces daily RV variables from minute data
daily_hf<-function(dts,W=501,MR=1502,filenam="hf",savefile=FALSE, sampling=1, split.list,alpha=0.05, overnight="simple"){
  
  # Prepare an empty list
  hf<-list()
  
  # Loop over symbols
  for (tic in 1:length(dts)){
    
    # Select dt with minute data from a list
    dt <- dts[[tic]]
    Symbol <- names(dts)[tic]
    print(paste("Symbol",Symbol,tic,"start."))
    
    # Extract unique dates
    DTS <- unique(dt$Date)
    N <- length(DTS)
    
    colnms = c(# Hansen & Lunde weights for the overnight return (OJ) and intraday RV (VI.L1)
               "w1","w2",   
               # Open, High, Low, Close, Volume, Adjusted Close, Date
               "O","H","L","C","V","aC","Date",   
               # Range-based estimators, Overnight return, Adjusted overnight return, Open-Close, Close-Open, Close-Close
               "PK","GK","RS","OJ","OJC","OC.H1","CO.H1","CC.H1",
               # Intraday price variation
               "VI.L1","VI.L5","VI.L22","VI.H1","VI.H5","VI.H22",
               # Weighted Full day price variation (using Hansen & Lunde weights)
               "V.L1","V.L5","V.L22","V.H1","V.H5","V.H22",
               # Non-weighted Full day price variation 
               "VON.L1","VON.L5","VON.L22","VON.H1","VON.H5","VON.H22",
               # Continuous and jump components, semi-variances, signed jump
               "JC.L1","JC.L5","JC.L22","CC.L1","CC.L5","CC.L22","NSV.L1","NSV.L5","NSV.L22",
               "PSV.L1","PSV.L5","PSV.L22","SJ.L1","SJ.L5","SJ.L22",
               # Log-transformed variables
               "VI.L1.Log","VI.L5.Log","VI.L22.Log","VI.H1.Log","VI.H5.Log","VI.H22.Log",
               "V.L1.Log","V.L5.Log","V.L22.Log","V.H1.Log","V.H5.Log","V.H22.Log",
               "VON.L1.Log","VON.L5.Log","VON.L22.Log","VON.H1.Log","VON.H5.Log","VON.H22.Log",
               "JC.L1.Log","JC.L5.Log","JC.L22.Log","CC.L1.Log","CC.L5.Log","CC.L22.Log","NSV.L1.Log","NSV.L5.Log","NSV.L22.Log",
               "PSV.L1.Log","PSV.L5.Log","PSV.L22.Log","SJ.L1.Log","SJ.L5.Log","SJ.L22.Log")
    
    # Data frame for daily data
    df <- data.frame(matrix(ncol=length(colnms),nrow=N, dimnames=list(NULL,colnms)))
    # Dates
    df$Date <- DTS
    
    
    ##### Intraday price variation ##### 
    
    # Loop over days
    for (d in 1:N){
      
      # Subset day DTS[d]
      subs <-subset(dt, dt$Date==DTS[d])
      # Only Trading hours
      subs <- filter(subs, (Date_time >= paste(DTS[d],"09:30:00") & Date_time <= paste(DTS[d],"16:00:00")))
      
      # Move to next date, if all observations were removed (i.e. were in the after trading hours)
      if (nrow(subs) == 0){next}
      
      # OHLC+Volume
      df[d, c("O", "H", "L", "C", "V")] <- c(subs$O[1], max(subs$H), min(subs$L), 
                                             subs$C[nrow(subs)], sum(subs$Volume))
      # C to aC
      df$aC[d]<-df$C[d]
      
      
      # Get all minute returns for day DTS[d] --> sum of squares
      if (sampling == 1){
        rets<-subs$ret
        rv <- sum(subs$ret^2, na.rm=T)
      }
      
      # An option for Patton Sheppard 5 min sub-sampling
      if (sampling == 5){
        rets5<-PS.subsample(subs = subs, dat=DTS[d]) # grid of returns 
        rets<-rowMeans(rets5);print(length(rets))
        rv = mean(colSums(rets5^2, na.rm=T))
      }
      
      # Continuous and jump components with jump test
      jc <-cj(rets,alpha,rv)[1]; cc <-cj(rets,alpha,rv)[2]
      
      # Semi-variances and signed jump
      nsv=sum((rets[rets<=0])^2, na.rm=T); psv=rv-nsv; sj=psv-nsv
      
      # Assign
      df[d, c("VI.L1", "JC.L1", "CC.L1", "NSV.L1", "PSV.L1", "SJ.L1")] <- c(rv, jc, cc, nsv, psv, sj)
    }
    
    
    ##### Range-based volatility estimators #####
    # Variables used to create range-based volatility estimators
    # Daily + annualized composite volatility measures
    h = log(df$H) - log(df$O)
    l = log(df$L) - log(df$O)
    c = log(df$C) - log(df$O)
    
    # Parkinson intraday estimator
    df$PK = (h - l)^2/(4*log(2))
    # Garman and Klass intraday estimator
    df$GK = 0.511*(h-l)^2-0.019*(c*(h+l)-2*h*l)-0.383*c^2
    # Roger and Satchel intraday estimator
    df$RS = h*(h-c) + l*(l-c)
    
    # Annualized
    annualized_cols =c("VI.L1","JC.L1","CC.L1","NSV.L1","PSV.L1","SJ.L1","PK","GK","RS")
    df[,annualized_cols] <- df[,annualized_cols]*100^2 * 252
    
    ##### Lags and future values #####
    ldf<-aggreg(L1=df$VI.L1, nam="VI", lags=c(1,5,22), ahead=c(1,5,22))
    # df[,colnames(ldf)]<-ldf
    df <- cbind(df, ldf)
    for (var in c("JC.L1", "CC.L1", "NSV.L1", "PSV.L1", "SJ.L1")) {
      ldf <- aggreg(L1 = df[[var]], nam = gsub("\\.L1", "", var), lags = c(1, 5, 22), ahead = NULL)
      df <- cbind(df, ldf)
    }
    
    
    ##### Full day price variation ####
    
    ##### Hansen & Lunde NO weights for the overnight return (OJ) and intraday RV (VI.L1) #####
    
    # Squared overnight return annualized (from the previous night)
    df$OJ <- c(NA,(log(df$O[-1]/df$C[-nrow(df)]))^2)*100^2*252
    # Copy of OJ for corrections of splits
    df$OJC<-df$OJ
    # get list of splits
    splits<-split.list[[Symbol]]
    
    # If there are some splits, replace the overnight return for these nights with zero
    if (sum(is.na(splits))==0){
      df$OJC[which(df$Date %in% as.Date(splits$Date))]<-0
    } 
    
    # Night + Day
    df$VON.L1<-df$OJC+df$VI.L1
    ldf<-aggreg(L1=df$VON.L1, nam="VON", lags=c(1,5,22), ahead=c(1,5,22))
    df[,colnames(ldf)]<-ldf
    
    ##### Hansen & Lunde weights for the overnight return (OJ) and intraday RV (VI.L1) #####
    # Perform a rolling window estimation of weights
    # First 500 days rolling window; rolling one-step-ahead estimates; only the most recent 1500 trading days
    
    # Loop over days: seq(501, number of days-1)
    if(W>(N-1)){
      WW=N-1
    } else {
      WW=W
    }
    
    for (i in seq(WW,(N-1))){
      
      # Only take the most recent (MR) 1500 trading days
      if (i <= MR){
        from=2
      } else {
        move=i-MR
        from=2+move
      }
      to=i
      if(to>(N-1)){
        to=N-1
      }
      
      # Squared overnight return
      night<-df$OJC[from:to]
      # RV measure for the active part of the day
      day<-df$VI.L1[from:to]
      # Sum of these two (night+day)
      full<-df$VON.L1[from:to]
      
      # Remove 5% of extreme values
      night=fun(night,qt=c(.95))
      day=fun(day,qt=c(.95))
      full=day+night
      
      # Calculate relative importance factor (phi)
      mu1=mean(night,na.rm=T)
      mu2=mean(day,na.rm=T)
      mu0=mean(full,na.rm=T)
      eta1=var(night,na.rm=T)
      eta2=var(day,na.rm=T)
      eta12=cov(night,day,use="complete.obs")
      # print(paste(mu1,mu2,mu0,eta1,eta2,eta12))
      phi=(mu2^2*eta1-mu1*mu2*eta12)/(mu2^2*eta1+mu1^2*eta2-2*mu1*mu2*eta12)
      # prevent negative weights by setting phi max as 1
      if (phi>1){phi<-1}
      
      # Weights
      w1=(1-phi)*(mu0/mu1)
      w2=phi*(mu0/mu2)
      
      # Calculate the final full day RV measure (multiply by weights)
      # For the first loop, use the weight on all first W days
      if (i==WW){
        df$w1[from:(to+1)]<-w1
        df$w2[from:(to+1)]<-w2
        df$V.L1[from:(to+1)]<-w1*df$OJC[from:(to+1)] + w2*df$VI.L1[from:(to+1)]
      } else {
        df$w1[to+1]<-w1
        df$w2[to+1]<-w2
        df$V.L1[to+1]<-w1*df$OJC[to+1] + w2*df$VI.L1[to+1]
      }
    }
    
    ldf<-aggreg(L1=df$V.L1, nam="V", lags=c(1,5,22), ahead=c(1,5,22))
    df[,colnames(ldf)]<-ldf
    
    
    ##### Log-transforms ##### 
    
    tolog =c("VI.L1","VI.L5","VI.L22","VI.H1","VI.H5","VI.H22","V.L1","V.L5","V.L22","V.H1","V.H5","V.H22",
             "VON.L1","VON.L5","VON.L22","VON.H1","VON.H5","VON.H22","JC.L1","JC.L5","JC.L22",
             "CC.L1","CC.L5","CC.L22","NSV.L1","NSV.L5","NSV.L22","PSV.L1","PSV.L5","PSV.L22"
             ,"SJ.L1","SJ.L5","SJ.L22"
    )
    
    afterlog=paste(tolog,"Log",sep=".")
    df[,afterlog] = LogTransform(df[,tolog],trans=c(1:ncol(df[,tolog])))
    

    ##### Return measures #####
    
    # Open to Close intraday returns - not continuous
    df$OC.H1 = 100*c(((df$C[-1] - df$O[-1])/df$O[-1]),NA)
    
    # Close to Open overnight returns - not continuous
    df$CO.H1 = 100*c((df$O[-1] - df$C[-dim(df)[1]])/df$C[-dim(df)[1]],NA)
    
    # Close to Close returns 1 day ahead
    df$CC.H1 = 100*c((df$C[-1] - df$C[-dim(df)[1]])/df$C[-dim(df)[1]],NA)
    
    ##### Save daily data into list #####
    # Remove all NAs
    df<-df[complete.cases(df),]
    hf[[Symbol]]<-df
    
    if (savefile==TRUE){
      saveRDS(hf, file = filenam) 
    }
    
    
    print(paste("Symbol",Symbol,"daily data complete."))
  }
  return(hf)
}


# Load and bind hf files 
load_bind<-function(load_path=path_1hf, save_path="./input/hfstocks_unadjusted/final", my_wd, filenam="hf1"){
  # Has to be in RDS format
  setwd(load_path)
  filenams<-list.files(pattern="hf*")
  hf<-list()
  for (f in 1:length(filenams)){
    fil <- readRDS(filenams[f])
    hf<-append(hf,fil)
  }
  
  setwd(my_wd)
  setwd(save_path)
  saveRDS(hf,file=filenam)
  setwd(my_wd)
  
  return(hf)
}


# Helper function for loading and renaming our input data
loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}



# Merges data from sentiment and market
SentiStock = function(market,senti=dataset.complete) {
  # Variable that corresponds to date
  senti$Date = as.Date(rownames(senti),format='%Y-%m-%d')
  rownames(senti) = NULL
  
  # Number of markets
  NM = length(market)
  
  # Loop over markets
  for (k in 1:NM) {
    
    # Select market data
    mrkt = market[[k]]
    
    # Select sentiment
    sent = senti
    
    # Common dates across datasets
    gd = intersect(mrkt$Date,sent$Date)
    
    # Select only relevant dates
    mrkt = mrkt[mrkt$Date %in% gd,]
    sent = sent[sent$Date %in% gd,]
    
    if (sum(mrkt$Date == sent$Date) < dim(mrkt)[1]) return('Matching dates did not work')
    market[[k]] = data.frame(mrkt,sent)
    
    rm(mrkt,sent,gd)
  }
  
  return(market)
}
