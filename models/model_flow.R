# 1 STOCK MODELS ESTIMATION

# This script can be run in two ways:
# A) Run a job with the model_flow.sh script to estimate all models for a specific stock
# We use the Metacentrum grid service to run a batch of jobs in parallel in model_jobs.sh
# or B) run the model_flow.R script directly in RStudio to estimate all models for a specific stock


# Read in the arguments listed at the command line
arg <- commandArgs(TRUE)
i <- as.numeric(arg[1])    # stock number i
nc <- as.numeric(arg[2])   # number of cores
vers <- arg[3]             # 1w/1s/5w/5s
depnum <- arg[4]           # number that dictates the dependent variable: 'V.H1.Log'/'V.H5.Log'/'V.H22.Log'
indices <- arg[5]          # F = compute for individual stocks, T = compute for indices
estim.type <- arg[6]       # 'WLS1', 'WLS3', 'MSE'

# Or run the script directly in RStudio
# Uncomment all commented out lines to run the script directly in RStudio!

# Select the stock number i, number of cores nc, version vers and dependent variable depnum
# i=1;nc=12;vers="5w";depnum=1;indices=FALSE;estim.type="WLS3"

print(paste("Arguments loaded", i, nc, vers, depnum, indices, estim.type))


######################## LOAD FUNCTIONS ########################

# Source the helper functions script to load custom functions and load libraries
# if (!require("pacman")) install.packages("pacman")
# pacman::p_load(here)
setwd('..')
source(here::here('shared_functions.R'))

# Source the helper functions script to load custom functions and load libraries
models_wd="./models"; create_wd(models_wd)
source('./models/model_functions.R')
print(sessionInfo())

print("functions loaded")



######################## LOAD EXTERNAL DATASETS ########################

# Set the working directory to load the dataset
if (indices==FALSE){
  path_final_other ="./stockdata/final/other_versions/"
} else {
  path_final_other ="./robustness_checks/global_risk/final/other_versions/"
}
setwd(path_final_other)

# Load dataset with volatility measures, attention and sentiment variables
if (indices==FALSE){
  filename=paste("hf_market_",vers,"_2.RData",sep='')
} else {
  filename=paste("indices_full_",vers,"_2.RData",sep='')  
}
stocks.market <- loadRData(filename)
gc()
print(paste("loaded file",filename,"from directory", path_final_other))

# Return to the original working directory
setwd(my_wd)


# Load csv with a description of variables
setwd("./dataset")
nams = read.csv(file='nams.csv')
nams = data.frame(nams)
# Return to the original working directory
setwd(my_wd)


######################## DEFINE PARAMETERS ########################

# ARE WE ADDING TO THE EXISTING DATASET - FALSE = WE SUBSTITUTE
addto = FALSE
# SMOOTHING FOR MOVING AVERAGE AND ASVI
K = 5
# COMPLEXITY PARAMETER IS
cx = 4
# WHAT TYPE OF ESTIMATION TO USE? Options: 'WLS1', 'WLS3', 'MSE'
estim.type = estim.type       # argument from command line
# WHAT IS THE ESTIMATION WINDOW SIZE?
W = 1000
# LOSS TO OPTIMIZE
loss = c('MSE','QLIKE')
# WHAT ARE THE MEMORY PARAMETERS
delta = c(0.95)
# TRIM
trim = 0.15
# NUMBER OF GROUPS IN KMEANS
ngroup = c(5,10)
# CALIBRATION SAMPLE SIZE
CS = 500
# ALPHA REGULARIZATION PARAMETERS - currently adaLASSO is used
alphas = c(1)
# NUMBER OF LAMBDA PENALTY PARAMETERS
nlambda = 50
# RE-ESTIMATE MODEL AFTER 'X' OBSERVATIONS
reestim=1
# NUMBER OF TREES
num.trees = c(500)
# NUMBER OF VARIABLES TO TRY
mtry = c(8,16,32)
# DEPTH OF A TREE
md = c(0, 6, 12)
# WHAT TRANSFORMATIONS ARE WE GOING TO USE?
formats = c('Log')
# In case of log transformed variables what should be the order of the approximation of the backward transformation?
orderapprox = 2
# TRANSFORMATION
LogTrans=TRUE
LogTransRF=TRUE
# Set to TRUE if you want to estimate the model with INTERACTION BETWEEN RV x Sentiment/Attention variables
IA=NULL

# Common parameters for general models
fixing = c('V.L1.Log','V.L5.Log','V.L22.Log')
# Dependent variable
dep = paste0("V.H",depnum,".Log")
# Benchmark model
bench=as.formula(paste0(dep,"~V.L1.Log+V.L5.Log+V.L22.Log"))


# Define the download directory
setwd("./models")
# Create the download directory if it doesn't exist
create_wd("./files")
if (indices==FALSE){
  download_wd=paste("./files/hf_market_",vers,"_H",depnum,sep="")
} else {
  download_wd=paste("./files/indices_full_",vers,"_H",depnum,sep="")
}
if (estim.type!="WLS3") download_wd=paste(download_wd,"_",estim.type,sep="")

create_wd(download_wd)


# Subset only to stocks that have been done before
if (indices==FALSE){
  filenames <- list.files("./files/hf_market_5w_H1")
  filenames <- filenames[!grepl("_part$", filenames)]
  stocks.market <- stocks.market[names(stocks.market) %in% filenames]
}


# Load all files that include "part" in their name and only run for those stocks
# part_filenames <- list.files(download_wd, pattern = "_part$")
# part_filenames <- gsub("_part$", "", part_filenames)
# stocks.market <- stocks.market[names(stocks.market) %in% part_filenames]


######################## APPLY MODELS TO ONE STOCK ###############

# Start
B = Sys.time()
res.nms <- names(stocks.market)[i]
print(paste("starting stock number", i, "called", res.nms))


# Select dataset for stock no.i
DR = stocks.market[[i]]; rm(stocks.market);gc()


# -----------------------
# Attempt to load existing results for this stock (if present).
# If found, use that 'store' as the starting point; we will only add missing model entries.
# -----------------------
store = list()
possible_paths <- c(paste0(file.path(download_wd, res.nms), "_part"),file.path(download_wd, res.nms))
loaded_any <- FALSE
for (p in possible_paths) {
  if (file.exists(p)) {
    tryCatch({
      tmp <- readRDS(p)
      if (is.list(tmp)) {
        store <- tmp
        loaded_any <- TRUE
        message(paste("Loaded existing results from", p))
        break
      } else {
        message(paste("Found file", p, "but it is not a list; ignoring."))
      }
    }, error = function(e) {
      message(paste("Error loading", p, ":", e$message))
    })
  }
}
if (!loaded_any) message("No existing results file found for this stock; starting with empty store.")



# Estimate and forecast with all models or quit if we do not have enough days for this stock
# Ignore this rule, if indices=="T"; set L to a smaller number, so that the exception is never raised
if (indices==TRUE){
  L=1000
} else {
  L=2500
}  

if (dim(DR)[1] < (L+1)){
  print("Our dataset for this stock is too small.")
  break
} else {
  print("We have enough observations for this stock")
  
  A = Sys.time()
  
  ######################## GENERAL MODELS ###############
  
  # GENERAL ATTENTION
  # Models: HAR-A
  # Only run if not already stored
  expected_name <- make_gen_name(categories = c('att'), senttype = NULL)
  if (!(expected_name %in% names(store))) {
    message(paste("Computing general attention models (will be saved under:", expected_name, ")"))
    store <- general_estimation(store, DR,dep,category='att',senttype=NULL,fixing,
                                estim.type,LogTrans,formats,K,addto,W,nc,orderapprox,nams)
  } else {
    message(paste("Skipping general attention models - found existing entry:", expected_name))
  }
  
  # GENERAL POSITIVE AND NEGATIVE SENTIMENT
  # Models: HAR-S
  # Select sentiment categories, the following setting will use both positive and negative sentiment variables
  sent_cats<-list(c('positive emotions','negative emotions'))
  
  # Select sentiment methods
  sent_types<-c("emolex","vader","finbert")
  # Further specify which sentiment methods are to be used for multiple day ahead predictions (depnum >1)
  if (depnum > 1) sent_types<-c("finbert")
  # Further specify which sentiment methods are to be used for alternative specifications (other than "5w")
  if (vers!="5w") sent_types<-c("finbert")
  
  # Loop over selected sentiment categories and methods
  for (sc in 1:length(sent_cats)){
    for (st in 1:length(sent_types)){
      # compute expected name the same way general_estimation will do
      expected_name <- make_gen_name(categories = sent_cats[[sc]], senttype = sent_types[st])
      if (!(expected_name %in% names(store))) {
        message(paste("Computing sentiment models for", paste(sent_cats[[sc]], collapse = ","), "using", sent_types[st],
                      "(will be saved under:", expected_name, ")"))
        store <- general_estimation(store, DR,dep,category=sent_cats[[sc]],senttype=sent_types[st],fixing,
                                    estim.type,LogTrans,formats,K,addto,W,nc,orderapprox,nams)
      } else {
        message(paste("Skipping sentiment models - found existing entry:", expected_name))
      }
    }
  }
  
  # MODEL WITH ALL DUMMIES X V.L1.Log
  # Models: HAR-M
  expected_name <- make_gen_name(categories = c('dummy'), senttype = NULL)
  if (!(expected_name %in% names(store))) {
    message(paste("Computing dummy interaction models (will be saved under:", expected_name, ")"))
    store <- general_estimation(store, DR,dep,category='dummy',senttype=NULL,fixing,
                                estim.type,LogTrans,formats=NULL,K,addto,W,nc,orderapprox,nams, IA=T)
  } else {
    message(paste("Skipping dummy interaction models - found existing entry:", expected_name))
  }
  
  print("general models complete")
  print(Sys.time()-A)
  
  ######################## + events ###############
  
  # IF you set LogTransRF=FALSE, alwayssplit should have one of these: c("V.L1","V.L5","V.L22")
  # For LogTransRF=TRUE, alwayssplit should have one of these: c('V.L1.Log','V.L5.Log','V.L22.Log')
  
  # GENERAL + EVENT SPECIFIC ATTENTION
  # Models: CSR-A, ALA-A, RF-A
  expected_name <- make_event_name(categories = c('att'), senttype = NULL)
  if (!(expected_name %in% names(store))) {
    message(paste("Computing event-attention models (will be saved under:", expected_name, ")"))
    A = Sys.time()
    store <- event_estimation(store, DR,dep = dep,category="att",senttype=NULL,fixing = c('V.L1.Log','V.L5.Log'),
                              estim.type,LogTrans,formats,K,addto,W,nc,orderapprox,bench=bench,
                              cx=4, alphas,reestim, nlambda,loss,CS,nams, IA=IA,
                              alwayssplit = c('V.L1.Log','V.L5.Log'),LogTransRF=LogTransRF)
    print("attention complete")
    print(Sys.time()-A)
  } else {
    message(paste("Skipping event-attention models - found existing entry:", expected_name))
  }
  
  # GENERAL + EVENT SPECIFIC SENTIMENT
  # Models: CSR-S, ALA-S, RF-S
  for (sc in 1:length(sent_cats)){
    for (st in 1:length(sent_types)){
      expected_name <- make_event_name(categories = sent_cats[[sc]], senttype = sent_types[st])
      if (!(expected_name %in% names(store))) {
        A = Sys.time()
        print(paste(sent_cats[[sc]],sent_types[st],"start"))
        
        store <- event_estimation(store, DR,dep = dep,category=sent_cats[[sc]],senttype=sent_types[st],fixing = c('V.L1.Log','V.L5.Log'),
                                  estim.type,LogTrans,formats,K,addto,W,nc,orderapprox,bench=bench,
                                  cx=4, alphas,reestim, nlambda,loss,CS,nams, IA=IA, 
                                  alwayssplit = c('V.L1.Log','V.L5.Log'),LogTransRF=LogTransRF)
        print(paste(sent_cats[[sc]],sent_types[st],"complete"))
        print(Sys.time()-A)
      } else {
        message(paste("Skipping event-sentiment models - found existing entry:", expected_name))
      }
    }
  }
  
  

  A = Sys.time()
  expected_name <- make_event_name(categories = c('superbench'), senttype = NULL)
  message(paste("Computing superbench models (will be saved under:", expected_name, ")"))
  store <- event_estimation(store, DR,dep = dep,category='superbench',senttype=NULL,fixing = c('V.L1.Log','V.L5.Log'),
                            estim.type,LogTrans,formats= NULL,K,addto,W,nc,orderapprox,bench=bench,
                            cx=4, alphas,reestim, nlambda,loss,CS,nams, IA=IA,
                            alwayssplit = c('V.L1.Log','V.L5.Log'),LogTransRF=LogTransRF)

  print("superbench complete")
  print(Sys.time()-A)
  
  # Additional bechmark models
  # HAR+SUPER BENCHMARK=HAR-CSLR 
  # expected_name <- make_event_name(categories = c('superbench'), senttype = NULL)
  # if (!(expected_name %in% names(store))) {
  #   A = Sys.time()
  #   store <- event_estimation(store, DR,dep = dep,category='superbench',senttype=NULL,fixing = c('V.L1.Log','V.L5.Log'),
  #                             estim.type,LogTrans,formats= NULL,K,addto,W,nc,orderapprox,bench=bench,
  #                             cx=4, alphas,reestim, nlambda,loss,CS,nams, IA=IA,
  #                             alwayssplit = c('V.L1.Log','V.L5.Log'),LogTransRF=LogTransRF)
  #   
  #   print("superbench complete")
  #   print(Sys.time()-A)
  # } else {
  #   message(paste("Skipping superbench event models - found existing entry:", expected_name))
  # }
  

  setwd(download_wd)
  # Save merged/updated store (partial result)
  res.nms.part=paste(res.nms,"part",sep="_")	
  saveRDS(store,file=res.nms.part)
  print("new file with partial results saved")	
  setwd(my_wd);setwd(models_wd)

  
  # HAR+SUPER BENCHMARK=HAR-CSLR-FULL with crosssectional RV over all stocks
  expected_name <- make_event_name(categories = c('superbench_full'), senttype = NULL)
  if (indices == FALSE & !(expected_name %in% names(store))){
  # if (!(expected_name %in% names(store))) {
    message(paste("Computing superbench full models (will be saved under:", expected_name, ")"))
    A = Sys.time()
    store <- event_estimation(store, DR,dep = dep,category='superbench_full',senttype=NULL,fixing = c('V.L1.Log','V.L5.Log'),
                              estim.type,LogTrans,formats= NULL,K,addto,W,nc,orderapprox,bench=bench,
                              cx=4, alphas,reestim, nlambda,loss,CS,nams, IA=IA, 
                              alwayssplit = c('V.L1.Log','V.L5.Log'),LogTransRF=LogTransRF)
    
    print("superbench full complete")
    print(Sys.time()-A)
  } else {
    message(paste("Skipping superbench full models - found existing entry:", expected_name))
  }

  setwd(download_wd)
  # Save merged/updated store (partial result)
  res.nms.part=paste(res.nms,"part",sep="_")	
  saveRDS(store,file=res.nms.part)
  print("new file with partial results saved")	
  setwd(my_wd);setwd(models_wd)


  # HAR-VIX BENCHMARK=HAR with only VIX as additional variable
  expected_name <- make_gen_name(categories = c('vix'), senttype = NULL)
  if (indices == FALSE & !(expected_name %in% names(store))){
  # if (!(expected_name %in% names(store))) {
    message(paste("Computing vix benchmark models (will be saved under:", expected_name, ")"))
    A = Sys.time()
    store <- general_estimation(store, DR,dep,category='vix',senttype=NULL,fixing,
                                estim.type,LogTrans,formats,K,addto,W,nc,orderapprox,nams)
    print("vix bench complete")
    print(Sys.time()-A)
    
  } else {
    message(paste("Skipping vix benchmark models - found existing entry:", expected_name))
  }

  setwd(download_wd)
  # Save merged/updated store (partial result)
  res.nms.part=paste(res.nms,"part",sep="_")	
  saveRDS(store,file=res.nms.part)
  print("new file with partial results saved")	
  setwd(my_wd);setwd(models_wd)
  
  # HAR+SUPER BENCHMARK=HAR-CSLR-RV with S&P500 RV instead
  expected_name <- make_event_name(categories = c('superbench_rv'), senttype = NULL)
  if (indices == FALSE & !(expected_name %in% names(store))){
  # if (!(expected_name %in% names(store))) {
    message(paste("Computing superbench full with S&P500 models (will be saved under:", expected_name, ")"))
    A = Sys.time()
    store <- event_estimation(store, DR,dep = dep,category='superbench_rv',senttype=NULL,fixing = c('V.L1.Log','V.L5.Log'),
                              estim.type,LogTrans,formats= NULL,K,addto,W,nc,orderapprox,bench=bench,
                              cx=4, alphas,reestim, nlambda,loss,CS,nams, IA=IA,
                              alwayssplit = c('V.L1.Log','V.L5.Log'),LogTransRF=LogTransRF)
    print("superbench full with S&P500 complete")
    print(Sys.time()-A)
    
  } else {
    message(paste("Skipping superbench full with S&P500 models - found existing entry:", expected_name))
  }

  
  setwd(download_wd)
  # Save merged/updated store (this will overwrite the file with the updated store)
  saveRDS(store,file=res.nms)
  # if we got here, delete the partial results file
  if (file.exists(res.nms.part)) {
    #Delete file if it exists
    file.remove(res.nms.part)
  }
  print("new file with results saved")	
  setwd(my_wd)
}


# End
Sys.time()-B
print(paste("finished stock number", i, "called", res.nms))
