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


# Or run the script directly in RStudio
# Uncomment all commented out lines to run the script directly in RStudio!

# Select the stock number i, number of cores nc, version vers and dependent variable depnum
# i=1;nc=12;vers="5w";depnum=1

print(paste("Arguments loaded", i, nc, vers, depnum))


######################## LOAD FUNCTIONS ########################

# Source shared helper functions to load the function create_wd()
# source(here::here('shared_functions.R'))
# parent_wd <- getwd()
# Set the working directory
# models_wd = "./models"
# Create models_wd if it doesn't exist
# create_wd(models_wd)
# setwd(models_wd)


# Source the helper functions script to load custom functions and load libraries
source('model_functions.R')
print(sessionInfo())

# setwd(parent_wd)


######################## LOAD EXTERNAL DATASETS ########################

# Set the working directory to load the dataset
path_final_other ="./stockdata/final/other_versions/";setwd(path_final_other)

# Load dataset with volatility measures, attention and sentiment variables
filename=paste("hf_market_",vers,"_2.RData",sep='')
stocks.market <- loadRData(filename)
gc()
print(paste("loaded file",filename))

# Return to the original working directory
setwd(my_wd)

# Load csv with a description of variables
nams = read.csv(file='nams.csv')
nams = data.frame(nams)


######################## DEFINE PARAMETERS ########################

# ARE WE ADDING TO THE EXISTING DATASET - FALSE = WE SUBSTITUTE
addto = FALSE
# SMOOTHING FOR MOVING AVERAGE AND ASVI
K = 5
# COMPLEXITY PARAMETER IS
cx = 4
# WHAT TYPE OF ESTIMATION TO USE?
estim.type = 'WLS3'
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
download_wd=paste("./files/hf_market_",vers,"_H",depnum,sep="")
# Create the download directory if it doesn't exist
create_wd("./files")
create_wd(download_wd)


######################## APPLY MODELS TO ONE STOCK ###############

# Start
B = Sys.time()
res.nms <- names(stocks.market)[i]
print(paste("starting stock number", i, "called", res.nms))


# Select dataset for stock no.i
DR = stocks.market[[i]]; rm(stocks.market);gc()

# Empty list for storing results
store = list()

# Estimate and forecast with all models or quit if we do not have enough days for this stock
if (dim(DR)[1] < 2501){
  print("Our dataset for this stock is too small.")
  break
} else {
  print("We have enough observations for this stock")
  
  A = Sys.time()
  
  ######################## GENERAL MODELS ###############
  
  # GENERAL ATTENTION
  # Models: HAR-A
  store <- general_estimation(store, DR,dep,category='att',senttype=NULL,fixing,
                              estim.type,LogTrans,formats,K,addto,W,nc,orderapprox,nams)
  
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
      store <- general_estimation(store, DR,dep,category=sent_cats[[sc]],senttype=sent_types[st],fixing,
                                  estim.type,LogTrans,formats,K,addto,W,nc,orderapprox,nams)
    }
  }
  
  # MODEL WITH ALL DUMMIES X V.L1.Log
  # Models: HAR-M
  store <- general_estimation(store, DR,dep,category='dummy',senttype=NULL,fixing,
                              estim.type,LogTrans,formats=NULL,K,addto,W,nc,orderapprox,nams, IA=T)
  
  print("general models complete")
  print(Sys.time()-A)
  
  ######################## + events ###############
  
  # IF you set LogTransRF=FALSE, alwayssplit should have one of these: c("V.L1","V.L5","V.L22")
  # For LogTransRF=TRUE, alwayssplit should have one of these: c('V.L1.Log','V.L5.Log','V.L22.Log')
  
  # GENERAL + EVENT SPECIFIC ATTENTION
  # Models: CSR-A, ALA-A, RF-A
  A = Sys.time()
  store <- event_estimation(store, DR,dep = dep,category="att",senttype=NULL,fixing = c('V.L1.Log','V.L5.Log'),
                            estim.type,LogTrans,formats,K,addto,W,nc,orderapprox,bench=bench,
                            cx=4, alphas,reestim, nlambda,loss,CS,nams, IA=IA,
                            alwayssplit = c('V.L1.Log','V.L5.Log'),LogTransRF=LogTransRF)

  print("attention complete")
  print(Sys.time()-A)
  
  # GENERAL + EVENT SPECIFIC SENTIMENT
  # Models: CSR-S, ALA-S, RF-S
  for (sc in 1:length(sent_cats)){
    for (st in 1:length(sent_types)){
      A = Sys.time()
      print(paste(sent_cats[[sc]],sent_types[st],"start"))

      store <- event_estimation(store, DR,dep = dep,category=sent_cats[[sc]],senttype=sent_types[st],fixing = c('V.L1.Log','V.L5.Log'),
                                estim.type,LogTrans,formats,K,addto,W,nc,orderapprox,bench=bench,
                                cx=4, alphas,reestim, nlambda,loss,CS,nams, IA=IA, 
                                alwayssplit = c('V.L1.Log','V.L5.Log'),LogTransRF=LogTransRF)
      print(paste(sent_cats[[sc]],sent_types[st],"complete"))
      print(Sys.time()-A)
    }
  }
  
  # HAR+SUPER BENCHMARK=HAR-CSLR 
  A = Sys.time()
  store <- event_estimation(store, DR,dep = dep,category='superbench',senttype=NULL,fixing = NULL,
                            estim.type,LogTrans,formats= NULL,K,addto,W,nc,orderapprox,bench=bench,
                            cx=4, alphas,reestim, nlambda,loss,CS,nams, IA=IA, 
                            alwayssplit = c('V.L1.Log','V.L5.Log'),LogTransRF=LogTransRF)

  print("superbench complete")
  print(Sys.time()-A)
  
  setwd(download_wd)
  saveRDS(store,file=res.nms)
  setwd(my_wd)
}


# End
Sys.time()-B
print(paste("finished stock number", i, "called", res.nms))



