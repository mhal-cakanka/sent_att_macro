# SCRIPT FOR RUNNING ONE VERSION OF THE MCS TEST ON ALL STOCKS IN THE DATASET
# WE RUN THIS ON MODEL PREDICTION RESULTS (version defined by arguments)

# The script can be run in two ways:
# A) Run a job with the mcs_meta.sh script to estimate mcs on all results for a specific setting 
# We use the Metacentrum grid service to run a batch of jobs in parallel
# or B) run this script directly in RStudio

#### ARGUMENTS #### 

# Read in the arguments listed at the command line
arg <- commandArgs(TRUE)
mcs_alpha <- as.numeric(arg[1])    # mcs_alpha
days <- arg[2]             # top/bottom/all corresponds to top 10% of days with the highest RV, bottom 90% and all days
nc <- as.numeric(arg[3])   # number of cores to use
vers <- arg[4]             # 1w/1s/5w/5s
depnum <- arg[5]           # number that dictates the dependent variable: 'V.H1.Log'/'V.H5.Log'/'V.H22.Log'
full <- arg[5]             # T - run MCS on all models, F - run a pairwise MCS

# Uncomment to get mcs pairwise results for the main specification (after aggregation Table 6 in the paper)
# mcs_alpha=0.05; days="all"; nc=12; vers="5w"; depnum=1; full=F

# Or uncomment the following for mcs on all models for the main specification
# mcs_alpha=0.05; days="all"; nc=12; vers="5w"; depnum=1; full=T


#### FUNCTIONS AND FILES IN THE PARENT DIRECTORY #### 

#  Save the current working directory
parent_wd <- getwd()

# Source the helper functions script to load custom functions and load libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(here)
source(here::here('shared_functions.R'))  # to load function loadRData

# Load file with results
filename<-paste0("results_",vers,"_H",depnum)
model_results_wd="./models/results"
results<-readRDS(paste(model_results_wd,filename,sep="/"));gc()
print(paste("loaded file",filename))


#### FUNCTIONS AND FILES IN THE TABLES DIRECTORY #### 

# Working directory for tables
tables_wd <- "./tables"
setwd(tables_wd)

# Source the rest of the functions and libraries
source('tables_functions.R')  # to load functions process_dt and loss.dt
source('mcs_functions.R')     
print(sessionInfo())

# Load in a dictionary of model names 
model_dict <- read_delim("model.dict.csv",delim = ";", escape_double = FALSE, trim_ws = TRUE)
model_dict <- data.frame(model_dict)
print("loaded model_dict")


# Define wd for storing these results and create of it does not already exist
mcs_wd <- "./mcs"
create_wd(mcs_wd)

#### PARAMETERS ####

# MCS bootstrap samples
MCSB = 5000
# How many extreme observations to remove?
ext.rem = 12 # approx 2%
# Which loss functions to run
# losses=c("MSE","QLIKE","MAE","MAPE")
losses=c("MSE")
# Which statistics to use
if (full == T){
  statistics = 'TM';opt="full"
} else {
  statistics = 'TR';opt="pair"
}

# Subset days
if (days == "top"){
  perc=0.1
  quan="top"
}

if (days == "bottom"){
  perc=0.1
  quan="bottom"
}

if (days == "all"){
  perc=1
  quan="top"
}

# Select names of individual models + date and realized volatility
nms = c('Date','rv','har',"cslr_har_km5",'gen_dum',
        'gen_att','cslr_att_km5','lasso_att','rf_att',
        'gen_posneg_fin','cslr_posneg_fin_km5','lasso_posneg_fin','rf_posneg_fin')

# Select dataframes from list of results
selection=c("gen.att","att","gen.pos.fin.gen.neg.fin",
            "pos.fin.neg.fin","gen.dum","superbench")

print("loaded parameters")



#### RUN FUNCTION ####

tables.mcs = table.mcs(out = results,MCSB=MCSB,nc=nc,ext.rem=ext.rem, statistics=statistics,
                       mcs_alpha=mcs_alpha,nms=nms,perc=perc,quan=quan, selection=selection, 
                       model_dict=model_dict,losses=losses,paralel=TRUE,full=full)


#### STORE RESULTS ####

setwd(mcs_wd)

tblname=paste("mcs", mcs_alpha, days, vers, paste0("H",depnum),opt, sep="_")
saveRDS(tables.mcs, file = tblname)

setwd(my_wd)

print("Done")

