# TABLES WITH RESULTS



################### Load datasets, functions and define wds  ###################

#  Save the current working directory
parent_wd <- getwd()

# Load shared functions
if (!require("pacman")) install.packages("pacman")
pacman::p_load(here)
source(here::here('shared_functions.R'))

# Source model functions from folder "./models" to get 2 functions
source(here::here('models','model_functions.R'))

# Data wd
data_wd <- "./stockdata/final/other_versions"
setwd(data_wd)

# Load data - list of dataframes with all variables - each dataframe for one stock
vers="5w"
depnum=1
filename=paste0("hf_market_",vers,"_2.RData")
stocks.market <- loadRData(filename);gc()
print(paste("loaded file",filename))
# Set the working directory to the main directory
setwd(parent_wd)


# Load results - list of dataframes with results for each stock and model
filename<-paste0("results_",vers,"_H",depnum)
model_results_wd="./models/results"
results<-readRDS(paste(model_results_wd,filename,sep="/"));gc()
print(paste("loaded file",filename))

# Set the working directory to the main directory
setwd(parent_wd)

# Working directory for tables
tables_wd <- "./tables"
setwd(tables_wd)

# Load table functions
source('tables_functions.R')

# Working directory for saving tables
results_wd <- "./results"
# Create if it doesn't exist
create_wd(results_wd)


################ Overview of Tables as referred to in the paper ################

# Tables in the main body of the paper:
# Table 1 - list of macro news announcements -> created manually
# Table 2 - list of types of variables -> created manually
# Table 3 - descriptive statistics for realized measures and general attention and sentiment variables
# Table 4 - insample models
# Table 5 - out-of-sample models - results for the main specification (5w = 5min RV and weighted fullday RV, MSE)
# Table 6 - pairwise MCS, 5w, 5%, MSE
# Table 7 - average overperformance to the HAR model in sectors
# Table 8 - out-of-sample models - results for the main specification, top 10% days with highest realised volatility
# Table 9 - out-of-sample models - results for the main specification, bottom 90% days with lowest realised volatility
# Table 10 - variable importance from CSR models (macro event / source of variable)

# Tables in the Appendix:
# Table A1 - list of macro event keywords -> created manually
# Table A2 - list of macro event keywords for wikipedia specifically -> created manually
# Table A3 - descriptive statistics for event-related attention variables
# Table A4 - descriptive statistics for event-related sentiment variables
# Table A5 - pairwise one-sided Wilcoxon signed-rank test

# Tables in Supplementary material:
# 2 Summary tables of out-of-sample results for alternative specifications
# Table S1 - robustness checks: proportion of cases the column model out-performed HAR
# Table S2 - robustness checks: average forecast improvement compared HAR


######################## Table 3 ########################
# Table 3 - Descriptive statistics for realized measures and general attention and sentiment variables

# Set parameters
# Minimum number of observations to have
L = 2500
# Log transformation
formats = c('Log')
# Variables of interest
interest = c('V.L1.Log',"JC.L1.Log","CC.L1.Log","NSV.L1.Log","PSV.L1.Log","SJ.L1.Log", # realised measures
             'gg.1','wg.1','tg.1','pg.1',   # general attention
             'tg.9','pg.9','tg.10','pg.10') # general finbert sentiment
# Variables to be transformed
totrans=interest[-c(1:6)]

# Run the table_dscr function and save
table3 <- table_dscr(stocks.market,L,interest,formats,totrans)
savetable(tablefile=table3, wd=results_wd,tblname=paste("table3",formats,sep="_"), vers = vers, depnum=depnum)


######################## Table 4 ########################
# Table 4 - insample models

# Specify models to be estimated
specs = list()
specs[['HAR']] = as.formula(V.H1.Log ~ V.L1.Log+V.L5.Log+V.L22.Log)
specs[['HAR-A']] = as.formula(V.H1.Log ~ V.L1.Log+V.L5.Log+V.L22.Log+gg.1+wg.1+tg.1+pg.1)
specs[['HAR-S']] = as.formula(V.H1.Log ~ V.L1.Log+V.L5.Log+V.L22.Log+tg.9+pg.9+tg.10+pg.10)
specs[['HAR-M']] = as.formula(V.H1.Log ~ V.L1.Log+V.L5.Log+V.L22.Log+
                           I(V.L1.Log*b.1)+I(V.L1.Log*b.2)+I(V.L1.Log*b.3)+I(V.L1.Log*b.4)+I(V.L1.Log*b.5)+
                           I(V.L1.Log*b.6)+I(V.L1.Log*b.7)+I(V.L1.Log*b.8)+I(V.L1.Log*b.9)+I(V.L1.Log*b.10))

# Run the table_insample function and save
table4 <- table_insample(stocks.market,specs,formats = c('Log'))
savetable(tablefile=table4,vers="5w", depnum=1,wd=results_wd, tblname="table4")


######################## Table 5 ########################
# Table 5 - out-of-sample models - results for the main specification (5w = 5min RV and weighted fullday RV, MSE loss)

# Out-Sample models
# How many extreme observations to remove?
ext.rem = 12 # approx 2%

# Select names of individual models + date and realized volatility
nms = c('Date','rv','har',"cslr_har_km5",'gen_dum',   # date, rv, benchmark models
        'gen_att','cslr_att_km5','lasso_att','rf_att',# attention models
        'gen_posneg_fin','cslr_posneg_fin_km5','lasso_posneg_fin','rf_posneg_fin') # sentiment models

# Select dataframes from list of results
selection=c("gen.att","att","gen.pos.fin.gen.neg.fin",
            "pos.fin.neg.fin","gen.dum","superbench")

# Load in a dictionary of model names
model_dict <- read_delim("model.dict.csv",delim = ";", escape_double = FALSE, trim_ws = TRUE)
model_dict <-data.frame(model_dict)

# Subset days
perc=1; quan=c("top")  # top 100% days with highest realized volatility = all data

# Select losses to be reported
losses=c("MSE")
# Use loss=c("MSE","QLIKE","MAE","MAPE") to report all losses

# Run the table_outsample function and save
table5 <- table_outsample(out=results,ext.rem=ext.rem, perc=perc, quan=quan, selection=selection, 
                          model_dict=model_dict, nms=nms,losses=losses, depnum=depnum)
savetable(tablefile=table5,vers=vers, depnum=depnum,wd=results_wd, tblname="table5_MSE")


######################## Table 6 ########################
# Table 6 - pairwise MCS, 5w, 5%, MSE

# To run the MCS test on results, run mcs_meta.R
# The following code loads the output from mcs_meta.R and aggregates it into final tables

# PARAMETERS
# Select these parameters to get table 6
MM=c(0.05);DD=c("all");VV=c("5w");DN=c(1)
mcs_wd="./mcs"
current_wd=getwd()


# Run to aggregate into tables. The function both outputs and saves results via function savetables().
output <- process_mcs_pair(mcs_alpha,days,vers,depnum,current_wd,mcs_wd=mcs_wd,results_wd = results_wd)
table6 <- output[["LOSS.MSE_ssm"]]
savetable(tablefile=table6,vers=vers, depnum=depnum,wd=results_wd, tblname="table6")

# These are all the possible options:
# MM=c(0.05,0.25)
# DD=c("all","bottom","top")
# VV=c("5w","1w","5s","1s")
# DN=c(1,5,22)

# To run multiple options:
# for (days in DD){
#   for (vers in VV){
#     for (depnum in DN){
#       for (mcs_alpha in MM){
#         process_mcs_pair(mcs_alpha,days,vers,depnum,current_wd,mcs_wd=mcs_wd,results_wd = results_wd)
#       }
#     }
#   }
# }


# To get a table of the MCS test on full set of models:
# table6_all_0.05 <- process_mcs(mcs_alpha, days, vers, depnum, current_wd, mcs_wd = "./mcs", results_wd = "./results")
# table6_all_0.25 <- process_mcs(mcs_alpha=0.25, days, vers, depnum, current_wd, mcs_wd = "./mcs", results_wd = "./results")
  
# To run multiple options:
# for (days in DD){
#   for (vers in VV){
#     for (depnum in DN){
#       for (mcs_alpha in MM){
#         process_mcs(mcs_alpha,days,vers,depnum,current_wd,mcs_wd=mcs_wd,results_wd = results_wd)
#       }
#     }
#   }
# }


######################## Table 7 ########################
# Table 7 - average overperformance to the HAR model in sectors

link="https://en.wikipedia.org/w/index.php?title=List_of_S%26P_500_companies&oldid=1106065758"
# Run the table_sectors function and save
table7<-table_sectors(link=link,out=results,ext.rem=ext.rem,loss=c('MSE','QLIKE','MAE','MAPE'),selection=selection, model_dict=model_dict, nms=nms)
# Note: we report table7$MSE.overperform as table 7 in the paper, but we save all results in the table7 object
savetable(tablefile=table7, tblname="table7", vers = vers, depnum=depnum, wd=results_wd)


######################## Table 8 ########################
# Table 8 - out-of-sample models - results for the main specification, top 10% days with highest realised volatility

# Use the same parameters as for Table 5 (results file, ext.rem, selection, model_dict, nms, losses)

# Subset days
perc=0.1; quan=c("top")  # top 100% days with highest realized volatility = all data

# Run the table_outsample function and save
table8 <- table_outsample(out=results,ext.rem=ext.rem, perc=perc, quan=quan, selection=selection, 
                          model_dict=model_dict, nms=nms,losses=losses, depnum=depnum)
savetable(tablefile=table8,vers=vers, depnum=depnum,wd=results_wd, tblname="table8_top10")


######################## Table 9 ########################
# Table 9 - out-of-sample models - results for the main specification, bottom 90% days with lowest realised volatility


# Use the same parameters as for Table 5 (results file, ext.rem, selection, model_dict, nms, losses)

# Subset days
perc=0.1; quan=c("bottom")  # bottom 90% days with lowest realized volatility

table9 <- table_outsample(out=results,ext.rem=ext.rem, perc=perc, quan=quan, selection=selection, 
                          model_dict=model_dict, nms=nms,losses=losses, depnum=depnum)
savetable(tablefile=table9,vers=vers, depnum=depnum,wd=results_wd, tblname="table9_bottom90")


######################## Table 10 ########################
# Table 10 - variable importance from CSR models (macro event / source of variable)

# Select which dataset to use


selects = c("att.cf.cslr.optim",'pos.fin.neg.fin.cf.cslr.optim')


# Load nams.csv from folder "./dataset"
setwd(parent_wd)
nams = read.csv(file='./dataset/nams.csv');nams = data.frame(nams)
setwd(tables_wd)

table10 <- table_vimp(selects=selects, nams=nams)
savetable(tablefile=table10,vers=vers, depnum=depnum,wd=results_wd, tblname="table10_vimp")


######################## Table A3 ########################
# Table A3 - descriptive statistics for event-related attention variables

# Set parameters
# Minimum number of observations to have
L = 2500
# Log transformation
formats = c('Log')
# Variables of interest
interest=c(paste("ge",seq(1:10),sep="."),paste("we",seq(1:10),sep="."),
           paste("te",c(1:10),sep="."),paste("pe",c(1:10),sep="."))
# Variables to be transformed
totrans=interest

tableA3 <- table_dscr(stocks.market,L,interest,formats,totrans)
savetable(tablefile=tableA3, wd=results_wd,tblname=paste("tableA3",formats,sep="_"), vers = vers, depnum=depnum)


######################## Table A4 ########################
# Table A4 - descriptive statistics for event-related sentiment variables

# Set parameters
# Minimum number of observations to have
L = 2500
# Log transformation
formats = c('Log')
# Variables of interest
interest = c(paste("te",c(81:100),sep="."),
             paste("pe",c(81:100),sep="."))
# Variables to be transformed
totrans=interest

tableA4 <- table_dscr(stocks.market,L,interest,formats,totrans)
savetable(tablefile=tableA4, wd=results_wd,tblname=paste("tableA4",formats,sep="_"), vers = vers, depnum=depnum)


######################## Table A5 ########################
# Table A5 - pairwise one-sided Wilcoxon signed-rank test on losses

# Set parameters
ext.rem = 12 # approx 2% extreme observations to remove
# Select names of individual models + date and realized volatility
nms = c('Date','rv','har',"cslr_har_km5",'gen_dum',   # date, rv, benchmark models
        'gen_att','cslr_att_km5','lasso_att','rf_att',# attention models
        'gen_posneg_fin','cslr_posneg_fin_km5','lasso_posneg_fin','rf_posneg_fin') # sentiment models
# Select dataframes from list of results
selection=c("gen.att","att","gen.pos.fin.gen.neg.fin",
            "pos.fin.neg.fin","gen.dum","superbench")
# Subset days
perc=1; quan=c("top")  # top 100% days with highest realized volatility = all data
# Loss function(s)
losses = c("MSE")
# Load in a dictionary of model names
model_dict <- read_delim("model.dict.csv",delim = ";", escape_double = FALSE, trim_ws = TRUE)
model_dict <-data.frame(model_dict)


# Prepare preliminary table with losses
store.tbl<-get_store.tbl(out=results,ext.rem=ext.rem, perc=perc,quan=quan,selection=selection, 
                         model_dict=model_dict, nms=nms,losses=losses)
NM = dim(store.tbl)[3]

# Loop over losses
for (l in losses){
  tableA5<-c()
  # Loop over models
  for (col_m in 1:NM){
    # Apply function ws_run to estimate a pairwise one-sided Wilcoxon signed-rank test for the proxy_model
    w.res.dt<-ws_run(proxy_model_num=col_m,store.tbl=store.tbl,nms=nms,calibri.loss=l)
    tableA5 <-rbind(tableA5,w.res.dt)
  }
  # Save for this loss function
  savetable(tablefile=tableA5, wd=results_wd,tblname=paste("tableA5",l,sep="_"), vers = vers, depnum=depnum)
}


######################## Table S1 and S2 ########################
# 2 Summary tables of out-of-sample results for alternative specifications
# Table S1 - robustness checks: proportion of cases the column model out-performed HAR
# Table S2 - robustness checks: average forecast improvement compared HAR

# How many extreme observations to remove?
ext.rem = 12 # approx 2%
# Subset days
perc=1; quan=c("top")  # top 100% days with highest realized volatility = all data

# Select names of individual models + date and realized volatility
nms = c('Date','rv','har',"cslr_har_km5",'gen_dum',   # date, rv, benchmark models
        'gen_att','cslr_att_km5','lasso_att','rf_att',# attention models
        'gen_posneg_fin','cslr_posneg_fin_km5','lasso_posneg_fin','rf_posneg_fin') # sentiment models
nms.vad = c('Date','rv','har',"cslr_har_km5",'gen_dum',   
            'gen_att','cslr_att_km5','lasso_att','rf_att',
            'gen_posneg_vad','cslr_posneg_vad_km5','lasso_posneg_vad','rf_posneg_vad')
nms.emo = c('Date','rv','har',"cslr_har_km5",'gen_dum',   
            'gen_att','cslr_att_km5','lasso_att','rf_att',
            'gen_posneg_emo','cslr_posneg_emo_km5','lasso_posneg_emo','rf_posneg_emo')
# Select dataframes from list of results
selection=c("gen.att","att","gen.pos.fin.gen.neg.fin",
            "pos.fin.neg.fin","gen.dum","superbench")
selection_long=c("gen.att","att",
                 "gen.pos.emo.gen.neg.emo","pos.emo.neg.emo",
                 "gen.pos.vad.gen.neg.vad","pos.vad.neg.vad",
                 "gen.pos.fin.gen.neg.fin","pos.fin.neg.fin",
                 "gen.dum","superbench")


# Use table_outsample() with different parameters
configs <- list(
  list(name = "5s",     selection = selection, nms=nms,vers="5s", depnum=1, losses=c("MSE")),
  list(name = "1w",     selection = selection, nms=nms,vers="1w", depnum=1, losses=c("MSE")),
  list(name = "1s",     selection = selection, nms=nms,vers="1s", depnum=1, losses=c("MSE")),
  list(name = "emolex", selection = selection_long, nms=nms.emo, vers="5w", depnum=1, losses=c("MSE")),
  list(name = "vader",  selection = selection_long, nms=nms.vad, vers="5w", depnum=1, losses=c("MSE")),
  list(name = "qlike",  selection = selection, nms=nms, vers="5w", depnum=1, losses=c("QLIKE")),
  list(name = "mae",    selection = selection, nms=nms, vers="5w", depnum=1, losses=c("MAE")),
  list(name = "mape",   selection = selection, nms=nms, vers="5w", depnum=1, losses=c("MAPE")),
  list(name = "h5",     selection = selection, nms=nms, vers="5w", depnum=5, losses=c("MSE")),
  list(name = "h22",    selection = selection, nms=nms, vers="5w", depnum=22, losses=c("MSE"))
)

# Initialize an empty list to store results
tables <- list()

# Loop through configurations and aggregate results
for (config in configs) {
  print(config$name)
  
  # Remove existing results object
  rm(results);gc()
  
  # Set the working directory to the main directory
  setwd(parent_wd)
  
  # Load results
  filename<-paste0("results_",config$vers,"_H",config$depnum)
  model_results_wd="./models/results"
  results<-readRDS(paste(model_results_wd,filename,sep="/"));gc()
  print(paste("loaded file",filename))
  setwd(tables_wd)
  
  tables[[config$name]] <- table_outsample(
    out=results,
    ext.rem=ext.rem, 
    perc=perc, 
    quan=quan, 
    selection=config$selection, 
    model_dict=model_dict, 
    nms=config$nms,
    losses=config$losses, 
    depnum=config$depnum
    )
}

# Combine all results into one dataframe
tableS1 <- c()
tableS2 <- c()
model_names =c("HAR","CSR","HAR-M","HAR-A", "CSR-A", "ALA-A", "RF-A","HAR-S", "CSR-S", "ALA-S", "RF-S")

for (conf in names(tables)){
  print(conf)
  outperf <- data.frame(Spec=conf,tables[[conf]][[1]][1,-1]) 
  names(outperf)[-1] <- model_names
  tableS1 <- rbind(tableS1,outperf)
  
  ave_imp <- data.frame(Spec=conf,tables[[conf]][[1]][10,-1]) 
  names(ave_imp)[-1] <- model_names
  tableS2 <- rbind(tableS2,ave_imp)
}

# Save tables
savetable(tablefile=tableS1, wd=results_wd,tblname="tableS1", vers = NULL, depnum=NULL)
savetable(tablefile=tableS2, wd=results_wd,tblname="tableS2", vers = NULL, depnum=NULL)

