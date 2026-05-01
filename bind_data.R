# SCRIPT LOADS ATTENTION, SENTIMENT AND PRICES DATASETS
# THEN IT MERGES EVERYTHING AND SAVES NEW DATASET READY FOR PREDICTIONS


# Source the helper functions script to load custom functions and load libraries
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(here)
  source(here::here('shared_functions.R'))
  source(here::here('stockdata/stockdata_functions.R'))
 
  
################################# Load all datasets ############################   
# Load all datasets from the "dataset" folder
  
  dataset_wd = "./dataset"
  setwd(dataset_wd)
  load("bloomberg.RData")
  load("gt.RData")
  load("wiki.RData")
  load("pq.RData")
  load("tw.RData")
  
  # More explanatory variables: VIX, S&P500 RV
  vix <- read_csv("VIXCLS.csv"); vix <- as.data.frame(vix)
  rownames(vix) <- vix$observation_date; vix <- vix[,-1, drop=F]
  colnames(vix) <- c("VIX")
  
  # SP500 RV
  load("SP_RV.RData")
  

  ################################# Merge ########################################
  
  # Bind dataframes with identical number of rows
  dataset<-cbind(gt,wiki)  
  # Add bloomberg 
  dataset<-merge(bloomberg,dataset, by="row.names", all = F)
  rownames(dataset)<-dataset$Row.names
  dataset<-dataset[,-1]
  # Add tw 
  dataset<-merge(dataset,tw, by="row.names", all = F)
  rownames(dataset)<-dataset$Row.names
  dataset<-dataset[,-1]
  # Add pq 
  dataset<-merge(dataset,pq, by="row.names", all = F)
  rownames(dataset)<-dataset$Row.names
  dataset<-dataset[,-1]
  # Add vix
  dataset<-merge(dataset,vix, by="row.names", all = F)
  rownames(dataset)<-dataset$Row.names
  dataset<-dataset[,-1]
  # Add SP500 RV
  dataset<-merge(dataset,SP_RV, by="row.names", all = F)
  rownames(dataset)<-dataset$Row.names
  dataset<-dataset[,-1]

  # Save
  save(dataset,file="dataset.RData")
  rm(bloomberg,gt,wiki,tw,pq,vix,SP_RV); gc()
  setwd(my_wd)


################################# Add individual stocks data ########################
  
################################# Load RV files ################################ 
  
  # Load stockdata_functions.R from the stockdata folder
  source(here::here('stockdata/stockdata_functions.R'))
  
  # Paths to files
  path_1hf="./stockdata/data/1hfs/"
  path_5hf="./stockdata/data/5hfs/"
  path_final ="./stockdata/final/"; create_wd(path_final) 
  path_final_other ="./stockdata/final/other_versions/"; create_wd(path_final_other)
  
  # Load, bind and save data with RV variables that was created in batches (script "hf_dataset.R")
  hf1=load_bind(load_path=path_1hf, save_path=path_final, my_wd, filenam="hf1")
  hf5=load_bind(load_path=path_5hf, save_path=path_final, my_wd, filenam="hf5")
  gc()
  
######################## Add cross-sectional average RV ########################   
  
  # Select columns for which to compute cross-sectional average RV
  column_nams=c("VON.L1","VON.L5", "VON.L22","V.L1","V.L5", "V.L22")
  # Minimum number of observations required to compute cross-sectional average RV from
  min_N=2500
  # Log transform the CRV variables? 
  LogTrans=TRUE
  hf1 <- compute_crv(hf1, column_nams=column_nams, min_N=min_N, LogTrans=LogTrans,
                     save_path=path_final, my_wd=my_wd, filenam="hf1_crv");gc()
  hf5 <- compute_crv(hf5, column_nams=column_nams, min_N=min_N, LogTrans=LogTrans,
                     save_path=path_final, my_wd=my_wd, filenam="hf5_crv");gc()

  
###################### Merge data on stocks with att/sent ######################
  
  # Concatenate with sentiment and attention data
  hf_market_full_1 = SentiStock(hf1,senti=dataset); rm(hf1);gc()
  hf_market_full_5 = SentiStock(hf5,senti=dataset); rm(hf5);gc()
  
  
########### Split into weighted and non-weighted version and save ############## 
  
  
  #### Version with weights #### 
  
  # Drop columns that use the non-weighted version of full day price variation (and also weights, which are not needed)
  all_col_nms = colnames(hf_market_full_1[[1]])
  # subset column names, that include "VON." or ".VON." from all_col_nms
  von_col_nms = all_col_nms[grepl("VON\\.", all_col_nms) | grepl("\\.VON\\.", all_col_nms)]
  drop_cols_w = c(von_col_nms, "w1","w2","OJC","Date.1")
  
  hf_market_1w=lapply(hf_market_full_1, function(x) x[!(names(x) %in% drop_cols_w)])
  hf_market_5w=lapply(hf_market_full_5, function(x) x[!(names(x) %in% drop_cols_w)])
  
  # Save
  setwd(my_wd); setwd(path_final)
  saveRDS(hf_market_1w,file="hf_market_1w")
  saveRDS(hf_market_5w,file="hf_market_5w")
  
  # RData version 2
  setwd(my_wd); setwd(path_final_other)
  save(hf_market_1w,file="hf_market_1w_2.RData",version = 2)
  save(hf_market_5w,file="hf_market_5w_2.RData",version = 2)
  rm(hf_market_1w,hf_market_5w);gc()
  
  
  #### Simple version without weights ####
  
  # Drop columns that use the weighted version of full day price variation (and also weights, which are not needed)
  all_col_nms = colnames(hf_market_full_1[[1]])
  # subset all_col_nms to those, that start with "V." with nothing before
  v_col_nms = all_col_nms[grepl("^V\\.", all_col_nms) | grepl("\\.V\\.", all_col_nms)]
  drop_cols_s = c(v_col_nms, "w1","w2","OJC","Date.1")
  
  hf_market_1s=lapply(hf_market_full_1, function(x) x[!(names(x) %in% drop_cols_s)])
  hf_market_5s=lapply(hf_market_full_5, function(x) x[!(names(x) %in% drop_cols_s)])
  
  
  # Rename columns "VON.L1" (simple sum of day and night) etc to "V.L1" etc (we can easily refer to V.L1)
  # base_names <- c("L1", "L5", "L22", "H1", "H5", "H22")
  # log_suffix <- ".Log"
  # old_col_name <- c(paste0("VON.", base_names), paste0("VON.", base_names, log_suffix))
  # new_col_name <- c(paste0("V.", base_names), paste0("V.", base_names, log_suffix))
  old_col_name=von_col_nms
  new_col_name=v_col_nms
  
  
  # Loop over all dataframes in the list and rename columns
  for (i in 1:length(hf_market_1s)){
    old_col_idx<-which(colnames(hf_market_1s[[i]]) %in% old_col_name)
    colnames(hf_market_1s[[i]])[old_col_idx]<-new_col_name
  }
  for (i in 1:length(hf_market_5s)){
    old_col_idx<-which(colnames(hf_market_5s[[i]]) %in% old_col_name)
    colnames(hf_market_5s[[i]])[old_col_idx]<-new_col_name
  }
  
  # Save
  setwd(my_wd); setwd(path_final)
  saveRDS(hf_market_1s,file="hf_market_1s")
  saveRDS(hf_market_5s,file="hf_market_5s")
  
  # RData version 2
  setwd(my_wd); setwd(path_final_other)
  save(hf_market_1s,file="hf_market_1s_2.RData",version = 2)
  save(hf_market_5s,file="hf_market_5s_2.RData",version = 2)
  rm(hf_market_5s,hf_market_1s,hf_market_full_1,hf_market_full_5);gc()
  
  
  