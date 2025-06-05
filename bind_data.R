# SRIPT LOADS ATTENTION, SENTIMENT AND PRICES DATASETS
# THEN IT MERGES EVERYTHING AND SAVES NEW DATASET READY FOR PREDICTIONS


# Source the helper functions script to load custom functions and load libraries
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(here)
  source(here::here('shared_functions.R'))
 
  
################################# Load all datasets ############################   
# Load all datasets from the "dataset" folder
  
  dataset_wd = "./dataset"
  setwd(dataset_wd)
  load("bloomberg.RData")
  load("gt.RData")
  load("wiki.RData")
  load("pq.RData")
  load("tw.RData")

  
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

  # Save
  save(dataset,file="dataset.RData")
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
  
  
###################### Merge data on stocks with att/sent ######################
  
  # Concatenate with sentiment and attention data
  hf_market_full_1 = SentiStock(hf1,senti=dataset); rm(hf1);gc()
  hf_market_full_5 = SentiStock(hf5,senti=dataset); rm(hf5);gc()
  
  
########### Split into weighted and non-weighted version and save ############## 
  
  
  #### Version with weights #### 
  
  # Drop columns that use the non-weighted version of full day price variation (and also weights, which are not needed)
  drop_cols=c("w1","w2","OJC","VON.L1","VON.L5","VON.L22","VON.H1","VON.H5","VON.H22",
              "VON.L1.Log","VON.L5.Log","VON.L22.Log","VON.H1.Log","VON.H5.Log","VON.H22.Log","Date.1")
  hf_market_1w=lapply(hf_market_full_1, function(x) x[!(names(x) %in% drop_cols)])
  hf_market_5w=lapply(hf_market_full_5, function(x) x[!(names(x) %in% drop_cols)])
  
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
  drop_cols=c("w1","w2","OJC","V.L1","V.L5","V.L22","V.H1","V.H5","V.H22",
              "V.L1.Log","V.L5.Log","V.L22.Log","V.H1.Log","V.H5.Log","V.H22.Log","Date.1")
  hf_market_1s=lapply(hf_market_full_1, function(x) x[!(names(x) %in% drop_cols)])
  hf_market_5s=lapply(hf_market_full_5, function(x) x[!(names(x) %in% drop_cols)])
  
  
  # Rename columns "VON.L1" (simple sum of day and night) etc to "V.L1" etc (we can easily refer to V.L1)
  base_names <- c("L1", "L5", "L22", "H1", "H5", "H22")
  log_suffix <- ".Log"
  old_col_name <- c(paste0("VON.", base_names), paste0("VON.", base_names, log_suffix))
  new_col_name <- c(paste0("V.", base_names), paste0("V.", base_names, log_suffix))
  
  
  # Loop over all dataframes in the list and rename columns
  
  for (i in 1:length(hf_market_1s)){
    old_col_idx<-which(colnames(hf_market_1s[[i]]) %in% old_col_name)
    colnames(hf_market_1s[[i]])[old_col_idx]<-new_col_name
    }
  for (i in 1:length(hf_market_5s)){
    old_col_idx<-which(colnames(hf_market_5s[[i]]) %in% old_col_name)
    colnames(hf_market_5s[[i]])[old_col_name]<-old_col_idx
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
  
  
  