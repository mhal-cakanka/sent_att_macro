# HELPER FUNCTIONS FOR SCRIPT mcs_meta.R

# Load necessary libraries
if (!require("pacman")) install.packages("pacman")
# Check whether these libraries are installed, install if not and load all of them
pacman::p_load('MCS',foreach,doParallel)

# Save current wd
my_wd=getwd()



# Note: make sure to load functions process_dt and loss.dt from script tables_functions.R 


# The outer function to run the MCS test on multiple stocks on multiple models, with different settings
table.mcs = function(out,MCSB,nc,ext.rem,statistics, mcs_alpha=0.15, nms, 
                     perc=1, quan=c("top"), selection, model_dict,losses=c("MSE","QLIKE","MAE","MAPE"),
                     paralel=TRUE,full=T) {
  
  # Number of stocks
  NI = length(out)
  
  # Prepare an object to store MCS for all models together
  mcs.all = array(NA, dim=c(length(losses),2,5,length(nms)-2))
  dimnames(mcs.all)[[1]] = losses
  dimnames(mcs.all)[[2]] = c('LOSS','RANK')
  dimnames(mcs.all)[[3]] = c("In_pval","Rank","In_SSM","mcs_pvalue","MCS_M")
  dimnames(mcs.all)[[4]] = nms[-c(1,2)]
  
  # Prepare an object to store pairwise mcs test results for all models together
  dm_all = array(NA, dim=c(length(losses),2,length(nms)-2,length(nms)-2))
  dimnames(dm_all)[[1]] = losses
  dimnames(dm_all)[[2]] = c('LOSS','RANK')
  dimnames(dm_all)[[3]] = nms[-c(1,2)]
  dimnames(dm_all)[[4]] = nms[-c(1,2)]
  print("table mcs function prepared")
  
  # Depending on the parameter "full", use one of objects for results prepared above
  if(full == T){
    res_all<-mcs.all
  } else {
    res_all<-dm_all
  }
  
  # Set up for parallel computing
  cl <- makeCluster(nc)
  registerDoParallel(cl)
  st <- Sys.time()
  
  # Parallel "loop" over stocks
  TMP = foreach (i = 1:NI,.export=c('mcs_eval','loss.dt','process_dt','mcs_run','mcs_pair_run'),.packages = 'MCS') %dopar% {
    
    # Select a dataset
    dt = out[[i]]
    
    # Loop over loss functions
    for (ls in losses){
      print(ls)
      res_all<-mcs_eval(res_all=res_all,dt=dt,ext.rem=ext.rem,perc=perc,quan=quan,
                        selection=selection,model_dict=model_dict,calibri.loss=ls,
                        nms=nms,i=i,mcs_alpha=mcs_alpha,MCSB=MCSB,paralel=paralel,
                        statistics=statistics,full=full)  
      gc()
    }
    return(res_all)
  }
  
  stopCluster(cl)
  print(Sys.time() - st)
  print("mcs counted")
  

  # Prepare an object to store MCS for all stocks
  # Add one more dimension
  res_all_fin<-array(res_all, dim = c(dim(res_all),NI), 
                     dimnames = c(dimnames(res_all),list(names(out)))) 
  
  # Loop over list of TMP results from the parallel process and store it to the new object
  for (t in 1:length(TMP)) res_all_fin[,,,,t] = TMP[[t]]
  print("mcs aggregated")
  
  
  return(res_all_fin)
  
}


# Main helper function for the function table.mcs, used to run the MCS test on one stock for one type of loss function 
mcs_eval<-function(res_all,dt,ext.rem,perc,quan,selection,model_dict,calibri.loss="MSE",
                   nms,i,mcs_alpha,MCSB,paralel=T,statistics,full=T){
  
  # PREPARE A LOSS DATASET
  temp.calibri.loss = calibri.loss
  if (calibri.loss=="MAE") temp.calibri.loss="MSE"
  if (calibri.loss=="MAPE") temp.calibri.loss="MSE"
  if (calibri.loss=="QLIKE") temp.calibri.loss="MSE"
  dt.loss <- loss.dt(dt=dt, selection=selection, model_dict=model_dict, calibri.loss=temp.calibri.loss, nms)
  
  # POSITION OF MODELS
  idx.mod = c(1:dim(dt.loss)[2])[-which(names(dt.loss) %in% c('Date','rv'))]
  
  # NUMBER OF MODELS
  NM = length(idx.mod)
  
  # Remove extreme values & choose quantile based on RV on days
  dt.loss <- process_dt(dt.loss,ext.rem,quan,perc)
  
  # LOSS STORE
  LS = matrix(NA,nrow=dim(dt.loss)[1],ncol=length(nms)-2)
  colnames(LS) = nms[-c(1,2)]
  if (calibri.loss=="QLIKE") dt.loss$rv[dt.loss$rv==0] = 1
  
  
  # LOOP OVER MODELS
  for (j in 1:NM) {
    
    # Loss
    if (calibri.loss == "MSE"){
      LS[,j]=(dt.loss$rv-dt.loss[,idx.mod[j]])^2
    }
    if (calibri.loss == "QLIKE"){
      LS[,j]=dt.loss$rv/dt.loss[,idx.mod[j]] - log(dt.loss$rv/dt.loss[,idx.mod[j]]) - 1
    }
    if (calibri.loss =="MAE"){
      LS[,j]=abs(dt.loss$rv-dt.loss[,idx.mod[j]])
    }
    if (calibri.loss == "MAPE"){
      LS[,j]=abs((dt.loss$rv-dt.loss[,idx.mod[j]])/dt.loss$rv)
    }
  }
  
  # Remove empty values
  LS = LS[complete.cases(LS),]
  
  # Ranks
  RS<-LS;RS[]<-NA
  for (day in 1:nrow(RS)) RS[day,]<-rank(LS[day,])
  
  # Run MCS on all models at once
  if(full == T){
    mcs.all<-res_all
    # Run MCS on loss
    mcs.all<-mcs_run(SS=LS,set="LS",mcs_alpha,MCSB,mcs.all,calibri.loss=calibri.loss,i,paralel=paralel,statistics=statistics)
    # Run MCS on ranks
    mcs.all<-mcs_run(SS=RS,set="RS",mcs_alpha,MCSB,mcs.all,calibri.loss=calibri.loss,i,paralel=paralel,statistics=statistics)
    res_all<-mcs.all
  # Or run it pair-wise 
  } else {
    dm_all<-res_all
    dm_all <-mcs_pair_run(SS=LS,set="LS",mcs_alpha=mcs_alpha,MCSB,dm_all,calibri.loss,i,paralel=paralel,statistics=statistics)
    res_all<-dm_all
  }
  
  
  return(res_all)
}


# Inner function for mcs_eval to run MCS on all models at once 
mcs_run<-function(SS,set="LS",mcs_alpha,MCSB,mcs.all,calibri.loss,i,paralel=T,statistics){
  
  if (set=="LS") s=1
  if (set=="RS") s=2
  
  if (calibri.loss == "MSE") cl=1
  if (calibri.loss == "QLIKE") cl=2
  if (calibri.loss == "MAE") cl=3
  if (calibri.loss == "MAPE") cl=4
  
  # Now across all models
  if (statistics=='TM') tmp = MCSprocedure(SS,alpha=mcs_alpha,B=MCSB,verbose=FALSE,statistic='Tmax')
  if (statistics!='TM') tmp = MCSprocedure(SS,alpha=mcs_alpha,B=MCSB,verbose=FALSE,statistic='TR')
  
  print(tmp)
  print(tmp@Info$mcs_pvalue)
  
  # Which statistics
  idx.sel = ifelse(statistics=='TM',3,6)
  
  # Store results for one stock
  if (paralel == T){
    mcs.all[cl,s,1,which(dimnames(mcs.all)[[4]] %in% names(tmp@show[,1]))]=ifelse(tmp@Info$mcs_pvalue < 0.05,1,0)
    mcs.all[cl,s,2,which(dimnames(mcs.all)[[4]] %in% names(tmp@show[,1]))]=tmp@show[,idx.sel-2]  # Rank
    mcs.all[cl,s,3,which(dimnames(mcs.all)[[4]] %in% names(tmp@show[,1]))]=1                     # In_SSM  
    mcs.all[cl,s,4,]=tmp@Info$mcs_pvalue                                                         # mcs_pvalue
    mcs.all[cl,s,5,which(dimnames(mcs.all)[[4]] %in% names(tmp@show[,1]))]=tmp@show[,idx.sel]    # MCS_M
  # Or store the number of stock under "i"
  } else {
    mcs.all[cl,s,1,which(dimnames(mcs.all)[[4]] %in% names(tmp@show[,1])),i]=ifelse(tmp@Info$mcs_pvalue < 0.05,1,0)
    mcs.all[cl,s,2,which(dimnames(mcs.all)[[4]] %in% names(tmp@show[,1])),i]=tmp@show[,idx.sel-2]  # Rank
    mcs.all[cl,s,3,which(dimnames(mcs.all)[[4]] %in% names(tmp@show[,1])),i]=1                     # In_SSM  
    mcs.all[cl,s,4,,i]=tmp@Info$mcs_pvalue                                                         # mcs_pvalue
    mcs.all[cl,s,5,which(dimnames(mcs.all)[[4]] %in% names(tmp@show[,1])),i]=tmp@show[,idx.sel]    # MCS_M
  }
  
  
  return(mcs.all)
}


# Inner function for mcs_eval to run pairwise MCS on all combinations of models
mcs_pair_run <-function(SS,set="LS",mcs_alpha=0.05,MCSB,dm_all,calibri.loss,i,paralel=T,statistics='TR'){
  
  if (set=="LS") s=1
  if (set=="RS") s=2
  
  if (calibri.loss == "MSE") cl=1
  if (calibri.loss == "QLIKE") cl=2
  if (calibri.loss == "MAE") cl=3
  if (calibri.loss == "MAPE") cl=4
  
  # Loop over models 2 times to get a pair of models
  for (k in 1:dim(SS)[2]){
    for (j in c(1:dim(SS)[2])[-k]){
      print(paste("models",k, "and", j))
      
      # Run the MCS procedure on this pair of models (their loss functions)
      tmp=MCSprocedure(SS[,c(k,j)],alpha=mcs_alpha,B=MCSB,verbose=T,statistic=statistics)
      ord=tmp@show[,4, drop = FALSE]
      # Get the names of the models
      j_name=colnames(SS)[j]
      k_name=colnames(SS)[k]
      
      # Find out, if model j is in the superior set of models (SSM)
      # If model j is in SSM, save its rank in j_state
      j_state = tryCatch(ord[j_name,], error = function(e) e)
      # If it is not, set variable j_state to NA
      if (inherits(j_state, 'error')) j_state=NA
      # Now check, if model k is in SSM
      k_state = tryCatch(ord[k_name,], error = function(e) e) 
      # If it is not, that means, k is not in SSM and j is the only one in SSM, set j_state to O
      if (inherits(k_state, 'error')) j_state=0
      
      # Notes: 
      # We only store the j_state with these possible outputs:
      # j_state=NA: j is not in SSM, k is the only model left, it is superior, k has rank 1
      # j_state=0:  k is not in SSM, j is the only model left, it is superior, j has rank 1
      # j_state=1:  both models are the SSM, MCS cannot distinguish them, but j is better, j has rank 1, k has rank 2
      # j_state=2:  both models are the SSM, MCS cannot distinguish them, but k is better, k has rank 1, j has rank 2
      
      # If we run this function within in parallel processes, the output should be for one stock (i) only
      if (paralel==T){
        dm_all[cl,s,k,j] = j_state
      } else {
        # Otherwise the last dimension is the number of stocks
        dm_all[cl,s,k,j,i] = j_state 
      }
      
    }
  }
  return(dm_all)
}

