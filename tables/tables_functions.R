# HELPER FUNCTIONS FOR THE SCRIPT make_tables.R

# Load necessary libraries
if (!require("pacman")) install.packages("pacman")
# Check whether these libraries are installed, install if not and load all of them
pacman::p_load(moments,xlsx,lmtest,sandwich,readr,rvest,stats)


# A function to save tables in different formats
savetable <- function(tablefile,wd=NULL,tblname="table1",vers=NULL,depnum=NULL){
  
  # Piece together the name for the file
  if (is.null(wd)){
    wd_tbl_nam = paste0(tblname,"_")
  } else {
    wd_tbl_nam = paste0(wd, "/",tblname,"_")
  }
  
  if (is.null(vers)){
    vers_nam = ""
  } else {
    vers_nam = paste0(vers,"_")
  }
  
  if (is.null(depnum)){
    depnum_nam = ""
  } else {
    depnum_nam = paste0("H",depnum)
  }

  
  if (is.null(vers) & is.null(depnum)){
    tname = paste0(wd, "/",tblname)
  } else {
    tname = paste0(wd_tbl_nam,vers_nam,depnum_nam)
  }
  print(tname)
  
  # RData version 2
  save(tablefile,file=paste0(tname,".RData"),version = 2)
  
  # Excel
  excel.tbl(tablefile,tname)
}


# Save list of dataframes as an excel file with multiple sheets
# input: 
# tbl = one of the tables that we want to save as multiple sheet excel file
# tname = table name (how do we want to name this file?)
excel.tbl<-function(tbl,tname){
  # paste the table name and ".xlsx" to create the file name
  file <- paste(tname,".xlsx", sep = "")
  
  # loop over dataframes in a list to save one by one into individual worksheets
  if (is.data.frame(tbl)){
    write.xlsx(tbl, file) 
  } else {
    for (l in 1:length(tbl)){
      if (l == 1){
        write.xlsx(tbl[[l]], file, sheetName = names(tbl)[l]) 
      } else {
        write.xlsx(tbl[[l]], file, sheetName = names(tbl)[l], append = TRUE)
      }
    }
  }
  
  
}





######################## Descriptive stats Table ###############################

# Run on the stocks.market object (list of stock price variation dataframes)
# Minimum number of observations to have for each stock:  L = 2500
# Variables of interest: e.g. interest = c('V.H1','V.H1.Log','gg.1','wg.1','tg.1','pg.1',tg.3','pg.3','tg.4','pg.4')
# totrans = vector of variables to be transformed (e.g. log, etc.)
# formats = format of variables / option for the transform function
# Output: Mean, SD, Skew, Kurt, min. 5%, 50%, 95%, max. rho(c(1,5,22)) 
#         -> average values over the N stocks in stocks.market

table_dscr = function(stocks.market,L=2500,interest,formats = c('Log'),totrans=interest[-1]) {
  
  # To store transformed data
  stocks.market.log<-stocks.market
  
  # Identify which stocks have at least L observations
  idx.use = c();  for (i in 1:length(stocks.market)) if(dim(stocks.market[[i]])[1]>=L) idx.use = c(idx.use,i)
  
  # Table structure
  tbl = data.frame(Variable=interest,
                   mean=NA,sd=NA,Skew=NA,Kurt=NA,min=NA,q5=NA,q50=NA,q95=NA,max=NA,rho1=NA,rho5=NA,rho22=NA)
  
  aux = array(NA,dim=c(dim(tbl)[1],dim(tbl)[2]-1,length(idx.use)))
  dimnames(aux)[[1]] = interest
  dimnames(aux)[[2]] = colnames(tbl)[-1]
  dimnames(aux)[[3]] = names(stocks.market)[idx.use]
  
  # Loop over stocks with >=L observations
  for (i in 1:length(idx.use)) {
    
    # Select data for the ith stock
    dt = stocks.market[[idx.use[i]]]
    
    # TRANSFORM VARIABLES
    if (!is.null(formats)){
      # Transform the data
      dt = transform(dt,totrans=totrans,formats=formats,K=5,addto=FALSE)
      # store transformed data
      stocks.market.log[[idx.use[i]]] <- dt
    }  
    
    # Loop over variables of interest to compute statistics
    for (v in 1:length(interest)) {
      
      x = dt[,interest[v]]
      x = x[complete.cases(x)]
      aux[v,,i] = c(mean(x),sd(x),skewness(x),kurtosis(x),min(x),quantile(x,p=c(0.05,0.50,0.95)),max(x),acf(x,plot=F)[[1]][c(2,6,23)])
      
    }
    
  }
  
  # Compute the mean of the statistics over the N stocks
  for (i in 1:dim(tbl)[1]) tbl[i,-1] = apply(aux[i,,],1,mean,na.rm=T)
  
  return(tbl)
}



############################ In-sample results Table ############################


table_insample = function(stocks.market,specs,L = 2500,formats = c('Log')) {
  
  # Helper functions
  
  
  sumcf = function(x) sum(x>0,na.rm=T)/length(x)
  
  percpv = function(x,ps=0.05){
    perc=sum(x<=ps,na.rm=T)/length(x)
    perc=round(perc*100,4)
    return(c(ps,perc))
  }
  
  sumpv = function(x,ps=c(0.01,0.05,0.1),lim=0.99){
    ps=sort(ps,F)
    fin_perc=NULL;fin_p=NULL
    for (p in ps){
      perc=sum(x<=p,na.rm=T)/length(x)
      if (perc>=lim){
        fin_perc=perc
        fin_p=p
        # print(paste("found p",fin_p,"at",fin_perc))
        break
      } else if (p==max(ps)) {
        fin_perc=perc
        fin_p=p
        # print(paste("returning pval under limit",fin_p,"at",fin_perc))
      }
    }
    fin_perc=round(perc*100,4)
    return(c(fin_p,fin_perc))
  } 
  
  
  # Number of specifications
  NS = length(specs)
  
  
  # Identify which stock have at least L observations
  idx.use = c();  for (i in 1:length(stocks.market)) if(dim(stocks.market[[i]])[1]>=L) idx.use = c(idx.use,i)
  
  
  # All variables employed in specifications
  variables = c()
  for (i in 1:NS){
    # Subset to get variables for the ith specification
    new_variables = all.vars(specs[[i]])
    # Compare to the vector variables and only add new ones
    variables_to_add = setdiff(new_variables,variables)

    variables = c(variables,variables_to_add)
  }
  # variables = c(); for (i in 1:NS) variables = c(variables,all.vars(specs[[i]]))
  # Remove the dependent variable from the list of variables
  variables = unique(variables)[-1]
  # Do not transform RV variables (they start with "V") and bloomberg dummies (they start with "b")
  skipp=c(grep("^[b].*", variables),grep("^[V].*", variables))
  totrans=c(variables[-skipp])
  variables = c('Intercept',variables,'R2','aR2')
  # variables = c('Intercept',variables,'R2','aR2','max VIF','DW','DW pval', 'El','White test','BP')
  
  
  # Table
  cfs = matrix(NA,nrow=length(variables),ncol=4*NS)
  nms = c(); for (i in 1:NS) nms = c(nms,names(specs)[i],'cps',"pval","perc")
  colnames(cfs) = nms
  tbl = data.frame(names=variables,cfs)
  
  
  # Store coefficients and R2 aR2
  store.cfs = array(NA,dim=c(dim(tbl)[1],NS,length(idx.use),2))
  dimnames(store.cfs)[[1]] = tbl$names
  dimnames(store.cfs)[[2]] = names(specs)
  dimnames(store.cfs)[[3]] = names(stocks.market)[idx.use]
  dimnames(store.cfs)[[4]] = c("coef","pval")
  
  for (i in 1:length(idx.use)) {
    
    for (s in 1:NS) {
      
      print(paste(i,s))
      
      dt = stocks.market[[idx.use[i]]]
      
      # TRANSFORM VARIABLES
      if (!is.null(formats)){
        dt = transform(dt,totrans=totrans,formats=formats,K=5,addto=FALSE)
      }
      # Estimate the model
      m = lm(specs[[s]],data=dt)
      # Run the quadratic spectral kernel heteroskedasticity and autocorrelation consistent 
      # estimator with automatic bandwidth selection, following the procedure of Newey and West (1994)
      # Note: used to be just coeftest(m,vcov = kernHAC), this produces the same exact results
      m.ct <- coeftest(m,vcov=vcovHAC(m,kernel="Quadratic Spectral",bw=bwAndrews,prewhite=1,weights=weightsAndrews)) 
      # store coeficients
      store.cfs[c(1,which(dimnames(store.cfs)[[1]] %in% all.vars(specs[[s]]))),s,i,1] = coefficients(m.ct)
      # extract and store r2 and adj.r2
      store.cfs[c('R2','aR2'),s,i,1] = c(summary(m)$r.squared,summary(m)$adj.r.squared)
      # store p-values
      store.cfs[c(1,which(dimnames(store.cfs)[[1]] %in% all.vars(specs[[s]]))),s,i,2] = m.ct[,4]
      gc()
    }
    
  }
  print("done with the loop")
  
  for (s in 1:NS) {
    # Mean of coefficients
    tbl[,1+4*s-3] = apply(store.cfs[,s,,1],1,mean)
    # How many times (out of 404) is the coefficient positive?
    tbl[,1+4*s-2] = apply(store.cfs[,s,,1],1,sumcf)
    # How many times (out of 404 is the coefficient significant at least at pval=0.05?)
    tbl[,c(1+4*s-1,1+4*s)] <- t(apply(store.cfs[,s,,2],1,percpv,ps=0.05))
  }
  tbl[tbl==0]<-NA
  tbl[grep("^[b].*", tbl$names),1]=paste(tbl[grep("^[b].*", tbl$names),1],tbl[2,1],sep="*")
  
  return(tbl)
}


############################ Out-of-sample results Table ############################

table_outsample = function(out,ext.rem,perc=1,quan=c("top"),selection,model_dict,nms,
                           losses=c("MSE","QLIKE","MAE","MAPE"),depnum) {
  
  # Number of stocks
  NI = length(out)
  
  # Empty table to store results
  tbl = data.frame(description=c('Outperform HAR','Average Rank','5%','10%','25%','50%','75%','90%','95%','Mean'))
  res.out = matrix(NA,nrow=dim(tbl)[1],ncol=length(nms)-2)
  colnames(res.out) = nms[-c(1,2)]
  tbl = data.frame(tbl,res.out)
  
  # Loop over losses to create a list of results, each with the structure of tbl
  results = list()
  for (ls in losses){
    results[[ls]]<-tbl
  }
  rm(tbl)
  
  # Use function get_store.tbl to prepare preliminary version of the table
  store.tbl <- get_store.tbl(out=out, ext.rem=ext.rem, perc=perc, quan=quan, selection=selection, 
                             model_dict=model_dict, nms=nms, losses=losses, depnum=depnum)
  

  # TABLES
  NM = dim(store.tbl)[3]
  
  # Check tables
  print(summary(t(store.tbl[1,1,,])))
  print(colSums(is.na(t(store.tbl[1,1,,]))))
  
  # Loop over models and loss functions to build final tables with results
  for (m in 1:NM) {
    print(m)
    
    for (cl in 1:length(losses)){
      ls=losses[cl]
      
      # % Outperforming HAR
      results[[ls]][1,m+1] <- 100 * sum(store.tbl[cl,1,1,]-store.tbl[cl,1,m,]>0, na.rm=T)/NI
      
      # Average rank
      rank.tmp = c(); for (i in 1:NI) rank.tmp[i] = rank(store.tbl[cl,1,,i])[m]
      results[[ls]][2,m+1] = mean(rank.tmp)
      
      # quantiles of forecast improvements
      results[[ls]][3:9,m+1] = quantile(100*(store.tbl[cl,1,1,]-store.tbl[cl,1,m,])/store.tbl[cl,1,1,],p=c(0.05,0.10,0.25,0.50,0.75,0.90,0.95), na.rm=T)
      
      # Ave improvement
      results[[ls]][10,m+1] = mean(100*(store.tbl[cl,1,1,]-store.tbl[cl,1,m,])/store.tbl[cl,1,1,], na.rm=T)
    }
  }
  
  return(results)
}


# Helper function for table_outsample
loss_eval<-function(store.tbl.tmp,dt,ext.rem,perc,quan,selection,model_dict,
                    calibri.loss="MSE",nms,depnum){
  
  # if (calibri.loss == "MSE") cl=1
  # if (calibri.loss == "QLIKE") cl=2
  # if (calibri.loss == "MAE") cl=3
  # if (calibri.loss == "MAPE") cl=4
  cl=1
  
  # PREPARE A LOSS DATASET
  temp.calibri.loss = calibri.loss
  if (calibri.loss=="MAE") temp.calibri.loss="MSE"
  if (calibri.loss=="MAPE") temp.calibri.loss="MSE"
  if (calibri.loss=="QLIKE") temp.calibri.loss="MSE"
  
  
  dt.loss <- loss.dt(dt=dt, selection=selection, model_dict=model_dict, calibri.loss=temp.calibri.loss, nms)
  
  # If the we do a multiple day ahead prediction, divide the losses to get comparable results
  if (depnum>1){
    print(colMeans(dt.loss[,-1],na.rm=T))
    print("divided by depnum")
    dt.loss[,-1]<-dt.loss[,-1]/depnum
    print(colMeans(dt.loss[,-1],na.rm=T))
  }
  
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
    print(j)
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
    store.tbl.tmp[cl,1,j] = mean(LS[,j],na.rm=T)
  }
  
  return(store.tbl.tmp)
}


# Helper function for table_outsample
loss.dt <- function(dt, selection, model_dict, calibri.loss="MSE", nms){
  
  # Select categories of models to work with
  new.dt=dt[selection]
  # Loop through these and extract dates, concatanate into one log vector
  for (g in 1:length(new.dt)){
    if (g ==1){
      gd <-new.dt[[g]][,"Date"]
    } else {
      gd <-c(gd, new.dt[[g]][,"Date"])
    }
  }
  # Using a table function, extract dates present in all predictions
  gd = as.Date(names(table(gd))[which(table(gd)==length(new.dt))],format='%Y-%m-%d')
  # Subset with the series of common dates "gd"
  for (g in 1:length(new.dt)){
    if (g ==1){
      new.dt[[g]] <-new.dt[[g]][new.dt[[g]][,"Date"]%in% gd,]
      colnames(new.dt[[g]])[]
    } else {
      # Do the same, but remove columns with date and true values
      new.dt[[g]] <-new.dt[[g]][new.dt[[g]][,"Date"]%in% gd,-which(colnames(new.dt[[g]]) ==c("Date","true")), drop = FALSE]
    }
  }
  # Bind all dataframes into one, fix some column names
  fin.dt = do.call("cbind", new.dt); colnames(fin.dt)[1:2]<-c("Date","true")
  
  # Subset models
  dt.loss <- fin.dt[,model_dict[model_dict$MODELS %in% nms,calibri.loss]]
  # Rename
  colnames(dt.loss) <- model_dict[model_dict$MODELS %in% nms,"MODELS"]
  # Reorder
  dt.loss = dt.loss[,nms]
  return(dt.loss)
}

# Helper function for table_outsample
process_dt<-function(dt.loss,ext.rem,quan,perc){
  
  # REMOVE ext.rem MAX RV OBSERVATIONS
  dt.loss = dt.loss[-tail(order(dt.loss$rv),n=ext.rem),]
  
  # Choose which observations to consider, for perc=1 and quan="top" we take all observations
  if (quan == "top"){
    dt.loss=dt.loss[tail(order(dt.loss$rv),n=nrow(dt.loss)*perc),]
  }
  
  if (quan == "bottom"){
    dt.loss=dt.loss[-tail(order(dt.loss$rv),n=nrow(dt.loss)*perc),]
  }
  
  dt.loss=dt.loss[order(dt.loss$Date),]
  
  return(dt.loss)
}

# Helper function for table_outsample and others
get_store.tbl = function(out,ext.rem,perc=1,quan=c("top"),selection,model_dict,nms,losses=c("MSE","QLIKE","MAE","MAPE"),depnum=1) {
  
  
  # Number of stocks
  NI = length(out)
  
  
  # Store results across stocks
  store.tbl = array(NA,dim=c(length(losses),1,length(nms)-2,NI))
  dimnames(store.tbl)[[1]] = losses
  dimnames(store.tbl)[[2]] = c('AVE')
  dimnames(store.tbl)[[3]] = nms[-c(1,2)]
  dimnames(store.tbl)[[4]] = names(out)
  
  # Loop over stocks
  for (i in 1:NI) {
    
    print(i)
    
    # Object to export
    store.tbl.tmp = array(NA,dim=c(length(losses),1,length(nms)-2))
    
    # Select a dataset
    dt = out[[i]]
    
    # Loop over losses
    for (ls in losses){
      print(ls)
      store.tbl.tmp<-loss_eval(store.tbl.tmp=store.tbl.tmp,dt=dt,ext.rem=ext.rem,
                               perc=perc,quan=quan,selection=selection,
                               model_dict=model_dict,calibri.loss=ls,nms=nms,depnum=depnum)
    }
    store.tbl[,,,i] = store.tbl.tmp
    
  }
  
  return(store.tbl)
}


############################ Out-of-sample results / sector Table ############################

table_sectors<-function(link,out,ext.rem,loss=c('MSE','QLIKE','MAE','MAPE'),selection, model_dict, nms){
  
  # Data on Sectors from wikipedia
  sectors<-sectors_fun(link)[[1]]
  sector.dict<-sectors_fun(link)[[2]]
  
  # Use function get.store to prepare preliminary version of the table
  store.tbl.ave <- get.store(out=out, ext.rem, loss=loss, selection=selection, model_dict=model_dict, nms=nms)
  
  results<-list()
  
  # Number of models
  NM = length(nms)-2
  # Number of stocks
  NI = dim(store.tbl.ave)[3]
  
  tbl <- data.frame(matrix(ncol=length(nms)-2+1,nrow=length(sectors)+1,dimnames=list(c(sectors, "all"),c(nms[-c(1,2)],"NI"))))
  tbl.imp <- tbl
  
  # Loop over loss functions selected
  for (l in 1:length(loss)){
    
    print(loss[l])
    loss.ind=which(dimnames(store.tbl.ave)[[1]] == loss[l])
    
    #### OUTPERFORMING & AVERAGE IMPROVEMENT
    # Loop over models
    for (m in 1:NM) {
      print(dimnames(store.tbl.ave)[[2]][m])
      
      # Loop over sectors
      for (s in 1:length(sectors)){
        
        # Select stock, that belong to this sector
        stock.selector=which(dimnames(store.tbl.ave)[[3]] %in% sector.dict[sector.dict$`GICS Sector` == sectors[s],1][[1]])
        
        # OVERPERFORM
        diff=store.tbl.ave[loss.ind,1,stock.selector]-store.tbl.ave[loss.ind,m,stock.selector]>0
        diff <- diff[!is.na(diff)]
        tbl[s,m] <- sum(diff)/length(diff)*100
        # add number of stocks
        tbl[s,ncol(tbl)]<-length(diff)
        
        
        # AVERAGE IMPROVEMENT
        imp = (store.tbl.ave[loss.ind,1,stock.selector]-store.tbl.ave[loss.ind,m,stock.selector])/store.tbl.ave[loss.ind,1,stock.selector]
        imp <- imp[!is.na(imp)]
        tbl.imp[s,m] <- mean(100*imp) 
        # add number of stocks
        tbl.imp[s,ncol(tbl)]<-length(imp)
      }
      
      # add results for all stocks
      diff=store.tbl.ave[loss.ind,1,]-store.tbl.ave[loss.ind,m,]>0
      diff <- diff[!is.na(diff)]
      tbl[nrow(tbl),m] <- sum(diff)/length(diff)*100
      tbl[nrow(tbl),ncol(tbl)]<-length(diff)
      
      # add results for all stocks
      imp = (store.tbl.ave[loss.ind,1,]-store.tbl.ave[loss.ind,m,])/store.tbl.ave[loss.ind,1,]
      imp <- imp[!is.na(imp)]
      tbl.imp[nrow(tbl.imp),m] <- mean(100*imp) 
      tbl.imp[nrow(tbl.imp),ncol(tbl.imp)]<-length(imp)
    }
    # Save
    results[[paste(loss[l], "overperform", sep=".")]] <- tbl
    results[[paste(loss[l], "ave","imp", sep=".")]] <- tbl.imp
    
    
    #### RANKING
    
    # Prepare tables
    # Ranks for all stock for all models
    all.ranks <- data.frame(matrix(ncol=length(nms)-2,nrow=NI,dimnames=list(dimnames(store.tbl.ave)[[3]],nms[-c(1,2)])))
    for (i in 1:NI) all.ranks[i,] = rank(store.tbl.ave[loss.ind,,i])
    # Now take average for individual sectors
    tbl.rank <- data.frame(matrix(ncol=length(nms)-2+1,nrow=length(sectors)+1,dimnames=list(c(sectors, "all"),c(nms[-c(1,2)],"NI"))))
    
    # Loop over sectors
    for (s in 1:length(sectors)){
      stock.selector=which(dimnames(store.tbl.ave)[[3]] %in% sector.dict[sector.dict$`GICS Sector` == sectors[s],1][[1]])
      tbl.rank[s,-ncol(tbl.rank)] <- colMeans(all.ranks[stock.selector,])
      # add number of stocks
      tbl.rank[s,ncol(tbl.rank)]<-length(stock.selector)
    }
    # add results for all stocks
    tbl.rank[nrow(tbl.rank),ncol(tbl.rank)]<-nrow(all.ranks)
    tbl.rank[nrow(tbl.rank),-ncol(tbl.rank)] <- colMeans(all.ranks)
    
    
    # Save
    results[[paste(loss[l],"ave","rank", sep=".")]] <- tbl.rank
  }
  
  return(results)
}

# Helper function for table_sectors. Loads a table form wikipedia.
sectors_fun<-function(link="https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"){
  # Load and modify list of sectors for S&P 500 stocks
  # Reading in the table from Wikipedia
  page = read_html(link)
  # Obtain the piece of the web page that corresponds to the "wikitable" node
  my.table = html_node(page, ".wikitable")
  # Convert the html table element into a data frame
  my.table = html_table(my.table, fill = TRUE)
  
  # Change some stocks
  my.table$Symbol[which(my.table$Symbol=="ELV")]<-"ANTM"
  my.table$Symbol[which(my.table$Symbol=="BALL")]<-"BLL"
  my.table<-rbind(my.table, c("CERN", "Cerner","reports", "Health Care", "Health Care Technology",
                              "North Kansas City, Missouri","2010-04-30","0000804753","1979"),
                  c("IPGP", "IPG Photonics","reports", "Information Technology", "Electronic Manufacturing Services",
                    "Oxford, Massachusetts","2018-03-07","0001111928","1990"),
                  c("UAA", "Under Armour","reports", "Consumer Discretionary", "Apparel, Accessories & Luxury Goods",
                    "Baltimore, Maryland","2014-05-01","0001336917","1996"))
  
  # Select only stocks that we have 
  sub.table = my.table[which(my.table$Symbol %in% names(results)),]
  
  # Find unique sectors
  sectors = unique(sub.table$`GICS Sector`)
  # Make a "dictionary" of stocks and sectors
  sector.dict<-sub.table[,c(1,4)]
  
  results<-list(sectors,sector.dict)
  
  return(results)
}


# Helper function for table_sectors 
get.store <- function(out, ext.rem, loss=c('MSE','QLIKE','MAE','MAPE'), selection, model_dict, nms){ # type full or average
  
  # Number of observations
  TT=dim(out[[1]][["att"]])[1]
  # Number of stocks
  NI = length(out)
  
  
  # Prepare files/objects to store results 
  # In store.tbl.ave we want to store average values for loss functions
  # This will be useful for building tables 
  store.tbl.ave = array(NA,dim=c(length(loss),length(nms)-2,NI))
  dimnames(store.tbl.ave)[[1]] = loss                        # loss functions
  dimnames(store.tbl.ave)[[2]] = nms[-c(1,2)]                # models
  dimnames(store.tbl.ave)[[3]] = names(out)                  # stocks
  
  # Loop over stocks
  for (i in 1:NI) {
    
    # Select a dataset
    dt = out[[i]]
    
    # Loop over loss functions selected
    for (l in 1:length(loss)){
      
      calibri.loss=loss[l]
      
      # PREPARE A LOSS DATASET
      temp.calibri.loss = calibri.loss
      if (calibri.loss=="MAE") temp.calibri.loss="MSE"
      if (calibri.loss=="MAPE") temp.calibri.loss="MSE"
      if (calibri.loss=="QLIKE") temp.calibri.loss="MSE"
      dt.loss <- loss.dt(dt=dt, selection=selection, model_dict=model_dict, calibri.loss=temp.calibri.loss, nms)
      
      # REMOVE ext.rem MAX RV OBSERVATIONS
      dt.loss = dt.loss[-tail(order(dt.loss$rv),n=ext.rem),]
      
      print(calibri.loss)
      loss.ind=which(dimnames(store.tbl.ave)[[1]] == calibri.loss)
      
      # QLIKE
      if (calibri.loss=="QLIKE"){dt.loss$rv[dt.loss$rv==0] = 1}
      
      # POSITION OF MODELS
      idx.mod = c(1:dim(dt.loss)[2])[-which(names(dt.loss) %in% c('Date','rv'))]
      
      # NUMBER OF MODELS
      NM = length(idx.mod)
      
      # AVERAGES
      # LOOP OVER MODELS
      for (j in 1:NM) {
        if (calibri.loss == "MSE"){
          store.tbl.ave[loss.ind,j,i]<-mean((dt.loss$rv-dt.loss[,idx.mod[j]])^2,na.rm=T)
        }
        if (calibri.loss == "QLIKE"){
          store.tbl.ave[loss.ind,j,i]<-mean(dt.loss$rv/dt.loss[,idx.mod[j]] - log(dt.loss$rv/dt.loss[,idx.mod[j]]) - 1,na.rm=T)
        }
        if (calibri.loss =="MAE"){
          store.tbl.ave[loss.ind,j,i]<--mean(abs(dt.loss$rv-dt.loss[,idx.mod[j]]),na.rm=T)
        }
        if (calibri.loss == "MAPE"){
          store.tbl.ave[loss.ind,j,i]<--mean(abs((dt.loss$rv-dt.loss[,idx.mod[j]])/dt.loss$rv),na.rm=T)
        }
      }
      
    }
    
  }
  
  return(store.tbl.ave)
}



############################ Variable Importance from CSR Table ############################



# Function to extract variable importance from results for one subset of models, given by parameter select
table_vimp_avg = function(out, select, loss="MSE") {
  
  # Number of stocks
  NI = length(out)
  
  # Loss function used in calibration
  if (loss=="MSE") l=1
  if (loss=="QLIKE") l=2
  
  # Table
  tbl = data.frame(variables=dimnames(out[[1]][[which(names(out[[1]]) == select)]])[[1]],
                       AVE=0)
  
  tbl.tmp = array(0,dim=c(dim(tbl)[1],NI))
  
  # Loop over stocks
  for (i in 1:NI) {
    
    # Select the stock
    dt = out[[i]]
    
    # Extract for all variables, the ratio of the variable importance = ratio of occurance in models
    subs = dt[[which(names(dt) == select)]][,"Ratio","Memory 0.95",l,"Group 5","Kmeans",]
    tbl.tmp[,i] = apply(subs,1,mean)
    
  }
  
  tbl[,"AVE"] = apply(tbl.tmp,1,mean)
  
  return(tbl)
}

table_vimp <- function(selects, nams){
  
  # Loop over each select in selects and run the table_vimp_avg function
  vimp_tables <- list()
  for (select in selects) {
    vimp_tables[[select]] <- table_vimp_avg(out=results, select=select, loss = "MSE")
  }
  
  # Assign to one dataframe, except for the first three rows 
  vimp_table <- do.call(rbind, lapply(vimp_tables, function(x) x[-(1:3), ]))
  
  # Add Source, Type of variable and Event from the nams dataframe to the vimp_table
  vimp_table <- merge(x=vimp_table, y=nams, by.x = "variables", by.y = "Var", all.x = TRUE)
  
  # Paste together columns Source and Type.of.variable into a new column Source.Type
  vimp_table$Source.Type <- paste(vimp_table$Source, vimp_table$Type.of.variable, sep = " - ")
  
  
  # Empty dataframe to store the final table with rownames equal to unique values of Event from nams and colnames equal to unique values of Source.Type
  final_table <- data.frame(matrix(ncol = length(unique(vimp_table$Source.Type)), nrow = length(unique(vimp_table$Event))))
  colnames(final_table) <- unique(vimp_table$Source.Type)
  rownames(final_table) <- unique(vimp_table$Event)
  
  # Populate the final_table with values from vimp_table$AVE
  for (event in unique(vimp_table$Event)) {
    for (source_type in unique(vimp_table$Source.Type)) {
      # Get the value from vimp_table where Event and Source.Type match
      value <- vimp_table$AVE[vimp_table$Event == event & vimp_table$Source.Type == source_type]
      # If value is not empty, assign it to the final_table
      if (length(value) > 0) {
        final_table[event, source_type] <- value
      }
    }
  }
  
  final_table$`gt - event volume`[nrow(final_table)]<-final_table$`gt - general volume`[nrow(final_table)] 
  final_table$`gt - general volume`<-NULL 
  # Do the same with wiki
  final_table$`wiki - event volume`[nrow(final_table)]<-final_table$`wiki - general volume`[nrow(final_table)]
  final_table$`wiki - general volume`<-NULL
  # Reorder columns
  final_table <- final_table[,c("bloom - #ests","gt - event volume", "wiki - event volume",
                                "tw - volume of tweets", "pq - volume of articles",
                                "tw - positive emotions", "pq - positive emotions",
                                "tw - negative emotions", "pq - negative emotions")]
  
  header = c(rep("Attention",5), rep("Positive sentiment",2), rep("Negative sentiment",2))
  subheader = c("Bloomberg","GT","Wikipedia","Twitter","Newspapers","Twitter","Newspapers","Twitter","Newspapers")
  
  final_table=rbind(subheader, final_table)
  colnames(final_table) <- header
  
  return(final_table)
}



ws_run<-function(proxy_model_num,store.tbl,nms,calibri.loss="MSE"){
  
  if (calibri.loss == "MSE") cl=1
  if (calibri.loss == "QLIKE") cl=2
  if (calibri.loss == "MAE") cl=3
  if (calibri.loss == "MAPE") cl=4
  
  NM = dim(store.tbl)[3]
  
  dt=t(store.tbl[cl,1,,])
  w.res.dt=data.frame(matrix(NA,nrow=1,ncol=NM,dimnames=list(NULL,nms[-c(1,2)])))
  
  # Loop over models except the proxy model
  for (m in c(1:ncol(dt))[-proxy_model_num]){
    
    # Run Wilcoxon signed-rank test
    w.res.dt[1,m]<-wilcox.test(x=dt[,proxy_model_num],y=dt[,m],paired=T, alternative = "greater")$p.value
  }
  
  W_MSE<-round(p.adjust(as.numeric(w.res.dt[1,]), method = 'holm' ),10)
  w.res.dt[1,]<-as.numeric(W_MSE)
  return(w.res.dt)
}


############################ MCS test Tables ############################

# Function to aggregate pairwise MCS test results
process_mcs_pair <- function(mcs_alpha, days, vers, depnum, current_wd, mcs_wd = "./mcs", results_wd = "./results") {
  # DEFINE HELPER FUNCTIONS
  
  # How many times was the column model better (according to MCS) == rank 1 or only one left in SSM
  summcs <- function(x) sum(x < 2, na.rm = TRUE) / length(x)
  
  # How many times was the column model the only one left in SSM
  sumhard <- function(x) sum(x == 0, na.rm = TRUE) / length(x)
  
  # How many times was the column model in SSM
  sumssm <- function(x) 1 - (sum(is.na(x)) / length(x))
  
  found_something <- FALSE
  setwd(mcs_wd)
  
  possibleError <- tryCatch(
    readRDS(paste("mcs", mcs_alpha, days, vers, paste0("H", depnum), "pair", sep = "_")),
    error = function(e) e
  )
  
  # If there was not an error, switch found_something to TRUE and process loaded data
  if (!inherits(possibleError, "error")) {
    # REAL WORK
    print(paste("mcs", mcs_alpha, days, vers, paste0("H", depnum), "pair", sep = "_"))
    mcs.all <- possibleError
    rm(possibleError); gc()
    
    if (!found_something) {
      found_something <- TRUE
      print("found")
      print(found_something)
      
      # PREPARE EMPTY LIST
      tables <- list()
      
      # Loop over loss functions
      for (l in 1:dim(mcs.all)[1]) {
        # Loop over loss/rank
        for (d in 1:dim(mcs.all)[2]) {
          
          # PREPARE EMPTY FILES
          # How many times was the column model better (according to MCS) == rank 1 or only one left in SSM
          mcs_better <- data.frame(matrix(NA, nrow = dim(mcs.all)[3], ncol = dim(mcs.all)[3],
                                          dimnames = list(dimnames(mcs.all)[[3]], dimnames(mcs.all)[[3]])))
          # How many times was the column model the only one left in SSM
          mcs_only <- mcs_better
          # How many times was the column model in SSM
          mcs_ssm <- mcs_better
          
          # Loop over models
          for (k in 1:dim(mcs.all)[3]) {
            mcs_better[k, ] <- apply(mcs.all[l, d, k, , ], 1, summcs)
            mcs_only[k, ]   <- apply(mcs.all[l, d, k, , ], 1, sumhard)
            mcs_ssm[k, ]    <- apply(mcs.all[l, d, k, , ], 1, sumssm)
          }
          
          # Save to list
          nam <- paste(dimnames(mcs.all)[[2]][d], dimnames(mcs.all)[[1]][l], sep = ".")
          tables[[paste(nam, "better", sep = "_")]] <- mcs_better
          tables[[paste(nam, "only", sep = "_")]] <- mcs_only
          tables[[paste(nam, "ssm", sep = "_")]] <- mcs_ssm
        }
      }
      
      print("table successfully constructed")
      
      # Save list of aggregated tables
      setwd(current_wd)
      savetable(tablefile = tables, wd= results_wd, tblname = paste("mcs", "pairs", days, mcs_alpha, sep = "_"),
                vers = vers, depnum = depnum)
      print("table saved")
      
      return(tables)
    }
  } else {
    print("not found")
    print("table was not constructed")
    setwd(current_wd)
    return(NULL)
  }
}



# Function to aggregate multiple model MCS test results
process_mcs <- function(mcs_alpha, days, vers, depnum, current_wd, mcs_wd = "./mcs", results_wd = "./results") {
  # LOAD MCS RESULTS
  setwd(mcs_wd)
  found_something <- FALSE
  
  possibleError <- tryCatch(
    readRDS(paste("mcs", mcs_alpha, days, vers, paste0("H", depnum), sep = "_")),
    error = function(e) e
  )
  if (!inherits(possibleError, "error")) {
    # REAL WORK
    print(paste("mcs", mcs_alpha, days, vers, paste0("H", depnum), sep = "_"))
    mcs.all <- possibleError
    rm(possibleError); gc()
    
    if (found_something == FALSE) {
      # PREPARE EMPTY FILES
      nams <- NULL
      for (d in 1:2) nams <- c(nams, paste(dimnames(mcs.all)[[2]][d], mcs_alpha, sep = "."))
      mcs.mse   <- data.frame(matrix(ncol = dim(mcs.all)[4], nrow = length(nams), dimnames = list(nams, dimnames(mcs.all)[4][[1]])))
      mcs.qlike <- mcs.mse
      mcs.mae   <- mcs.mse
      mcs.mape  <- mcs.mse
      found_something <- TRUE # switch to TRUE
    }
    
    # REPLACE NAS WITH ZEROS
    for (l in 1:4) for (d in 1:2) for (h in 1:dim(mcs.all)[4]) mcs.all[l, d, 3, h, is.na(mcs.all[l, d, 3, h, ])] <- 0
    
    # LOOP TO AGGREGATE
    for (d in 1:2) {
      mcs.mse[paste(dimnames(mcs.all)[[2]][d], mcs_alpha, sep = "."), ]   <- apply(mcs.all[1, d, 3, , ], 1, mean, na.rm = TRUE)
      mcs.qlike[paste(dimnames(mcs.all)[[2]][d], mcs_alpha, sep = "."), ] <- apply(mcs.all[2, d, 3, , ], 1, mean, na.rm = TRUE)
      mcs.mae[paste(dimnames(mcs.all)[[2]][d], mcs_alpha, sep = "."), ]   <- apply(mcs.all[3, d, 3, , ], 1, mean, na.rm = TRUE)
      mcs.mape[paste(dimnames(mcs.all)[[2]][d], mcs_alpha, sep = "."), ]  <- apply(mcs.all[4, d, 3, , ], 1, mean, na.rm = TRUE)
    }
    
    print("found")
  } else {
    print("not found")
  }
  
  print(found_something)
  if (found_something == TRUE) {
    print("table sucessully constructed")
    mcs.mse   <- mcs.mse * 100
    mcs.qlike <- mcs.qlike * 100
    mcs.mae   <- mcs.mae * 100
    mcs.mape  <- mcs.mape * 100
    mcs.tbls  <- list()
    mcs.tbls[["mse"]]   <- mcs.mse
    mcs.tbls[["qlike"]] <- mcs.qlike
    mcs.tbls[["mae"]]   <- mcs.mae
    mcs.tbls[["mape"]]  <- mcs.mape
    
    # Save list of aggregated tables
    setwd(current_wd)
    savetable(tablefile = mcs.tbls, wd = results_wd, tblname = paste("mcs", "tbl", days, sep = "_"), vers = vers, depnum = depnum)
    print("table saved")
    return(mcs.tbls)
  } else {
    print("not found")
    print("table was not constructed")
    setwd(current_wd)
    return(NULL)
  }
}

