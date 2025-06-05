# HELPER FUNCTIONS FOR SCRIPT "model_flow.R"

# Load necessary libraries
if (!require("pacman")) install.packages("pacman")
# Check whether these libraries are installed, install if not and load all of them
pacman::p_load(zoo,foreach,doParallel,speedglm,useful,cluster,glmnetUtils,ranger,readr)


# Save working directory
my_wd <- getwd()


# Function that transforms the attention/sentiment variables
# We only use the log transformation in the paper
# Inputs are:
# totrans - vector of variables to transform
# formats - vector of transformations to apply
# K - number of observations for moving average
# addto - TRUE if we want to add the transformed variables to the original dataset
transform = function(DR,totrans=att,formats=c('None','Log','MA','ASVI','SQRT'),K=5,addto=TRUE) {
  
  if (formats=='None') return(DR)
  
  # Number of observations
  TT = dim(DR)[1]
  
  # Number of transformations
  NS = length(formats)
  
  # Number of variables to transform
  NT = length(totrans)
  
  # Conditional new vector of variables to use
  if (addto) new.att = totrans
  
  # Store dataset
  results = list()
  
  # Loop over transformations
  for (i in 1:NS) {
    
    # New data
    new.data = matrix(NA,nrow=TT,ncol=NT)
    nms = paste(totrans,'_',formats[i],sep='')
    colnames(new.data) = nms
    
    for (v in 1:NT) {
      
      x = DR[,totrans[v]]
      
      # if (formats[i] == 'Log')  new.data[,v] = log(x+1)
      if (formats[i] == 'Log'){
        # Are there any negative values?
        if (sum(x<0,na.rm=T) == 0) {
          
          # If there is 0 -> add 1, else keep ordinary log
          if (sum(x==0,na.rm=T) > 0) new.data[,v] = log(1+x) else new.data[,v] = log(x)
          
          # In case of negative values  
        } else {
          
          # We need to make sure that signs are not forgotten :-)
          neg.ind=which(x < 0)
          pos.ind=which(x > 0)
          zero.ind=which(x == 0)
          
          x.neg=-1 * log(abs(x[neg.ind]))
          x.pos=log(x[pos.ind])
          x.zero=log(1+x[zero.ind])
          
          x[neg.ind]=x.neg
          x[pos.ind]=x.pos
          x[zero.ind]=x.zero
          new.data[,v] = x
          
        }
      }
      
      if (formats[i] == 'MA')   new.data[,v] = c(rep(NA,K-1),rollmean(x,k=K,align='right'))
      if (formats[i] == 'ASVI') new.data[,v] = c(rep(NA,K-1),log(x[K:TT]+1)-log(x[1:(TT-K+1)]+1))
      if (formats[i] == 'SQRT') new.data[,v] = sqrt(x)
    }
    
    if (addto) {
      # Variable names
      new.att = c(new.att,nms)
      # Add the dataset to DR
      DR = data.frame(DR,new.data)
    } else {
      results[[i]] = new.data
    }
    
  }
  
  if (!addto) {
    indv<-c()
    for (var in totrans){
      indv<-c(indv, which(names(DR) == var))
    }
    
    if (!identical(names(DR)[indv],totrans)) print("incorrectly selected indices to store transformed data")
    
    DR[,indv] = results[[NS]]
    return(DR)
  }
  
  if (addto) {
    results = list()
    results[['att']] = new.att
    results[['DR']]  = DR
    return(results)
  }
  
}


# Function that creates specifications with a given complexity for CSR models
# Inputs are:
# dep - dependent variable (name)
# fixing - fixed independent variables (name)
# indep - free independent variables (name)
# cx - complexity level = maximum number of free independent variables
specs.create = function(dep='V.H1.Log',fixing=c('V.L1.Log','V.L5.Log','V.L22.Log'),indep=att,cx=4) {
  
  # Number of fixed independent variables
  NXI = length(fixing)
  
  # Number of free independent variables
  NFI = length(indep)
  
  # Adjusted complexity
  cxa = cx - NXI
  if (cxa <= 0) return('Too many fixed independent variables or increase complexity level')
  
  # List of specifications
  specs = list()
  
  # Baseline spec
  base = paste(dep,'~1',sep='')
  if (NXI>0) for (i in 1:NXI) base = paste(base,'+',fixing[i],sep='')
  
  # Matrix of variable combinations given by numbers/positions -> columns is cxa
  select = t(combn(x = NFI, m = cxa))
  # Number of models
  R = dim(select)[1]
  
  # Loop over possible models and create spec
  k = 1
  for (r in 1:R) {
    tmp = base
    for (i in 1:cxa) tmp = paste(tmp,'+',indep[select[r,i]],sep='')
    specs[[k]] = as.formula(tmp)
    k = k + 1
  }
  
  return(specs)
  
}


# Function estimates a model using one of the types of estimators
# Function makes a prediction - if LogTrans=TRUE, converts back to variances using one of the transformation methods
# Inputs are:
# estim - data for estimation
# predi - data for prediction
# estim.type - type of estimator
# LogTrans - TRUE if we use log transformed dependent variable
# model.estim - model to be estimated
# y.max - maximum value of the dependent variable in the estimation window
# y.min - minimum value of the dependent variable in the estimation window
# orderapprox - order of approximation
estimate = function(estim,predi,estim.type,LogTrans=TRUE,model.estim=specs[[1]],y.max,y.min,orderapprox=12) {
  
  # Select dataset
  estim = estim[,all.vars(model.estim)]
  estim = estim[complete.cases(estim),]
  
  # Check if there are any observations, otherwise return NA
  if (dim(estim)[1] == 0) return(NA)
  
  # SETUP DEP and INDEP VARIABLES
  y = as.matrix(estim[,all.vars(model.estim)[1]])
  X = as.matrix(cbind(rep(1,dim(estim)[1]),estim[,all.vars(model.estim)[-1]]))
  
  # ESTIMATE MODEL
  if (estim.type=='MSE')  {
    # Estimate
    pm = coefficients(speedlm.fit(y=y,X=X))
    # Residuals-squared
    res = (y - X %*% cbind(pm))
  }
  
  if (estim.type == 'WLS1') {
    # Setup weights
    y[y==0] = min(y!=0)
    wls1 = as.numeric(1/y)
    pm = coefficients(speedglm::speedlm.wfit(y=y,X=X,w=wls1))
    # Residuals-squared
    res = (y - X %*% cbind(pm))
  }

  if (estim.type == 'WLS3') {
    # Number of observations in the estimation window
    TE = dim(y)[1]
    # Setup weights
    wls3 = (-log(1 - (1:(TE-1))/TE))/(TE-1)
    wls3[TE] = log(TE)/(TE-1)
    wls3 = wls3/sum(wls3)
    # Estimate model with weights
    pm = coefficients(speedglm::speedlm.wfit(y=y,X=X,w=wls3))
    # Residuals-squared
    res = (y - X %*% cbind(pm))
  }
  
  # PREDICT 
  XP = as.matrix(cbind(rep(1,dim(predi)[1]),predi[,all.vars(model.estim)[-1]]))
  pred = XP %*% cbind(pm)
  
  if (is.na(pred[1])) {
    
    return(NA) 
    
  } else {
    
    # TRANSFORM
    if (LogTrans) {
      # Lower order approximation
      if (orderapprox< 2) pred = exp(pred)
      # Second order approximation
      if (orderapprox==2) pred = exp(pred)*(1+mean(res^2,na.rm=T)/2)
      # Higher order approximation
      if (orderapprox> 2) {
        scale = c(1,0); 
        for (k in 2:orderapprox) scale[k+1] = mean(res^k)/factorial(k)
        pred = exp(pred)*sum(scale)
      }
    } 
    
    # SANITY FILTER
    return(ifelse(pred > y.max,y.max,ifelse(pred < y.min, y.min, pred)))
    
  }
  
}



# Function that forecasts specific HAR models
# Inputs are:
# DR - dataset
# dep - dependent variable (name)
# indep - independent variables (name)
# fixing - fixed independent variables (name)
# estim.type - type of estimator
# LogTrans - TRUE if we use log transformed dependent variable
# formats - vector of transformations to apply
# K - number of observations for moving average
# addto - TRUE if we want to add the transformed variables to the original dataset
# W - number of observations in the estimation window
# nc - number of cores
# orderapprox - order of approximation
estim.bench = function(DR,dep,indep,fixing,estim.type,LogTrans,formats,K,addto,W,nc,orderapprox=12) {
  
  library(foreach)
  library(doParallel)
  library(speedglm)
  
  # Winsorize 4 highest observations
  winsor = function(x,q=4) {
    TX = length(x)
    px = sort(x)
    x[which(x %in% px[c(TX:(TX-q+1))])] = x[which(x == px[TX-q])]
    return(x)
  }
  
  # Overall number of observations
  TT = dim(DR)[1]
  
  
  # TRANSFORM ATTENTION/SENTIMENT VARIABLES
  if (!is.null(formats)){
    DD = transform(DR,totrans=indep,formats=formats,K=K,addto=addto)
    if (addto) {
      att = DD$att
      DD  = DD$DR
    }
  } else {
    DD=DR
  }
  
  # Define the formula for the model
  nms = paste(dep,'~1'); for (i in 1:length(fixing)) nms = paste(nms,'+',fixing[i],sep=''); for (i in 1:length(indep)) nms = paste(nms,'+',indep[i],sep='')
  spec.use = as.formula(nms) 
  
  A = Sys.time()
  cl <- makeCluster(nc)
  registerDoParallel(cl)
  TMP = foreach::foreach (s = (W+1):TT,.export=c('estimate'), .packages = 'speedglm') %dopar% {
    
    # Select dataset
    DT = DD[(s-W):s,]
    
    # Number of observations
    TT = dim(DT)[1]
    
    # Data for estimation purposes, TT is left for prediction purposes
    h=as.numeric(substring(strsplit(all.vars(spec.use)[1], "[.]")[[1]][2],2))
    estim = DT[1:(TT-h),]
    
    # Data for predictive purposes
    predi = DT[+TT,]
    
    # Maximum & Minimum realized RV in the estimation window - needed for sanity filter
    dep = all.vars(as.formula(spec.use))[1]
    y.max = ifelse(LogTrans,exp(max(estim[dim(estim)[1]:1,dep],na.rm=T)),max(estim[dim(estim)[1]:1,dep],na.rm=T))
    y.min = ifelse(LogTrans,exp(min(estim[dim(estim)[1]:1,dep],na.rm=T)),min(estim[dim(estim)[1]:1,dep],na.rm=T))

    # Matrix of proxy + model
    results = matrix(NA,ncol=1,nrow=2)
    rownames(results) = c('Proxy','Model')
    
    # If there are some really extreme values: go from max to lowest and winsorize max if it is 2 times the previous value
    estim[,dep] = winsor(x=estim[,dep],q=4)
    # Start by storing the proxy
    results[1,1] = ifelse(LogTrans,exp(predi[,dep]),predi[,dep])
    # Estimate the model and store the prediction
    results[2,1] = estimate(estim,predi,estim.type,LogTrans=LogTrans,model.estim=spec.use,y.max,y.min,orderapprox)
    
    return(results)
    
  }
  stopCluster(cl)
  Sys.time()-A
  
  # Create matrix of results
  outsample = matrix(NA,nrow=length(TMP),ncol=2)
  for (i in 1:length(TMP)) outsample[i,] = t(TMP[[i]])
  
  outsample = data.frame(DR$Date[(W+1):TT],outsample)
  
  return(outsample)
}



# Function that runs CSLR regressions
# Inputs are:
# DR - dataset
# specs - list of model specifications
# bench - benchmark model = a simple HAR model specification in our case
# estim.type - type of estimator
# LogTrans - TRUE if we use log transformed dependent variable
# orderapprox - order of approximation
CSLR = function(DT=DR,specs,bench,estim.type=c('MSE','WLS1','WLS3'),LogTrans=TRUE,orderapprox) {
  
  # Winsorize 4 highest observations
  winsor = function(x,q=4) {
    TX = length(x)
    px = sort(x)
    x[which(x %in% px[c(TX:(TX-q+1))])] = x[which(x == px[TX-q])]
    return(x)
  }
  
  # Estimation types:
  # MSE - standard OLS
  # WLS1 - 1/RV
  # WLS2 - 1/fitted(RV)
  # Read the package that estimates fast OLS
  library(speedglm) # it still calculates standard errors. So matrix form solution might be even faster!
  
  # Number of observations
  TT = dim(DT)[1]
  
  # Data for estimation purposes. TT is left fore prediction purposes
  h=as.numeric(substring(strsplit(all.vars(specs[[1]])[1], "[.]")[[1]][2],2))
  estim = DT[1:(TT-h),]
  # Data for predictive purposes
  predi = DT[+TT,]
  
  # Maximum & Minimum realized RV in the estimation window - needed for sanity filter
  dep = all.vars(as.formula(specs[[1]]))[1]
  y.max = ifelse(LogTrans,exp(max(estim[dim(estim)[1]:1,dep],na.rm=T)),max(estim[dim(estim)[1]:1,dep],na.rm=T))
  y.min = ifelse(LogTrans,exp(min(estim[dim(estim)[1]:1,dep],na.rm=T)),min(estim[dim(estim)[1]:1,dep],na.rm=T))
  
  # Number of models
  M = length(specs)
  
  # Matrix of proxy + HAR + M models
  results = matrix(NA,ncol=1,nrow=M+2)
  rownames(results) = c('Proxy','HAR',rep('CSLR',M))
  
  # If there are some really extreme values: go from max to lowest and winsorize max if it is 2 times the previous value
  estim[,dep] = winsor(x=estim[,dep],q=4)
  # Start with proxy and HAR
  results[1,1] = ifelse(LogTrans,exp(predi[,dep]),predi[,dep])
  results[2,1] = estimate(estim,predi,estim.type,LogTrans=LogTrans,model.estim=bench,y.max,y.min,orderapprox)
  
  ### Estimate CSLR models ###
  for (r in 1:M) results[r+2,1] = estimate(estim,predi,estim.type,LogTrans=LogTrans,
                                           model.estim=specs[[r]],y.max,y.min,orderapprox)
  
  return(results)
}



# Run CSLR forecasts - Parallelized
ROLL.CSLR = function(DR=DR,specs,bench,estim.type,LogTrans,W=1000,nc=32,orderapprox) {
  
  library(foreach)
  library(doParallel)
  library(speedglm)
  
  # Overall number of observations
  TT = dim(DR)[1]
  # Number of models
  M = length(specs)
  
  A = Sys.time()
  cl <- makeCluster(nc)
  registerDoParallel(cl)
  TMP = foreach::foreach (s = (W+1):TT,.export=c('CSLR','estimate')) %dopar% CSLR(DT=DR[(s-W):(s),],specs,bench,estim.type,LogTrans,orderapprox)
  stopCluster(cl)
  Sys.time()-A
  
  # Create matrix of results
  outsample = matrix(NA,nrow=length(TMP),ncol=M+2)
  for (i in 1:length(TMP)) outsample[i,] = t(TMP[[i]])
  return(outsample)
}



# MSE LOSS 
mse.loss = function(proxy,true) {
  results = list()
  TT = length(true)
  yl = (proxy - true)^2
  yl.L1 = c(NA,yl[1:(length(yl)-1)])
  tr.L1 = c(NA,true[1:(length(yl)-1)])
  ld = data.frame(yl = yl, yl.L1 = yl.L1, tr.L1 = tr.L1)
  results[['loss']] = ld
  results[['avel']] = mean((proxy - true)^2,na.rm=T)
  return(results)
}

# QLIKE LOSS
qli.loss = function(proxy,true) {
  results = list()
  TT = length(true)
  yl = (true/proxy - log(true/proxy) - 1)
  yl.L1 = c(NA,yl[1:(length(yl)-1)])
  tr.L1 = c(NA,true[1:(length(yl)-1)])
  ld = data.frame(yl = yl, yl.L1 = yl.L1, tr.L1 = tr.L1)
  results[['loss']] = ld
  results[['avel']] = mean(true/proxy - log(true/proxy) - 1,na.rm=T)
  return(results)
}



# Conditional - hyperparameter tunned CSLR forecasts
# Inputs are:
# DR - dataset
# pred - predictions from the models estimated in the previous step (in ROLL.CSLR)
# W - number of observations in the estimation window
# CS - number of observations in the calibration sample
# loss - loss function
# dep - dependent variable (name)
# LogTrans - TRUE if we use log transformed dependent variable
# delta - memory parameter
# trim - parameter for the trimmed mean
# ngroup - number of groups in K-means clustering
COMBINE.CSLR = function(DR = DR, pred = pred, W=W, CS=CS, loss='MSE', dep = dep, LogTrans=LogTrans,
                        delta = delta, trim = trim, ngroup = ngroup, nc=32, specs) {
  library(foreach)
  library(doParallel)
  library(useful)
  library(cluster)
  
  # 
  mse.loss.delta = function(proxy,true,mem) {
    out = tail(order(true),n=3)
    proxy = proxy[-out]
    true  = true[-out]
    TT = length(true)
    return(weighted.mean((proxy - true)^2,w=c(mem^(TT:2),1),na.rm=T))
  }
  
  qli.loss.delta = function(proxy,true,mem) {
    out = tail(order(true),n=3)
    if (length(which(true==0))>0) out = c(out,which(true==0)) 
    proxy = proxy[-out]
    true  = true[-out]
    TT = length(true)
    return(weighted.mean((true/proxy - log(true/proxy) - 1),w=c(mem^(TT:2),1),na.rm=T))
  }
  
  # Combine DR and datasets with predictions
  colnames(pred) = c('Proxy','Bench',paste('m.',1:(dim(pred)[2]-2),sep=''))
  DRN = data.frame(DR[(W+1):dim(DR)[1],],pred)
  
  # Number of forecasts
  TF = dim(pred)[1]
  # Number of models
  M  = dim(pred)[2] - 1 # =1 that is the bench. However, now bench is also among all models.
  # Number of groups in K-Means algorithm
  NG = length(ngroup)
  # If number of cluster is more than distinct data points
  idx = which(ngroup > M)
  if (length(idx) > 0) ngroup[idx] = M
  # Number of memory parameters
  ME = length(delta)
  # Number of trim parameters
  NT = length(trim)
  # Number of loss functions
  LF = length(loss)
  
  # Predictions
  if(LogTrans) y = exp(DRN[(CS+1):dim(DRN)[1],dep]) else y = DRN[(CS+1):dim(DRN)[1],dep]
  predict = data.frame(Date = DRN$Date[(CS+1):dim(DRN)[1]], true = y, bench=NA, cslr=NA)
  
  for (s in (CS+1):TF) predict[s-CS,c('bench','cslr')] = c(pred[s,2],mean(pred[s,-c(1,2)],trim=trim))
  fx = matrix(NA,nrow=dim(predict)[1],ncol=ME*LF)
  nms = c(); for (m in 1:ME) for (l in 1:LF) nms = c(nms,paste('FX_',delta[m],'_',loss[l],sep=''))
  colnames(fx) = nms
  
  km = matrix(nrow=dim(predict)[1],ncol=ME*NG*LF)
  nms = c(); for (g in 1:NG) for (m in 1:ME) for (l in 1:LF) nms = c(nms,paste('KM_',delta[m],'_','GR',ngroup[g],'_',loss[l],sep=''))
  colnames(km) = nms
  
  kd = km
  nms = c(); for (g in 1:NG) for (m in 1:ME) for (l in 1:LF) nms = c(nms,paste('KD_',delta[m],'_','GR',ngroup[g],'_',loss[l],sep=''))
  colnames(kd) = nms  
  
  predict = data.frame(predict,fx,km,kd)

  # Which variables are potentially used?
  var.nms = c()
  for (i in 1:length(specs)) var.nms = c(var.nms,all.vars(specs[[i]]))
  var.nms = unique(var.nms)
  imp.var = array(NA,dim=c(length(var.nms)-1,3,ME,LF,NG,2,dim(predict)[1]))
  dimnames(imp.var)[[1]] = var.nms[-1]
  dimnames(imp.var)[[2]] = c('No.Models','Count','Ratio')
  dimnames(imp.var)[[3]] = paste('Memory',delta)
  dimnames(imp.var)[[4]] = paste('Loss',loss)
  dimnames(imp.var)[[5]] = paste('Group',ngroup)
  dimnames(imp.var)[[6]] = c('Kmeans','Kmedoid')
  dimnames(imp.var)[[7]] = predict$Date
    
  # FIRST  - Find average losses for all delta parameters
  # SECOND - For each memory run k-means, identify group with lowest ranking and apply a trimmed average to produce the next period forecast
  
  
  # Loop over time
  TMP = list()
  for (s in (CS+1):TF) {

    cslr.fixed  = matrix(NA,nrow=ME,ncol=LF)
    rownames(cslr.fixed) = paste('delta_',delta ,sep='')
    colnames(cslr.fixed) = paste(loss ,sep='')
    cslr.kmeans = array(NA,dim=c(ME,NG,LF))
    dimnames(cslr.kmeans)[[1]] = list(paste('delta_',delta ,sep=''))
    dimnames(cslr.kmeans)[[2]] = c(paste('groups_',ngroup,sep=''))
    dimnames(cslr.kmeans)[[3]] = paste(loss ,sep='')
    cslr.kmedoi = cslr.kmeans
    imp.var.tmp = array(0,dim=c(length(var.nms)-1,3,ME,LF,NG,2,1))
    dimnames(imp.var.tmp)[[1]] = var.nms[-1]
    
    true = pred[(s-CS):(s-1),1]
    # Loop over memory parameters
    for (r in 1:ME) {
      
      # Loop over loss functions
      for (l in 1:LF) {
        
        # Loop over models
        loss.tmp = matrix(NA,nrow=M,ncol=1)
        # If loss function is MSE, use the mse.loss.delta function, otherwise use qli.loss.delta
        for (a in 1:M) loss.tmp[a,1] = ifelse(loss[l]=='MSE',mse.loss.delta(true=true,proxy=pred[(s-CS):(s-1),a+1],mem=delta[r]),qli.loss.delta(true=true,proxy=pred[(s-CS):(s-1),a+1],mem=delta[r]))
        
        # Now perform clustering
        for (g in 1:NG) {
          
          #####################################################################################
          # K-MEANS CLUSTERING
          #####################################################################################        
          clusters.tmp = kmeans(loss.tmp,centers=ngroup[g], iter.max = 25, nstart = 50)$cluster
          # Find cluster with lowest error
          loss.ave.group = c(); for (h in 1:ngroup[g]) loss.ave.group[h] = mean(loss.tmp[clusters.tmp==h])
          
          # Which models correspond to n-best performing clusters
          if (ngroup[g] <= 5)                   opt.models = which(clusters.tmp %in% head(order(loss.ave.group),n=1))
          if (ngroup[g] >  5 & ngroup[g] <= 10) opt.models = which(clusters.tmp %in% head(order(loss.ave.group),n=3))
          if (ngroup[g] > 10)                   opt.models = which(clusters.tmp %in% head(order(loss.ave.group),n=5))
          
          # Select optimum models and find the average - trimmed - forecast
          cslr.kmeans[r,g,l] = mean(pred[s,opt.models+1],trim=trim) # + 1 because the first is the true. After that opt.models+1 = 2 belonst to HAR (benchmark)
          
          # Which variables are selected among the top models?
          if (length(opt.models) > 4) {
            opt.nms = names(which(pred[s,opt.models+1] >= quantile(pred[s,opt.models+1],p=trim) & pred[s,opt.models+1] <= quantile(pred[s,opt.models+1],p=1-trim)))
          } else {
            opt.nms = names(pred[s,opt.models+1])
          }
          if (opt.nms[1] == 'Bench') opt.nms = opt.nms[-1]
          opt.num = as.numeric(substr(opt.nms,start=3,stop=nchar(opt.nms)))
          imp.var.tmp[,1,r,l,g,1,1] = length(opt.num)
          for (m in 1:length(opt.num)) {
            var.tmp = all.vars(specs[[opt.num[m]]])
            imp.var.tmp[which(dimnames(imp.var.tmp)[[1]] %in% var.tmp),2,r,l,g,1,1] = imp.var.tmp[which(dimnames(imp.var.tmp)[[1]] %in% var.tmp),2,r,l,g,1,1] + 1
          }
          imp.var.tmp[,3,r,l,g,1,1] = 100 * (imp.var.tmp[,2,r,l,g,1,1]/imp.var.tmp[,1,r,l,g,1,1])
          
          #####################################################################################
          # K-MEDOIDS CLUSTERING
          #####################################################################################
          clusters.tmp = pam(loss.tmp,k=ngroup[g],diss=FALSE,do.swap=FALSE)$cluster
          # Find cluster with lowest error
          loss.ave.group = c(); for (h in 1:ngroup[g]) loss.ave.group[h] = mean(loss.tmp[clusters.tmp==h])
          
          # Which models correspond to n-best performing clusters
          if (ngroup[g] <= 5)                   opt.models = which(clusters.tmp %in% head(order(loss.ave.group),n=1))
          if (ngroup[g] >  5 & ngroup[g] <= 10) opt.models = which(clusters.tmp %in% head(order(loss.ave.group),n=3))
          if (ngroup[g] > 10)                   opt.models = which(clusters.tmp %in% head(order(loss.ave.group),n=5))
          
          # Select optimum models and find the average - trimmed - forecast
          cslr.kmedoi[r,g,l] = mean(pred[s,opt.models+1],trim=trim) # + 1 because the first is the true 
          
          # Which variables are selected among the top models?
          if (length(opt.models) > 4) {
            opt.nms = names(which(pred[s,opt.models+1] >= quantile(pred[s,opt.models+1],p=trim) & pred[s,opt.models+1] <= quantile(pred[s,opt.models+1],p=1-trim)))
          } else {
            opt.nms = names(pred[s,opt.models+1])
          }
          if (opt.nms[1] == 'Bench') opt.nms = opt.nms[-1]
          opt.num = as.numeric(substr(opt.nms,start=3,stop=nchar(opt.nms)))
          imp.var.tmp[,1,r,l,g,2,1] = length(opt.num)
          for (m in 1:length(opt.num)) {
            var.tmp = all.vars(specs[[opt.num[m]]])
            imp.var.tmp[which(dimnames(imp.var.tmp)[[1]] %in% var.tmp),2,r,l,g,2,1] = imp.var.tmp[which(dimnames(imp.var.tmp)[[1]] %in% var.tmp),2,r,l,g,2,1] + 1
          }
          imp.var.tmp[,3,r,l,g,2,1] = 100 * (imp.var.tmp[,2,r,l,g,2,1]/imp.var.tmp[,1,r,l,g,2,1])
          
        }

        cslr.fixed[r,l] = weighted.mean(pred[s,-1],w=loss.tmp)
        
      }
      
    }
    
  
    results            = list()
    results[['fixed']] = cslr.fixed
    results[['kmean']] = cslr.kmeans
    results[['kmedo']] = cslr.kmedoi
    results[['varim']] = imp.var.tmp
    TMP[[s-CS]] = results
  }
  
  # Predictions
  for (i in 1:length(TMP)) predict[i,5:dim(predict)[2]] = c(TMP[[i]]$fixed,
                                                            TMP[[i]]$kmean[1,1,],TMP[[i]]$kmean[1,2,],
                                                            TMP[[i]]$kmedo[1,1,],TMP[[i]]$kmedo[1,2,])
  
  # Variable importance - no coefficients
  for (i in 1:length(TMP)) imp.var[,,,,,,i] = TMP[[i]]$varim
  
  results = list()
  results[['predict']] = predict
  results[['var.imp']] = imp.var
  
  return(results)
}



# Create full specification
# Inputs are:
# dep - dependent variable (name)
# fixing - fixed independent variables (name)
# indep - free independent variables (name)
full.spec = function(dep,fixing,indep) {
  
  # Number of fixing variables
  NXI = length(fixing)

  # Baseline spec
  base = paste(dep,'~1',sep='')
  if (NXI > 0){
    for (i in 1:NXI) base = paste(base,'+',fixing[i],sep='')
  }
  
  # Number of indep variables
  NFI = length(indep)
  
  for (r in 1:NFI) base = paste(base,'+',indep[r],sep='')
  
  spec = as.formula(base)
  return(spec)
  
}

# Create LASSO, RIDGE, EN -> re-estimate every n observations
###################################################
# REGULARIZATIONS

regul = function(spec = spec, DT = DR, W = W, CS = CS, alphas = c(1.0), reestim = 1,
                 nlambda = 100, loss = c('MSE', 'QLIKE'), nc = 32, LogTrans=LogTrans, orderapprox=2, estim.type='WLS3') {
  
  # packages
  library(glmnetUtils)
  library(foreach)
  library(doParallel)
  
  # Winsorize 4 highest observations
  winsor = function(x,q=4) {
    TX = length(x)
    px = sort(x)
    x[which(x %in% px[c(TX:(TX-q+1))])] = x[which(x == px[TX-q])]
    return(x)
  }
  
  # Loss function
  mse = function(fr,true) {
    results = list()
    results[['mean']] = mean((fr-true)^2, na.rm=T)
    results[['loss']] = (fr-true)^2
    return(results) 
  }
  
  qli = function(fr,true) {
    results = list()
    results[['mean']] = mean((true/fr - log(true/fr) - 1), na.rm=T)
    results[['loss']] = (true/fr - log(true/fr) - 1)
    return(results)     
  }
  
  # Number of observations
  TT = dim(DT)[1]
  
  # Number of alpha's
  NP = length(alphas)
  
  # Number of lambas
  NL = nlambda
  
  # Number of loss functions
  LF = length(loss)
  
  # Number of forecasts
  NF = length((W+1):TT)
  
  # Predicted value
  PY = exp(DT[(W+1):TT,all.vars(spec)[1]])
  
  
  # What lambda's to use?
  LMBD <- c()
  for (a in 1:NP) {
    cv <- glmnetUtils:::glmnet.formula(formula=spec, data=DT[1:(W+CS),], alpha = alphas[a], nlambda=10)
    LMBD <- c(LMBD,cv[["lambda"]])
  }
  LMBD <- exp(seq(min(log(LMBD)),max(log(LMBD)),length.out=nlambda))
  
  
  # Object to store predictions
  fore = array(NA,dim=c(NF,NP,NL,NL))
  dimnames(fore)[[1]] = as.character(DT$Date[(W+1):TT])
  dimnames(fore)[[2]] = paste('Alpha' , alphas)
  dimnames(fore)[[3]] = paste('Lambda', LMBD)
  dimnames(fore)[[4]] = paste('Lambda1', LMBD)
  
  # Re-estimation windows
  steps = seq(from=W+1,to=TT,by=reestim)
  
  # Name of the dependent variables
  dep = all.vars(spec)[1]
  
  # Matrix of potential coefficients
  NV = length(all.vars(spec)[-1])
  cf.store = array(0,dim=c(NV+1,NP,NL,NL,NF))
  dimnames(cf.store)[[1]] = c('Intercept',all.vars(spec)[-1])
  dimnames(cf.store)[[2]] = paste('alpha.',NP,sep='')
  dimnames(cf.store)[[3]] = paste('lambda.',LMBD,sep='')
  dimnames(cf.store)[[4]] = paste('lambda1.',LMBD,sep='')
  dimnames(cf.store)[[5]] = as.character(DT$Date[(W+1):TT])
  
  # extract depnum
  h=as.numeric(substring(strsplit(as.character(spec)[2], "[.]")[[1]][2],2))
  
  print("lasso parameters set up")
  
  # For a given alpha and lambda, calculate the model on the sample - write down the in-sample squared residual - make a prediction - convert to variance
  cl <- makeCluster(nc)
  registerDoParallel(cl)
  
  for (A in 1:NP){
    print(paste("A",A))
    for (L in 1:NL){
      for (N in 1:NL) {
        TMP = foreach::foreach (r = steps) %dopar% {
          # Select a dataset where model is estimated
          Estim.DT = DT[(r-W):(r-h),all.vars(spec)]; Estim.DT = Estim.DT[complete.cases(Estim.DT),]
          
          # Select a dataset where model is predicted
          Predi.DT = DT[r:min(c((r+reestim-1),TT)),]
          
          # If there are some really extreme values: go from max to lowest and winsorize max if it is 2 times the previous value
          Estim.DT[,dep] = winsor(x=Estim.DT[,dep],q=4)
          
          # Weights
          if (estim.type=='WLS1') {
            yd = Estim.DT[,all.vars(spec)[1]]
            yd[yd==0] = min(yd[yd!=0])
            wls= 1/yd
          }
          if (estim.type=='WLS3') {
            # Number of observations in the estimation window
            TE = dim(Estim.DT)[1]
            # Setup weights
            wls = (-log(1 - (1:(TE-1))/TE))/(TE-1)
            wls[TE] = log(TE)/(TE-1)
            wls = wls/sum(wls)
          }
          
          # If the model is estimated with WLS1 or WLS3, this also has to be specified in the glmnet function
          if (estim.type=='WLS1' | estim.type=='WLS3') {
            # Adaptive LASSO
            # 1) Estimate RIDGE model
            model = glmnetUtils:::glmnet.formula(formula=spec, data=Estim.DT, alpha = 0, lambda=LMBD[L], weights = wls)
            # 2) Extract weights
            wgt <- 1/abs(coefficients(model)[-1])
            # 3) Estimate LASSO with RIDGE weights
            model = glmnetUtils:::glmnet.formula(formula=spec, data=Estim.DT, alpha = 1, lambda=LMBD[N], penalty.factor = wgt, weights = wls)
          } else {
            # Adaptive LASSO
            # 1) Estimate RIDGE model
            model = glmnetUtils:::glmnet.formula(formula=spec, data=Estim.DT, alpha = 0, lambda=LMBD[L])
            # 2) Extract weights
            wgt <- 1/abs(coefficients(model))
            # 3) Estimate LASSO with RIDGE weights
            model = glmnetUtils:::glmnet.formula(formula=spec, data=Estim.DT, alpha = 1, lambda=LMBD[N], penalty.factor = wgt)
          }
          
          # Calculate 2nd order scaling of the variance
          res = (as.numeric(predict(model, newdata = Estim.DT)) - Estim.DT[,all.vars(spec)[1]])
          
          # Model predictions
          pred = as.numeric(predict(model,newdata = Predi.DT))
          
          if (is.na(pred[1])) {
            
            return(NA) 
            
          } else {
            
            if (LogTrans) {
              # Lower order approximation
              if (orderapprox< 2) pred = exp(pred)
              # Second order approximation 
              if (orderapprox==2) pred = exp(pred)*(1+mean(res^2,na.rm=T)/2)
              # Higher order approximation
              if (orderapprox> 2) {
                scale = c(1,0); 
                for (k in 2:orderapprox) scale[k+1] = mean(res^k)/factorial(k)
                pred = exp(pred)*sum(scale)
              }
            }
          }
          
          # Find the forecast and return
          # Maximum & Minimum realized RV in the estimation window - needed for sanity filter
          y.max = ifelse(LogTrans,exp(max(Estim.DT[dim(Estim.DT)[1]:1,dep],na.rm=T)),max(Estim.DT[dim(Estim.DT)[1]:1,dep],na.rm=T))
          y.min = ifelse(LogTrans,exp(min(Estim.DT[dim(Estim.DT)[1]:1,dep],na.rm=T)),min(Estim.DT[dim(Estim.DT)[1]:1,dep],na.rm=T))
          
          if (length(which(pred>y.max))>0) pred[which(pred>y.max)] = y.max
          if (length(which(pred<y.min))>0) pred[which(pred<y.min)] = y.min
          
          cf = coefficients(model)
          
          return(list(pred,cf))
        }
        
        # Store forecasts
        fore.tmp = c(); for (i in 1:length(TMP)) fore.tmp = c(fore.tmp,TMP[[i]][[1]])
        fore[,A,L,N] = fore.tmp
        
        # Store coefficients
        for (i in 1:length(TMP)) for (r in ((i-1)*reestim+1):min(c((i*reestim),NF))) cf.store[,A,L,N,r] = as.matrix(TMP[[i]][[2]]) #which(TMP[[i]][[2]]!=0)
        
      }
    }
  }
  print("first dopar complete")
  stopCluster(cl)
  
  # For each estimation window select the preferred forecast according to a loss function
  lasso = matrix(NA,nrow=NF-CS,ncol=1+LF)
  colnames(lasso) = c('true',paste('lasso.',loss,sep=''))
  rownames(lasso) = as.character(DT$Date[(W+CS+1):TT])
  lasso[,1] = exp(DT[which(DT$Date %in% DT$Date[(W+CS+1):TT]),all.vars(spec)[1]])
  
  # CF in optimum models
  cf.optim = array(0,dim=c(NV+1,LF,NF-CS))
  dimnames(cf.optim)[[1]] = c('Intercept',all.vars(spec)[-1])
  dimnames(cf.optim)[[2]] = loss
  dimnames(cf.optim)[[3]] = as.character(DT$Date[(W+CS+1):TT])
  
  cl <- makeCluster(nc)
  registerDoParallel(cl)

  # Loop and select optimal models
  TMP = foreach::foreach (r = (CS+1):(NF-1)) %dopar% {
  
    ave.loss = array(0,dim=c(LF,NL,NL))
    dimnames(ave.loss)[[1]] = loss
    dimnames(ave.loss)[[2]] = paste('Lambda_',round(LMBD,4),sep='')
    dimnames(ave.loss)[[3]] = paste('Lambda1_',round(LMBD,4),sep='')

    # Loop over lambdas two times
    for (L in 1:NL) {
      for (N in 1:NL) {

        fr    = fore[1:(r-1),A,L,N]
        true  = PY[1:(r-1)]
        # Robustify results - remove 3 highest true values
        top.idx = head(true[order(true,decreasing = T)],n=3); top.idx = which(true %in% top.idx)
        true  = true[-top.idx]
        fr    = fr  [-top.idx]
        ave.loss[1,L,N] = mse(fr,true)$mean
        ave.loss[2,L,N] = qli(fr,true)$mean
      }
    }

    
    min.mse=which(ave.loss[1,,] == min(ave.loss[1,,], na.rm = TRUE), arr.ind = TRUE);min.mse.l=min.mse[1,1]; min.mse.n=min.mse[1,2]
    min.qli=which(ave.loss[2,,] == min(ave.loss[2,,], na.rm = TRUE), arr.ind = TRUE);min.qli.l=min.qli[1,1]; min.qli.n=min.qli[1,2]

    lasso.mse = fore[r,NP,min.mse.l,min.mse.n]
    lasso.qli = fore[r,NP,min.qli.l,min.qli.n]

    optim.mse = cf.store[,1,min.mse.l,min.mse.n,r-CS+1]
    optim.qli = cf.store[,1,min.qli.l,min.qli.n,r-CS+1]

    return(list(lasso.mse,lasso.qli,optim.mse,optim.qli))

  }
  
  print("second dopar complete")
  
  for (i in 1:length(TMP)) lasso[i,-1] = c(TMP[[i]][[1]],TMP[[i]][[2]])
  for (i in 1:length(TMP)) cf.optim[,1,i] = TMP[[i]][[3]]
  for (i in 1:length(TMP)) cf.optim[,2,i] = TMP[[i]][[4]]  
  
  # stopCluster(cl)
  
  # Combine into a single dataset
  predict = data.frame(Date=as.character(DT$Date[(W+CS+1):TT]),true=lasso[,1],lasso[,2],lasso[,3])
  names(predict) = c('Date','true','lasso.mse','lasso.qli')
  predict$Date = as.Date(predict$Date,format='%Y-%m-%d')
  
  results = list()
  results[['predict']] = predict
  results[['cf.opti']] = cf.optim
  
  return(results)
}


###################################################
# RANDOM FOREST
# Hyperparameter tuning is done in the function over num.trees, mtry and max.depth
# Remaining parameters are the same as in the rest of the functions
rf = function(spec = spec, DT = DR, W = W, CS = CS, num.trees = c(100), mtry = c(12,18), max.depth = md,
              loss = c('MSE'), reestim = 1, nc = 32,
              alwayssplit = fixing, LogTrans=LogTrans, orderapprox=2, estim.type='WLS3') {
  # Packages
  library(ranger)
  library(foreach)
  library(doParallel)
  
  # Winsorize 4 highest observations
  winsor = function(x,q=4) {
    TX = length(x)
    px = sort(x)
    x[which(x %in% px[c(TX:(TX-q+1))])] = x[which(x == px[TX-q])]
    return(x)
  }
  
  # Loss function
  mse = function(fr,true) {
    out = tail(order(true),n=3)
    if (length(which(true==0))>0) out = c(out,which(true==0)) 
    fr = fr[-out]
    true  = true[-out]
    TT = length(true)
    results = list()
    results[['mean']] = mean((fr-true)^2)
    results[['loss']] = (fr-true)^2
    return(results) 
  }
  
  qli = function(fr,true) {
    out = tail(order(true),n=3)
    if (length(which(true==0))>0) out = c(out,which(true==0)) 
    fr = fr[-out]
    true  = true[-out]
    TT = length(true)
    results = list()
    results[['mean']] = mean((true/fr - log(true/fr) - 1))
    results[['loss']] = (true/fr - log(true/fr) - 1)
    return(results)     
  }
  
  mae = function(fr,true) {
    out = tail(order(true),n=3)
    if (length(which(true==0))>0) out = c(out,which(true==0)) 
    fr = fr[-out]
    true  = true[-out]
    TT = length(true)
    results = list()
    results[['mean']] = mean(abs(fr-true))
    results[['loss']] = abs(fr-true)
    return(results) 
  }
  
  mape = function(fr,true) {
    out = tail(order(true),n=3)
    if (length(which(true==0))>0) out = c(out,which(true==0)) 
    fr = fr[-out]
    true  = true[-out]
    TT = length(true)
    results = list()
    results[['mean']] = mean(abs((fr-true)/true))
    results[['loss']] = abs((fr-true)/true)
    return(results) 
  }
  
  # Number of observations
  TT = dim(DT)[1]

  # Number of forecasts
  NF = length((W+1):TT)
  
  # Predicted value
  if (LogTrans) PY = exp(DT[(W+1):TT,all.vars(spec)[1]])-1
  if (!LogTrans) PY = DT[(W+1):TT,all.vars(spec)[1]]
  
  # Number of try
  NY = length(mtry)
  
  # Number of loss functions
  LF = length(loss)
  
  # Number of ntrees
  NR = length(num.trees)
  
  # Max depth
  NM = length(max.depth)
  
  # Object to store predictions
  fore = array(NA,dim=c(NF,NY,NR,NM))
  dimnames(fore)[[1]] = as.character(DT$Date[(W+1):TT])
  dimnames(fore)[[2]] = paste('mtry', mtry)
  dimnames(fore)[[3]] = paste('tree', num.trees)
  dimnames(fore)[[4]] = paste('depth',max.depth)

  # Re-estimation windows
  steps = seq(from=W+1,to=TT,by=reestim)
  
  # Dependent variable
  dep = all.vars(spec)[1]
  # extract depnum
  h=as.numeric(substring(strsplit(dep, "[.]")[[1]][2],2))
  print(h)
  
  # For a given mtry, ntrees and depth, calculate the model on the sample - write down the in-sample squared residual - make a prediction - convert to variance
  cl <- makeCluster(nc)
  registerDoParallel(cl)
  for (Y in 1:NY) {
    for (R in 1:NR) {
      for (M in 1:NM) {
        print(c(mtry[Y],num.trees[R],max.depth[M]))
        TMP = foreach::foreach(r = steps, .packages = 'ranger') %dopar% {
          # Select a dataset where model is estimated
          Estim.DT = DT[(r-W):(r-h),all.vars(spec)]; Estim.DT = Estim.DT[complete.cases(Estim.DT),]
          # Select a dataset where model is predicted
          Predi.DT = DT[r:min(c((r+reestim-1),TT)),]
          
          # If there are some really extreme values: go from max to lowest and winsorize max if it is 2 times the previous value
          Estim.DT[,dep] = winsor(x=Estim.DT[,dep],q=4)
          
          # Weights
          if (estim.type=='WLS1') {
            yd = Estim.DT[,all.vars(spec)[1]]
            yd[yd==0] = min(yd[yd!=0])
            wls= 1/yd
          }
          if (estim.type=='WLS3') {
            # Number of observations in the estimation window
            TE = dim(Estim.DT)[1]
            # Setup weights
            wls = (-log(1 - (1:(TE-1))/TE))/(TE-1)
            wls[TE] = log(TE)/(TE-1)
            wls = wls/sum(wls)
          }
          
          if (estim.type=='WLS1' | estim.type=='WLS3') {
            # Estimate model
            if (LogTrans) model = ranger(spec,data=Estim.DT,case.weights=wls,mtry=mtry[Y],num.trees=num.trees[R],max.depth=max.depth[M],write.forest=TRUE,min.node.size=5,num.threads=1,always.split.variables = c("V.L1.Log")) # Added the two variables directly instead of 'fixing' because of .... Error in { : task 1 failed - "object 'fixing' not found"
            # if (LogTrans) model = ranger(spec,data=Estim.DT,case.weights=wls,mtry=mtry[Y],num.trees=num.trees[R],max.depth=max.depth[M],write.forest=TRUE,min.node.size=5,always.split.variables = c("V.L1.Log","V.L5.Log")) # Added the two variables directly instead of 'fixing' because of .... Error in { : task 1 failed - "object 'fixing' not found"
            if (!LogTrans) model = ranger(spec,data=Estim.DT,case.weights=wls,mtry=mtry[Y],num.trees=num.trees[R],max.depth=max.depth[M],write.forest=TRUE,min.node.size=5,num.threads=1,always.split.variables = c("V.L1")) # Added the two variables directly instead of 'fixing' because of .... Error in { : task 1 failed - "object 'fixing' not found"
            # if (!LogTrans) model = ranger(spec,data=Estim.DT,case.weights=wls,mtry=mtry[Y],num.trees=num.trees[R],max.depth=max.depth[M],write.forest=TRUE,min.node.size=5,always.split.variables = c("V.L1","V.L5")) # Added the two variables directly instead of 'fixing' because of .... Error in { : task 1 failed - "object 'fixing' not found"
          } else {
            # Estimate model
            if (LogTrans) model = ranger(spec,data=Estim.DT,mtry=mtry[Y],num.trees=num.trees[R],max.depth=max.depth[M],write.forest=TRUE,min.node.size=5,num.threads=1,always.split.variables = c("V.L1.Log","V.L5.Log")) # Added the two variables directly instead of 'fixing' because of .... Error in { : task 1 failed - "object 'fixing' not found"
            if (!LogTrans) model = ranger(spec,data=Estim.DT,mtry=mtry[Y],num.trees=num.trees[R],max.depth=max.depth[M],write.forest=TRUE,min.node.size=5,num.threads=1,always.split.variables = c("V.L1","V.L5")) # Added the two variables directly instead of 'fixing' because of .... Error in { : task 1 failed - "object 'fixing' not found"
          }
          
          
          # Calculate 2nd order scaling of the variance
          fitvol = predict(model,data=Estim.DT,mtry=mtry[Y],num.trees=num.trees[R],max.depth=max.depth[M],num.threads=1)$predictions
          res = (as.numeric(fitvol) - Estim.DT[,all.vars(spec)[1]])
          
          # Model predictions
          pred = as.numeric(predict(model,data=Predi.DT,mtry=mtry[Y],num.trees=num.trees[R],max.depth=max.depth[M],num.threads=1)$predictions)
          
          if (is.na(pred[1])) {
            
            return(NA) 
            
          } else {
            
            if (LogTrans) {
              # Lower order approximation
              if (orderapprox< 2) pred = exp(pred)
              # Second order approximation
              if (orderapprox==2) pred = exp(pred)*(1+mean(res^2,na.rm=T)/2)
              # Higher order approximation
              if (orderapprox> 2) {
                scale = c(1,0); 
                for (k in 2:orderapprox) scale[k+1] = mean(res^k)/factorial(k)
                pred = exp(pred)*sum(scale)
              }
            }
          }
          
          # Maximum & Minimum realized RV in the estimation window - needed for sanity filter
          y.max = ifelse(LogTrans,exp(max(Estim.DT[dim(Estim.DT)[1]:1,dep],na.rm=T))-1,max(Estim.DT[dim(Estim.DT)[1]:1,dep],na.rm=T))
          y.min = ifelse(LogTrans,exp(min(Estim.DT[dim(Estim.DT)[1]:1,dep],na.rm=T))-1,min(Estim.DT[dim(Estim.DT)[1]:1,dep],na.rm=T))
          
          if (length(which(pred>y.max))>0) pred[which(pred>y.max)] = y.max
          if (length(which(pred<y.min))>0) pred[which(pred<y.min)] = y.min
          point=7
          return(pred)
        }
        
        fore.tmp = c(); for (i in 1:length(TMP)) fore.tmp = c(fore.tmp,TMP[[i]])
        fore[,Y,R,M] = fore.tmp
      }
    }
  }
  
  # For each estimation window select preferred forecast
  rf = matrix(NA,nrow=NF-CS,ncol=LF+1)
  colnames(rf) = c('true',paste('rf.',loss,sep=''))
  rownames(rf) = as.character(DT$Date[(W+CS+1):TT])
  rf[,1] = exp(DT[which(DT$Date %in% DT$Date[(W+CS+1):TT]),all.vars(spec)[1]])
  
  if (LogTrans) rf[,1] = exp(DT[which(DT$Date %in% DT$Date[(W+CS+1):TT]),all.vars(spec)[1]])
  if (!LogTrans) rf[,1] = DT[which(DT$Date %in% DT$Date[(W+CS+1):TT]),all.vars(spec)[1]]
  
  # Loop and select optimal models
  TMP = foreach::foreach(r = (CS+1):(NF-1)) %dopar% {
    
    rf.select = matrix(NA,nrow=1,ncol=LF)
    
    for (l in 1:LF) {
      
      ave.loss = array(NA,dim=c(NY,NR,NM))
      if (NY > 1) dimnames(ave.loss)[[1]] = paste('mtry_',mtry,sep='')
      if (NR >1) dimnames(ave.loss)[[2]] = paste('ntrees_',num.trees,sep='')
      if (NM >1) dimnames(ave.loss)[[3]] = paste('depth_',max.depth,sep='')
      for (Y in 1:NY) {
        for (R in 1:NR) {
          for (M in 1:NM) {
            true  = PY[1:(r-1)]
            fr    = as.numeric(fore[1:(r-1),Y,R,M])
            # Robustify results - remove 6 highest true values
            top.idx = head(true[order(true,decreasing = T)],n=6); top.idx = which(true %in% top.idx)
            true  = true[-top.idx]
            fr    = fr  [-top.idx]
            if (loss[l]=='MSE') ave.loss.res = mse(fr,true)$mean
            if (loss[l]=='QLIKE') ave.loss.res = qli(fr,true)$mean
            if (loss[l]=='MAE') ave.loss.res = mae(fr,true)$mean
            if (loss[l]=='MAPE') ave.loss.res = mape(fr,true)$mean
            ave.loss[Y,R,M] = ave.loss.res
          }
        }
      }
      
      # RF
      idx.mse = which(ave.loss==min(ave.loss),arr.ind = TRUE)
      rf.select[1,l] = fore[r,idx.mse[1],idx.mse[2],idx.mse[3]]
      
    }

    return(rf.select)
  }
  stopCluster(cl)
  
  for (i in 1:length(TMP)) rf[i,-1] = TMP[[i]]
  
  rf = data.frame(Date=as.character(DT$Date[(W+CS+1):TT]),rf)
  rf$Date = as.Date(rf$Date,format='%Y-%m-%d')
  
  return(rf)
}

###################################################
# SYNCHRONIZE PREDICTED FORECASTS
synch = function(cslr=pred.cslr,reg=pred.reg,ranfor=pred.rf) {
  
  cslr = cslr[cslr$Date %in% reg$Date ,]
  reg  =  reg[reg$Date  %in% cslr$Date,]
  cslr = cslr[cslr$Date %in% reg$Date ,]
  reg  =  reg[reg$Date  %in% cslr$Date,]
  
  ranfor = ranfor[ranfor$Date %in% cslr$Date  ,]
  cslr   = cslr[cslr$Date     %in% ranfor$Date,]
  ranfor = ranfor[ranfor$Date %in% cslr$Date  ,]
  cslr   = cslr[cslr$Date     %in% ranfor$Date,]
  
  ranfor = ranfor[ranfor$Date %in% reg$Date  ,]
  reg    = reg[reg$Date     %in% ranfor$Date,]
  ranfor = ranfor[ranfor$Date %in% reg$Date  ,]
  reg    = reg[reg$Date     %in% ranfor$Date,]
  
  # Check
  if (sum(ranfor$Date != cslr$Date) + sum(ranfor$Date != reg$Date) + sum(cslr$Date != reg$Date) > 0) return('Synchronization failed')
  
  # Number of observations
  TT = dim(cslr)[1]
  
  # Combine datasets
  pred = data.frame(cslr,reg[,-which(names(reg) %in% c('Date','true'))],ranfor[,-which(names(reg) %in% c('Date','true'))])

  return(pred)
}


###################################################

# "main" orchestrator function for "general models" 
general_estimation <- function(store, DR,dep = 'V.H1.Log',category="att",senttype="emolex",fixing = c('V.L1.Log','V.L5.Log','V.L22.Log'),
                               estim.type,LogTrans,formats,K,addto,W,nc,orderapprox, nams, IA=NULL){
  
  # Set up individual independent variables
  categories=category
  
  # Loop over all selected categories
  LC=length(categories)
  for (lc in 1:LC){
    
    category = categories[lc]
    
    # Set up individual independent variables, use dataframe nams to select variables
    
    # If category is 'att', use all variables in the category attention and only use "general" keywords
    if (category == 'att'){
      indep = as.character(nams$Var[which(nams$Keyword=='General' & nams$Category == category)])
    }
    
    # If category is 'positive emotions' or 'negative emotions', select all variables in the category and only use "general" keywords
    if (category == 'positive emotions' ||	category == 'negative emotions'){
      indep = as.character(nams$Var[which(nams$Keyword=='General' & nams$Type.of.variable == category & nams$Sentiment == senttype)])
    }
    
    # If category is 'dummy', select all variables in the category and only use "general" keywords
    if (category == 'dummy'){
      indep = as.character(nams[which(nams$Type.of.variable==category),'Var'])
      
      # If interaction between data is needed perform now
      if (!is.null(IA)) DR = InterAct(DR,cross=c('V.L1.Log'),explanat=indep)
      indep = paste(indep,1,sep = '.')
    }

    # Prepare name for the result
    cat.name = paste('gen', substr(category , 1, 3), sep = ".")
    if (category == 'positive emotions' ||	category == 'negative emotions'){
      cat.name = paste('gen', substr(category , 1, 3), substr(senttype , 1, 3), sep = ".")
    }

    # Store names of selected variables and categories
    if (lc==1){
      all.indep<-indep
      all.cat.name<-cat.name
    } else {
      all.indep<-c(all.indep, indep)
      all.cat.name<-paste(all.cat.name, cat.name, sep=".")
    }
  }
  print("Selected the following variables:")
  print(all.indep)
  print(paste("Model predictions stored under the name:",all.cat.name))
  
  # Estimate the model
  tmp = estim.bench(DR,dep,all.indep,fixing,estim.type,LogTrans,formats,K,addto,W,nc,orderapprox)
  
  # Store results
  names(tmp) = c('Date','true', all.cat.name)
  store[[all.cat.name]] = tmp
  return(store)
}



###############
# "main" orchestrator function for "event models"
# Takes a long time to run, as it produces predictions with csr, adalasso and rf
event_estimation <- function(store, DR,dep = 'V.H1.Log',category="att",senttype="emolex",fixing = c('V.L1.Log','V.L5.Log'),
                             estim.type,LogTrans,formats=NULL,K,addto,W,nc,orderapprox,
                             bench  = as.formula(V.H1.Log~V.L1.Log+V.L5.Log+V.L22.Log), cx, 
                             alphas,reestim, nlambda, loss, CS, nams, 
                             IA=NULL, alwayssplit=c('V.L1.Log','V.L5.Log'),LogTransRF=T){
  
  # Set up individual independent variables
  categories=category
  
  # Loop over all selected categories
  LC=length(categories)
  for (lc in 1:LC){
    
    category = categories[lc]
    print(category)
    
    # If category is 'att', use all variables in the category attention including "general" and "event-specific" keywords
    if (category == 'att'){
      att = as.character(nams[which(nams$Category==category),'Var'])
    }
    
    # If category is 'positive emotions' or 'negative emotions', select all variables in the category
    if (category == 'positive emotions' ||	category == 'negative emotions'){
      att = as.character(nams$Var[which(nams$Type.of.variable == category & nams$Sentiment == senttype)])
    }
    
    # If category is 'dummy', select all variables in the category "dummy"
    if (category == 'dummy'){
      att = as.character(nams[which(nams$Type.of.variable==category),'Var'])
      print(att)
      
      # If interaction between data is needed perform now
      if (!is.null(IA)) DR = InterAct(DR,cross=c('V.L1.Log'),explanat=att)
      
      att = paste(att,1,sep = '.')
      print(att)
    }
    
    # Prepare name for the result
    cat.name = substr(category , 1, 3)
    if (category == 'positive emotions' ||	category == 'negative emotions'){
      cat.name = paste(substr(category , 1, 3), substr(senttype , 1, 3), sep = ".")
    }
    
    # If category is 'superbench', select the following variables
    if (category == 'superbench'){
      att = c('JC.L1.Log','JC.L5.Log','JC.L22.Log','CC.L1.Log','CC.L5.Log','CC.L22.Log',
              'NSV.L1.Log','NSV.L5.Log','NSV.L22.Log','SJ.L1.Log','SJ.L5.Log','SJ.L22.Log')
      cat.name = category
    } 
    
    
    # Store names of selected variables and categories
    if (lc==1){
      all.att<-att
      all.cat.name<-cat.name
    } else {
      all.att<-c(all.att, att)
      all.cat.name<-paste(all.cat.name, cat.name, sep=".")
    }
  }

  print("Selected the following variables:")
  print(all.att)
  print(paste("Model predictions stored under the name:",all.cat.name))
  
  # Bind all independent variables except for the ones "fixed" in the model
  indep = c(setdiff(c('V.L1.Log','V.L5.Log','V.L22.Log'),fixing),all.att)
  print("Selected the following independent variables:")
  print(indep)
  
  print("These variables are fixed:")
  print(fixing)
  
  
  # TRANSFORM ATTENTION/SENTIMENT VARIABLES
  if (!is.null(formats)){
    DRT = transform(DR,totrans=all.att,formats=formats,K=K,addto=addto)
    print("transformed")
    # If addto is TRUE, we add the transformed variables to the original dataset
    if (addto) {
      att = DRT$att
      DRT  = DRT$DR
      print("addded")
    }
  } else {
    DRT=DR
    print("not transformed")
  }

  store[['Data']][[all.cat.name]] = DRT
  print("data stored")
  
  D = Sys.time()
  
  # CSLR - Complete subset linear regression models
  # LIST OF SPECIFICATIONS
  specs = specs.create(dep=dep,fixing=fixing,indep=indep,cx=cx)
  print("csr specs done")
  # ROLLING CSLR
  pred = ROLL.CSLR(DR=DRT,specs=specs,bench=bench,estim.type=estim.type,LogTrans=LogTrans,W=W,nc=nc,orderapprox)
  gc(); closeAllConnections()
  print("csr pred done")
  # COMPILE CSLR FORECASTS
  pred.cslr = COMBINE.CSLR(DR = DRT, pred = pred, W=W, CS=CS, loss=loss, dep = dep, LogTrans=LogTrans, delta = delta, trim = trim, ngroup = ngroup, nc=nc, specs) 
  print("csr pred combined")
  store[[paste(all.cat.name, 'cf.cslr.optim', sep=".")]] = pred.cslr$var.imp
  pred.cslr = pred.cslr$predict
  gc(); closeAllConnections()
  print('CSR DONE')
  print(Sys.time()-D)
  
  # If category is 'superbench', we stop here and store the results
  if (category == "superbench"){
  
	# SYNCHRONIZE FORECASTS
	gc(); closeAllConnections()
	store[[all.cat.name]] = pred.cslr
	print("all stored")
  
	# For other categories we continue with LASSO and RF
  } else {
	# LASSO
	# FULL MODEL SPECIFICATIONS
	spec = full.spec(dep=dep,fixing=fixing,indep=indep)
	print("lasso specs done")
	# REGULARIZATION
	pred.reg = regul(spec = spec, DT = DRT, W = W, CS = CS, alphas = alphas, reestim = reestim, 
                     nlambda = nlambda, loss = loss, nc = nc, LogTrans=LogTrans, orderapprox, estim.type)
 
	print("lasso pred done")
	cf.optim = pred.reg$cf.opti
	pred.reg = pred.reg$predict
	gc(); closeAllConnections()
	print('LASSO DONE')
	print(Sys.time()-D)
  
  
  
	# RANDOM FOREST
	
	# If LogTransRF is FALSE we want to use a non-log transformed RV variables
	# This is a setting we do not use in the paper
	if (!LogTransRF){
	  dep="V.H1"
	  indep = c(setdiff(c('V.L1','V.L5','V.L22'),alwayssplit),all.att)
	  spec = full.spec(dep=dep,fixing=alwayssplit,indep=indep)
	  print(spec)
	}
	
	# PREDICT WITH RANDOM FOREST
	pred.rf = rf(spec = spec, DT = DRT, W = W, CS = CS, num.trees = num.trees, mtry = mtry[mtry<length(indep)], max.depth = md,
                loss = loss, reestim = reestim, nc = nc, alwayssplit = alwayssplit, LogTrans=LogTransRF, orderapprox, estim.type)
  
	print("RF pred done")
	gc(); closeAllConnections()
	print('RF DONE')
	print(Sys.time()-D)
  
  
	# SYNCHRONIZE FORECASTS
	pred = synch(cslr=pred.cslr,reg=pred.reg,ranfor=pred.rf)
	print("all predictions synchronized")
	gc(); closeAllConnections()
	store[[all.cat.name]] = pred
	store[[paste(all.cat.name, 'cf.optim', sep=".")]] = cf.optim
	print("all stored")
  }
  
  return(store)
}


# Helper function for loading and renaming our input data
loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}


  
# In case of realized variance we should add interaction RV x Sentiment/Attention
# Define variables by names or by the number of column in the dataset in arguments cross and explanat
InterAct = function(DT,cross=2,explanat=c(17:197)) {
  
  # Number of variables to interact
  NVI = length(explanat)
  
  # Perform interaction and save
  interact = DT[,cross] * DT[,explanat]
  
  # Add to the dataset
  DT = data.frame(DT,interact)
  
  return(DT)
}

# Function to unregister the parallel backend
unregister_dopar <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}
