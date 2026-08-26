library(parallel)
library(zoo)
library(here)


#-------------------------------------------------------------------------------
# Estimate the gamma of a logistic function
speed <- function(x, scale = c(0.49,0.51)) {

  FLIP <- FALSE

  if (scale[1]<0 & scale[2]<0) {
    scale <- -1*scale
    FLIP  <- TRUE
  }

  gamma <- function(x, p_lower = scale[1], p_upper = scale[2], na.rm = TRUE, quantile_type = 7) {
    # Basic argument checks
    if (!(is.numeric(p_lower) && is.numeric(p_upper)))
      stop("p_lower and p_upper must be numeric.")
    if (!(0 < p_lower && p_lower < p_upper && p_upper < 1))
      stop("Require 0 < p_lower < p_upper < 1.")

    # Compute quantiles of x (already log-transformed)
    qL <- as.numeric(quantile(x, probs = p_lower, na.rm = na.rm, names = FALSE, type = quantile_type))
    qU <- as.numeric(quantile(x, probs = p_upper, na.rm = na.rm, names = FALSE, type = quantile_type))

    # Numerator = difference in logits
    num <- qlogis(p_upper) - qlogis(p_lower)

    # Denominator = quantile difference on log-scale (since x is already log)
    den <- qU - qL

    if (!is.finite(den) || den == 0)
      stop("Degenerate quantiles: Q_pu - Q_pl is zero/inf.")

    gamma <- num / den
    return(gamma)
  }

  if (scale[2] - scale[1] < 0.001) {
    k <- 10000000000 # Sudden jump case
  }
  else {
    k  <- gamma(x, p_lower = scale[1], p_upper = scale[2], na.rm = TRUE, quantile_type = 7)
  }

  if (FLIP) return(-k) else return(k)

}
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Trade for a given model and given scale
t.mod.scale <- function(dt=dt,mod='M1_1',EW=EW,type=type,price,scale=hyp[[1]],upp,low,kap,rbc,rf) {
  # Three explicitly defined parameters in this function are in:
  # center      <- median(log(y),na.rm=T)
  # Choice for: i) median, ii) log, iii) full-sample y.

  # Select the predicted variance
  y     <- dt[,mod]
  # Select the price column
  p     <- dt[,price]
  # Select the date series
  dts   <- dt$Date
  # Extract the forecast horizon - number
  h     <- as.numeric(sub(".*?(\\d+)$", "\\1", mod))
  # Size of the time-series
  TT    <- length(y)
  # Container to store realized gross returns from volatility managed positions
  trade <- data.frame(date=rep(NA,TT),return=0,weight=0,dw=0,gross=0,net=0,speed=NA)
  # Fill-out the known
  trade$date   <- dt$Date
  # Trade
  for (r in seq_len(TT - h)) {
    # Price to 'open' a position for that period
    o.price            <- p[r]
    # Price to 'close' a position for that period
    c.price            <- p[r+h]
    # Return over that period -> assigned to 'r' not future as with h>1 we have multiple open positions at a time.
    trade[r,'return']  <- (c.price-o.price)/o.price # This is the buy & hold for a given forecast horizon
    # Select volatility forecast and take log
    ysel               <- log(y[r])

    #---------------------------------------------------------------------------
    # Centering volatilities
    if (type == 'rolling') {
      center <- median(log(y[(max(EW,r)-EW+1):c(max(EW,r))]),na.rm=T) # This will respect the rolling window used later. The first EW observations are sacrificed anyway to estimate optimum speed parameter.
      # Speed of the volatility transition in the logistic function
      gam    <- speed(log(y[(max(EW,r)-EW+1):c(max(EW,r))]),scale=scale)
    }
    if (type == 'expanding') {
      center <- median(log(y[1:c(max(EW,r))]),na.rm=T) # This will respect the rolling window used later. The first EW observations are sacrificed anyway to estimate optimum speed parameter.
      # Speed of the volatility transition in the logistic function
      gam    <- speed(log(y[1:c(max(EW,r))]),scale=scale)
    }
    #---------------------------------------------------------------------------

    # Calculate weights given the parameters and forecast
    wgti              <- as.numeric(low + (upp-low)/(1 + exp(1 * gam * (ysel - center))))
    # Input weight
    trade[r,'weight'] <- wgti
    # Gross return
    trade[r,'gross']  <- wgti*trade[r,'return']

    # Change in weights
    if (r-h<1) trade[r,'dw'] <- abs(0-wgti) else trade[r,'dw'] <- abs(trade[r-h,'weight']-wgti)
    # Net return
    # sofr - short-term funding benchmark
    idx  <- which(abs(rf$Date - dts[r])==min(abs(rf$Date - dts[r])))
    # Sometimes sofr is missing from - https://fred.stlouisfed.org/series/DTB4WK -> use last known
    sofr <- tail(na.omit(rf[1:idx,2]),n=1)/100

    trade[r,'net']    <-  trade[r,'gross'] - kap*trade[r,'dw'] - ((1+sofr+rbc)^(h/252)-1)*max(trade[r,'weight']-1,0) + ((1+sofr)^(h/252)-1)*max(1-trade[r,'weight'],0)
    #trade[r,'net']    <-  trade[r,'gross'] - kap*trade[r,'dw'] - ((1+sofr+rbc)^(h/252)-1)*max(trade[r,'weight']-1,0)

    # Speed of transition actually employed
    trade[r,'speed'] <- gam

  }
  return(trade)
}
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Trade for a given model over a set of scales
t.mode <- function(dt=dt,mod='M1_1',EW=EW,type=type,price,scales=hyp,upp,low,kap,rbc,rf) {
  # Select the predicted variance
  y     <- dt[,mod]
  # Select the price column
  p     <- dt[,price]
  # Extract the forecast horizon - number
  h     <- as.numeric(sub(".*?(\\d+)$", "\\1", mod))
  # Size of the time-series
  TT    <- length(y)
  # Select number of scales
  NS    <- length(scales)
  # Container to store realized gross returns from volatility managed positions across different scales (speeds of transition)
  trade <- array(NA,dim=c(TT,6,NS))
  dimnames(trade)[[1]] <- dt$Date
  dimnames(trade)[[2]] <- c('return','weight','dw','gross','net','speed')
  dimnames(trade)[[3]] <- as.character(scales)
  trade[1,,]           <- 0

  # Loop over scales
  for (s in seq_len(NS)) trade[,,s] <- as.matrix(t.mod.scale(dt=dt,mod=mod,EW=EW,type=type,price=price,scale=scales[[s]],upp=upp,low=low,kap=kap,rbc=rbc,rf=rf)[,-1])

  return(trade)
}
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Find optimum speed in a rolling/expanding window -> maximize trade adjusted Sharpe ratio
t.mode.opt <- function(trade,mod='M1_1',EW=EW,type=type,kap=kap,rbc=rbc) {
  # Select number of scales
  NS    <- dim(trade)[3]
  # Size of the time-series
  TT    <- dim(trade)[1]
  # Extract the forecast horizon - number
  h     <- as.numeric(sub(".*?(\\d+)$", "\\1", mod))

  # Container to store returns, opt.gamma, weights
  res        <- data.frame(date=rep(NA,TT),return=NA,opt.gam=NA,weight=NA,gross=NA,net=NA)
  # Fill-out the known
  res$date   <- dimnames(trade)[[1]]

  # Loop over-time and select optimum gamma
  for (r in (EW+1):(TT-h)) {
    # Container to store historical Sharpe ratios
    sharpes <- rep(NA,NS)
    # Roll over different speed (gamma) parameters
    for (s in seq_len(NS)) {
      if (type == 'rolling') {
        # Net returns
        ret        <- trade[(r-EW):(r-h),'net',s]
      }
      if (type == 'expanding') {
        # Net returns
        ret        <- trade[1:(r-h)     ,'net',s]
      }
      sde        <- sqrt(sum((ret-mean(ret))^2)/length(ret))
      sharpes[s] <- mean(ret)/sde
    }
    # Select optimum gamma parameter
    opt.gam        <- which(sharpes==max(sharpes))
    res$opt.gam[r] <- trade[r,'speed',opt.gam]
    res$gross[r]   <- trade[r,'gross',opt.gam]
    res$net[r]     <- trade[r,'net',opt.gam]
    res$weight[r]  <- trade[r,'weight',opt.gam]
    res$return[r]  <- trade[r,'return',min(2L, NS)] # Has to be same for ,1] ,2] ,3] ... ,7]
  }

  return(res)
}
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Go over every model and store - full results and selected results - table
t.all.mode <- function(dt=dt,vmod=vmod,EW=EW,type=type,price,scales=hyp,upp,low,kap,rbc,B,alpha,rf) {
  library(MCS)
  library(sandwich)

  # Number of models
  NM <- length(vmod)
  # Number of observations
  TT <- nrow(dt)
  # Full results
  res <- list()
  # Extract the forecast horizon - number
  h     <- as.numeric(sub(".*?(\\d+)$", "\\1", vmod[[1]]))

  # Container for selected results
  tmp           <- matrix(NA,nrow=TT,ncol=NM*4)
  colnames(tmp) <- as.vector(outer(c('g','n','w','s'),vmod, paste0))
  sel           <- data.frame(Date=dt$Date,return=NA,tmp)

  # Loop over models
  idx <- seq(3,ncol(sel),4)
  A   <- Sys.time()
  for (m in seq_len(NM)) {

    trade       <- t.mode(dt=dt,mod=vmod[m],EW=EW,type=type,price=price,scales=scales,upp=upp,low=low,kap=kap,rbc=rbc,rf=rf)
    res[[m]]    <- t.mode.opt(trade,mod=vmod[m],EW=EW,type=type,kap=kap,rbc=rbc)
    if (m == 1) sel$return <- res[[m]]$return
    sel[,idx[m]]   <- res[[m]]$gross
    sel[,idx[m]+1] <- res[[m]]$net
    sel[,idx[m]+2] <- res[[m]]$weight
    sel[,idx[m]+3] <- res[[m]]$opt.gam

  }
  print(Sys.time()-A)

  # Evaluations -
  # 1) Strategy level mean, sd, Sharpe, MCS with and without trading costs
  # 2) Information ratio against BHOLD and HAR
  # 3) Jensen's regression against BHOLD and HAR
  # 4) CE for gamma 1 and 3.
  tbl <- data.frame(mod=c('bh',vmod),M.g=NA,SD.g=NA,Sharpe.g=NA,MCS.g=NA,M.n=NA,SD.n=NA,Sharpe.n=NA,MCS.n=NA,weight=NA,speed=NA,JAC=NA,JAT=NA,IR=NA,CE1=NA,CE3=NA,CE6=NA)
  for (m in seq_len(NM)) {
    # Select risk-free returns
    rfr                 <- rf$rf[match(dt$Date, rf$Date)]
    # Select gross returns for model 'm'
    ret                 <- na.omit(sel[,idx[m]])
    tbl[m+1,'M.g']      <- round(100*((1+mean(ret))^(252/h)-1),3)
    tbl[m+1,'SD.g']     <- round(sd(ret)*100*sqrt(252/h),3)
    retrf               <- na.omit(sel[,idx[m]]-((1+rfr/100)^(h/252)-1))
    tbl[m+1,'Sharpe.g'] <- round((100*((1+mean(retrf))^(252/h)-1))/(sd(retrf)*100*sqrt(252/h)),3)
    # Select net returns for model 'm'
    ret                 <- na.omit(sel[,idx[m]+1])
    tbl[m+1,'M.n']      <- round(100*((1+mean(ret))^(252/h)-1),3)
    tbl[m+1,'SD.n']     <- round(sd(ret)*100*sqrt(252/h),3)
    retrf               <- na.omit(sel[,idx[m]+1]-((1+rfr/100)^(h/252)-1))
    tbl[m+1,'Sharpe.n'] <- round((100*((1+mean(retrf))^(252/h)-1))/(sd(retrf)*100*sqrt(252/h)),3)
    # Average holding
    tbl[m+1,'weight']   <- round(mean(sel[,idx[m]+2],na.rm=T),2)
    # Median speed
    tbl[m+1,'speed']    <- round(100*sum(sel[,idx[m]+3]>0,na.rm=T)/length(is.na(sel[,idx[m]+3])),1)

    # CE1, CE3, CE6
    tbl[m+1,'CE1']      <- round(252*(mean(retrf) -   var(retrf)/2),3)
    tbl[m+1,'CE3']      <- round(252*(mean(retrf) - 3*var(retrf)/2),3)
    tbl[m+1,'CE6']      <- round(252*(mean(retrf) - 6*var(retrf)/2),3)

    if (m > 1) {
      # Jensen's
      retp   <- sel[,idx[m]+1]
      # What if 'rfr' is missing somewhere?
      erp    <- (retp-((1+rfr/100)^(h/252)-1))

      # Benchmark is model 1
      retb   <- sel[,idx[1]+1]
      erb    <- (retb-((1+rfr/100)^(h/252)-1))

      mlm    <- lm(erp~erb)
      mlm    <- lmtest::coeftest(mlm, vcov. = sandwich::NeweyWest(mlm, lag = NULL, prewhite = FALSE, adjust = TRUE))
      tbl[m+1,'JAC']  <- round(mlm[1,1],6)
      tbl[m+1,'JAT']  <- round(mlm[1,3],3)
      #------------------------------------------------
      # Information ratio
      at              <- na.omit((retp-retb))
      tbl[m+1,'IR']   <- round(round(100*((1+mean(at))^(252/h)-1),3)/round(sd(at)*100*sqrt(252/h),3),3)
      #------------------------------------------------
    }
  }
  # Add Buy & Hold
  ret                             <- na.omit(sel[,'return'])
  tbl[1,c('M.g','M.n')]           <- round(100*((1+mean(ret))^(252/h)-1),3)
  tbl[1,c('SD.g','SD.n')]         <- round(sd(ret)*100*sqrt(252/h),3)
  # Excess return for BHOLD
  retrf                           <- na.omit(sel[,'return']-((1+rfr/100)^(h/252)-1))
  tbl[1,c('Sharpe.g','Sharpe.n')] <- round((100*((1+mean(retrf))^(252/h)-1))/(sd(retrf)*100*sqrt(252/h)),3)

  # MCS Sharpe ratio test for gross returns
  #tmp                             <- na.omit(sel[,idx])
  #tmp                             <- tmp/apply(tmp,2,sd)
  #tmp                             <- MCSprocedure(tmp*-1,B=B,alpha=alpha)
  #is.sm                           <- which(tbl[,1] %in% substr(tmp@Info$model.names,start=2,stop=nchar(tmp@Info$model.names)))
  #tbl[is.sm,'MCS.g']              <- 1

  # MCS Sharpe ratio test for net returns
  #tmp                             <- na.omit(sel[,idx+1])
  #tmp                             <- tmp/apply(tmp,2,sd)
  #tmp                             <- MCSprocedure(tmp*-1,B=B,alpha=alpha)
  #is.sm                           <- which(tbl[,1] %in% substr(tmp@Info$model.names,start=2,stop=nchar(tmp@Info$model.names)))
  #tbl[is.sm,'MCS.n']              <- 1

  return(tbl)
}
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Load merged_list datasets for all horizons and keep only selected columns
load_horizon_data <- function(horizons, result_files, base_path, select_cols) {
  out <- setNames(vector("list", length(horizons)), as.character(horizons))

  for (h in horizons) {
    obj_name <- "merged_list"
    file_name <- result_files[[as.character(h)]]
    load(file = file.path(base_path, file_name))

    NS <- length(merged_list)
    for (s in seq_len(NS)) merged_list[[s]] <- merged_list[[s]][,select_cols]
    out[[as.character(h)]] <- merged_list
    rm(merged_list)
  }

  out
}
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Run all assets for one horizon in parallel
run_horizon_parallel <- function(dtl, H, vmod, EW, type, price, hyp, upp, low, kap, rbc,
                                 B, alpha, rf, strategy_labels, n_workers) {

  NS <- length(dtl)
  cl <- makeCluster(min(n_workers, max(1L, NS)))
  on.exit(stopCluster(cl), add = TRUE)

  # Export globals used inside the worker
  clusterExport(cl, c("dtl","H","vmod","EW","type","price","hyp","upp","low",
                      "kap","rbc","B","alpha","rf",
                      "t.all.mode","t.mode.opt","t.mode","t.mod.scale","speed","strategy_labels"),
                envir = environment())

  # (optional) load packages on workers if t.all.mode uses them
  # clusterEvalQ(cl, { library(yourPkg) })

  t.res <- parLapplyLB(cl, seq_len(NS), function(s) {
    dt <- dtl[[s]]
    rename_len <- min(ncol(dt), length(vmod) + 3L)
    names(dt)[seq_len(rename_len)] <- c("Date","RV", paste0("M", seq_along(vmod), "_", H), "CP1")[seq_len(rename_len)]

    tbl <- t.all.mode(dt=dt, vmod=vmod, EW=EW, type=type, price=price,
                      scales=hyp, upp=upp, low=low, kap=kap, rbc=rbc,
                      B=B, alpha=alpha, rf=rf)
    tbl[,1] <- strategy_labels
    tbl
  })

  t.res
}
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Summarize t.res across assets
summarize_horizon_results <- function(t.res) {

  n_strategies <- nrow(t.res[[1]])

  # Ave Return, SD of Ave Returns, % out-perf. HAR.
  # Ave SR SD if SR % out-perf. HAR.
  # % JAC > 0, % JAT ? 1.96.
  # Ave Weights, SD of Weights, Ave Speed
  ts <- data.frame(mod=t.res[[1]][,1],
                   aretN=NA,sdretN=NA,outretN=NA,
                   asrN =NA,sdsrN =NA,outsrN =NA,
                   Z    =NA,
                   dCE1  =NA,sddCE1 =NA,outdCE1 =NA,
                   dCE3  =NA,sddCE3 =NA,outdCE3 =NA,
                   dCE6  =NA,sddCE6 =NA,outdCE6 =NA)

  har_row <- which(t.res[[1]][,1] == "HAR")
  if (length(har_row) == 0L) har_row <- 2L

  for (m in seq_len(n_strategies)) {
    # Average net return
    x <- vapply(seq_along(t.res), function(s) t.res[[s]][m,'M.n'], numeric(1))
    ts[m,'aretN'] <- mean(x); ts[m,'sdretN'] <- sd(x);

    # Average Sharpe Ratio from net returns
    x <- vapply(seq_along(t.res), function(s) t.res[[s]][m,'Sharpe.n'], numeric(1))
    ts[m,'asrN'] <- mean(x); ts[m,'sdsrN'] <- sd(x);

    # If not HAR
    if (m != har_row) {
      # Average net return out-perf
      xb <- vapply(seq_along(t.res), function(s) t.res[[s]][har_row,'M.n'], numeric(1))
      xc <- vapply(seq_along(t.res), function(s) t.res[[s]][m,'M.n'], numeric(1))
      ts[m,'outretN'] <- 100*sum(xc>xb)/length(xb)
      # Average Sharpe Ratio gross return out-perf
      xb <- vapply(seq_along(t.res), function(s) t.res[[s]][har_row,'Sharpe.n'], numeric(1))
      xc <- vapply(seq_along(t.res), function(s) t.res[[s]][m,'Sharpe.n'], numeric(1))
      ts[m,'outsrN'] <- 100*sum(xc>xb)/length(xb)

      # delta Certainty Equivalence - gamma 1
      xb <- vapply(seq_along(t.res), function(s) t.res[[s]][har_row,'CE1'], numeric(1))
      xc <- vapply(seq_along(t.res), function(s) t.res[[s]][m,'CE1'], numeric(1))
      ts[m,'dCE1']    <- mean(xc-xb)
      ts[m,'sddCE1']  <- sd(xc-xb)
      ts[m,'outdCE1'] <- 100*sum(xc>xb)/length(xb)

      # delta Certainty Equivalence - gamma 3
      xb <- vapply(seq_along(t.res), function(s) t.res[[s]][har_row,'CE3'], numeric(1))
      xc <- vapply(seq_along(t.res), function(s) t.res[[s]][m,'CE3'], numeric(1))
      ts[m,'dCE3']    <- mean(xc-xb)
      ts[m,'sddCE3']  <- sd(xc-xb)
      ts[m,'outdCE3'] <- 100*sum(xc>xb)/length(xb)

      # delta Certainty Equivalence - gamma 6
      xb <- vapply(seq_along(t.res), function(s) t.res[[s]][har_row,'CE6'], numeric(1))
      xc <- vapply(seq_along(t.res), function(s) t.res[[s]][m,'CE6'], numeric(1))
      ts[m,'dCE6']    <- mean(xc-xb)
      ts[m,'sddCE6']  <- sd(xc-xb)
      ts[m,'outdCE6'] <- 100*sum(xc>xb)/length(xb)

      # Stouffer from Jensen's alpha
      xc <- vapply(seq_along(t.res), function(s) t.res[[s]][m,'JAC'], numeric(1))
      xt <- vapply(seq_along(t.res), function(s) t.res[[s]][m,'JAT'], numeric(1))
      if (length(which(xt==0))>0) {
        xc <- xc[-which(xt==0)]
        xt <- xt[-which(xt==0)]
      }
      if (length(which(xc==0))>0) {
        xt <- xt[-which(xc==0)]
        xc <- xc[-which(xc==0)]
      }
      if (length(xt) > 0 && length(xc) > 0) {
        xe <- xc/xt
        ws <- 1/xe
        ts[m,'Z'] <- sum(ws*xt)/sqrt(sum(ws^2))
      }
    }
  }

  ts
}
#-------------------------------------------------------------------------------