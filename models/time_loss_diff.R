# TIME-VARYING LOSS DIFFERENTIALS ANALYSIS
#
# This script analyses the time-series properties of loss differentials between
# each model and the HAR benchmark, using the output of make_tables.R. It runs
# two analyses in parallel across stocks:
#
#   Part 1 - Structural break detection (strucchange):
#     For each model and stock, identifies the BIC-optimal number of structural
#     breaks (up to 4) in the MSE and QLIKE loss differentials. When no break
#     is found, records the slope of a linear time trend.
#
#   Part 2 - Time trend regression with Newey-West SEs (lmtest, sandwich):
#     NOTE: Not reported in the paper.
#     Regresses loss differentials on a linear trend and four quintile-based
#     sub-period dummies. Tests for trend significance, joint dummy significance,
#     and joint overall significance using Wald tests.


################### Load packages, functions and define wds ###################

# Save the current working directory
parent_wd <- getwd()

# Load shared functions and libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(here, lmtest, sandwich, foreach, doParallel, strucchange)
source(here::here('shared_functions.R'))


################### Load loss differentials dataset ###########################

# Loss differentials are produced by the loss differentials section of make_tables.R
# Dimensions: [loss functions, dates, models (excl. HAR), stocks]
tr_path  <- "./models/table_results"
filename <- "loss_differentials.RData"
load(file = paste(tr_path, filename, sep="/"))
print(paste("Loaded file", filename))

ld <- loss_differentials
rm(loss_differentials)
gc()

# Subset for testing with fewer stocks
# ld <- ld[,,,1:10]

# Quick check on dimensions
print(paste("Loss functions:", paste(dimnames(ld)[[1]], collapse=", ")))
print(paste("Models:", paste(dimnames(ld)[[3]], collapse=", ")))
NS <- dim(ld)[4]
print(paste("Number of stocks:", NS))


######################## Part 1: Structural Break Analysis ####################

# For each stock and model, fit a breakpoints model (with time trend) to the
# loss differentials. BIC selects the number of breaks (0-4). When zero breaks
# are selected, the slope of a linear trend is recorded to capture directionality.
# Results: res[[s]][loss, model, 1=breaks / 2=trend slope]

avail_cores <- parallel::detectCores()
n_cores <- max(1, avail_cores - 1) # Use all but one core for parallel processing)
cl <- parallel::makeCluster(n_cores)
doParallel::registerDoParallel(cl)

A   <- Sys.time()
res <- foreach(s = 1:NS,
               .packages = "strucchange",
               .inorder  = FALSE) %dopar% {

  ress <- array(NA, dim=c(2,10,2))
  dimnames(ress)[[1]] <- c('MSE','QLIKE')
  dimnames(ress)[[2]] <- dimnames(ld)[[3]]

  print(s)
  # Select dataset
  dt <- ld[,,,s]

  # Loop over 2 losses: MSE (l=1) and QLIKE (l=2)
  for (l in 1:2) {
    dl <- dt[l,,]
    dl <- na.omit(dl)
    TT <- dim(dl)[1]
    tr <- 1:TT

    # Loop over models
    for (m in 1:10) {
      # Loss differentials for model CSR, CSR-A, CSR-S c(1,4,8)
      y1  <- dl[,m]
      # Model with time-trend
      A      <- Sys.time()
      d      <- breakpoints(y1~1+tr,h=0.20,breaks=4)
      Sys.time()-A
      bic_d  <- BIC(d) 
      
      y1    <- dl[,m]
      # Model with time-trend
      d     <- breakpoints(y1~1+tr, h=0.20, breaks=4)
      bic_d <- BIC(d)
      ress[l,m,1] <- which.min(bic_d) - 1
      # If no break, record linear trend slope
      if (ress[l,m,1]==0) ress[l,m,2] <- coefficients(lm(y1~1+tr))[2]
      rm(d, bic_d)
      gc()
    }
  }
  ress
}
print(Sys.time()-A)
stopCluster(cl)

# Summarise across stocks: average number of breaks and % with positive trend slope
tmse <- matrix(NA, nrow=dim(res[[1]])[2], ncol=2)
rownames(tmse) <- dimnames(res[[1]])[[2]]
colnames(tmse) <- c('Breaks','Trend')
tqli <- tmse

for (m in 1:10) {
  x <- c(); for (s in 1:NS) x[s] <- res[[s]][1,m,1]
  tmse[m,1] <- mean(x)
  x <- c(); for (s in 1:NS) x[s] <- res[[s]][1,m,2]
  tmse[m,2] <- round(100*sum(x>0)/NS, 1)

  x <- c(); for (s in 1:NS) x[s] <- res[[s]][2,m,1]
  tqli[m,1] <- mean(x)
  x <- c(); for (s in 1:NS) x[s] <- res[[s]][2,m,2]
  tqli[m,2] <- round(100*sum(x>0)/NS, 1)
}

print("Structural break summary for MSE:")
tmse_breaks = tmse
print("Structural break summary for QLIKE:")
tqli_breaks = tqli
# Save results
saveRDS(list(tmse=tmse_breaks, tqli=tqli_breaks), file = paste(tr_path, "structural_breaks_summary.rds", sep="/"))


######################## Part 2: Time Trend Regression ########################

# Regress loss differentials on a linear trend and four quintile-based sub-period
# dummies using Newey-West HAC standard errors. Three statistics are recorded per
# model and stock:
#   Sig   - p-value on the trend coefficient (coeftest)
#   JointD - p-value for joint significance of all dummies (waldtest)
#   JointT - p-value for joint significance of the full model (waldtest)


# Setup 4D array to store results: [stock, loss, model, stat]
res <- array(NA,dim=c(NS,2,10,4))
dimnames(res)[[2]] <- c('MSE','QLIKE')
dimnames(res)[[3]] <- dimnames(ld)[[3]]
dimnames(res)[[4]] <- c('Trend','Sig','JointD','JointT')

C <- Sys.time()

for (s in 1:NS) {
  print(s)
  # Select data-set
  dt <- ld[,,,s]
  
  # Loop over two losses 
  for (l in 1:2) {
    dl <- dt[l,,]
    dl <- na.omit(dl)
    TT <- dim(dl)[1]
    # Add trend
    tr <- 1:TT
    # Add a dummies that separate the sample into four out of five parts
    se <- floor(TT/5)
    d1 <- c(rep(0,se),rep(1,se),rep(0,se),rep(0,se),rep(0,TT-4*se))
    d2 <- c(rep(0,se),rep(0,se),rep(1,se),rep(0,se),rep(0,TT-4*se))
    d3 <- c(rep(0,se),rep(0,se),rep(0,se),rep(1,se),rep(0,TT-4*se))
    d4 <- c(rep(0,se),rep(0,se),rep(0,se),rep(0,se),rep(1,TT-4*se))
    
    # Loop over models
    for (m in 1:10) {
      # Loss differentials for model CSR, CSR-A, CSR-S c(1,4,8)
      y1  <- dl[,m]
      # Model with time-trend
      d  <- lm(y1~d1+d2+d3+d4+tr)
      r1 <- lmtest::coeftest(d, vcov. = sandwich::NeweyWest(d, lag = NULL, prewhite = FALSE, adjust = TRUE))
      r2 <- lmtest::waldtest(d, . ~ tr, vcov = function(x) sandwich::NeweyWest(x, lag=NULL, prewhite=FALSE, adjust=TRUE))
      r3 <- lmtest::waldtest(d, . ~ 1 , vcov = function(x) sandwich::NeweyWest(x, lag=NULL, prewhite=FALSE, adjust=TRUE))
      
      res[s,l,m,] <- c(r1[6,c(1,4)],r2$`Pr(>F)`[2],r3$`Pr(>F)`[2])
      rm(r1,r2,r3,d)
      gc()
    }
  }
  
}
print(Sys.time()-C)
res_single <- res

# Summarise: count of stocks with significant result at 5% threshold
tmse <- matrix(NA,nrow=dim(res)[3],ncol=3)
rownames(tmse) <- dimnames(ld)[[3]]
colnames(tmse) <- c('Sig','JointD','JointT')
tqli <- tmse
thr  <- 0.05
for (m in 1:10) {
  tmse[m,1] <- sum(res[,1,m,2]<thr)
  tmse[m,2] <- sum(res[,1,m,3]<thr)
  tmse[m,3] <- sum(res[,1,m,4]<thr)
  
  tqli[m,1] <- sum(res[,2,m,2]<thr)
  tqli[m,2] <- sum(res[,2,m,3]<thr)
  tqli[m,3] <- sum(res[,2,m,4]<thr)
}
tmse_regr <- tmse
round(tmse_regr/NS*100,2) # % of stocks with significant trend, joint dummy, joint overall
tqli_regr <- tqli
round(tqli_regr/NS*100,2) 
saveRDS(list(tmse=tmse_regr, tqli=tqli_regr), file = paste(tr_path, "time_trend_regression_summary.rds", sep="/"))
