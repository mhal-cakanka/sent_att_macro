#  TRADING STRATEGY ANALYSIS
# This script analyzes a trading strategy by evaluating various models and their performance metrics over a specified forecast horizon.
# Make sure to run the portfolio_data_merge.R script first to generate the necessary merged data files in the ./portfolios/results/ directory.

################### Load packages, functions and define wds ###################

rm(list=ls()); gc()

# Save the current working directory
parent_wd <- getwd()

# Load shared functions and libraries
source(here::here('shared_functions.R'))
port_path <- "./portfolios"; create_wd(port_path)
source(here::here('portfolios','trading_strategy_functions.R'))


############################### Parameters ######################################

# Forecast horizons to process
horizons <- c(1, 5, 22)

# Input file names by horizon
result_files <- setNames(
  paste0("port_results_5w_H", horizons, "_MSE_all.RData"),
  as.character(horizons)
)

# Risk-free series file name
rf_file <- "DTB4WK.csv"

# Strategy parameters
price_col <- "CP1" # What is the adjusted close.
kap       <- 0.001  # Trading costs (0.001 = 10bps) - percentage from trading volume, in p.a.
rbc       <- 0.005  # Borrowing costs (0.005 = 50bps) in p.a.
upp       <- 2      # Maximum leverage
low       <- 0      # Minimum position size
hyp       <- list(c(0.50,0.50),c(0.30,0.70),c(0.10,0.90),c(0.01,0.99),-c(0.30,0.70),-c(0.10,0.90),-c(0.01,0.99))
EW        <- 252 # Estimation window for finding some optimum parameter(s).
type      <- 'rolling'
B         <- 1000
alpha     <- 0.25

# Parallel
n_workers <- min(16L, max(1L, detectCores(logical = TRUE) - 2L))

# Model naming and labels
n_models <- 11L
model_prefix <- "M"
strategy_labels <- c("BHOLD","HAR","CSR_FULL","HAR_M","HAR_A","CSR_A","ALA_A","RF_A","HAR_S","CSR_S","ALA_S","RF_S")
n_strategies <- length(strategy_labels)

# Relevant columns in loaded merged_list
nms <- c("Date","rv","har","cslr_har_km5_full","gen_dum","gen_att","cslr_att_km5",
         "lasso_att","rf_att","gen_posneg_fin","cslr_posneg_fin_km5","lasso_posneg_fin",
         "rf_posneg_fin","aC")


################################ Load all data #################################

# Source: https://fred.stlouisfed.org/series/DTB4WK for 2016-02-24 to 2021-02-24
rf_raw <- read.csv(file = file.path(port_path, rf_file))
rf_raw[,1] <- as.Date(rf_raw[,1], format = '%Y-%m-%d')
names(rf_raw) <- c('Date','rf')
# forward fill and subset trading dates from dt
rf_raw <- na.locf(na.locf(rf_raw), fromLast = TRUE)

# Load all horizon datasets at start
h_data <- load_horizon_data(
  horizons      = horizons,
  result_files  = result_files,
  base_path     = file.path(port_path, "results"),
  select_cols   = nms
)

# Align rf to all trading dates across all horizons
# all_dates <- sort(unique(do.call(c, lapply(h_data, function(x) x$Date))))
# rf <- rf_raw[rf_raw$Date %in% all_dates, , drop = FALSE]
rf <- rf_raw

########################## Example on one stock ################################

# Select toy example for working with functions
example_h <- as.character(horizons[1])
dt  <- h_data[[example_h]][[min(80L, length(h_data[[example_h]]))]]

# Rename
rename_len <- min(ncol(dt), n_models + 3L)
names(dt)[seq_len(rename_len)] <- c('Date','RV',paste0(model_prefix, seq_len(n_models), '_', as.integer(example_h)),'CP1')[seq_len(rename_len)]

# Plot prices
plot.ts(dt$CP1)

# Input parameters
H     = as.integer(example_h) # Forecast horizon(s).
vmod  = paste0(model_prefix, seq_len(n_models), '_', H) # Name of the models without the forecast horizon information

A   <- Sys.time()
tbl <- t.all.mode(dt=dt,vmod=vmod,EW=EW,type=type,price=price_col,scales=hyp,upp=upp,low=low,kap=kap,rbc=rbc,B=B,alpha=alpha,rf=rf)
Sys.time()-A
tbl[,1] <- strategy_labels
tbl


############################# Go over assets ###################################

output_path <- file.path(port_path, "port_results")
create_wd(output_path)

for (H in horizons) {

  cat("\n================ Horizon:", H, "================\n")

  dtl <- h_data[[as.character(H)]]
  vmod <- paste0(model_prefix, seq_len(n_models), '_', H)

  A <- Sys.time()
  t.res <- run_horizon_parallel(
    dtl = dtl,
    H = H,
    vmod = vmod,
    EW = EW,
    type = type,
    price = price_col,
    hyp = hyp,
    upp = upp,
    low = low,
    kap = kap,
    rbc = rbc,
    B = B,
    alpha = alpha,
    rf = rf,
    strategy_labels = strategy_labels,
    n_workers = n_workers
  )
  Sys.time()-A

  names(t.res) <- names(dtl)

  t_res_file <- file.path(output_path, paste0("t_res_H", H, ".RData"))
  save(t.res, file = t_res_file)

  # rm(list=ls())
  load(file = t_res_file)

  # Ave Return, SD of Ave Returns, % out-perf. HAR.
  # Ave SR SD if SR % out-perf. HAR.
  # % JAC > 0, % JAT ? 1.96.
  # Ave Weights, SD of Weights, Ave Speed
  ts <- summarize_horizon_results(t.res = t.res)

  print(cbind(ts[,1], round(ts[,-1], 2)))

  write.csv(ts, file = file.path(output_path, paste0("h", H, ".csv")), row.names = FALSE)
}
