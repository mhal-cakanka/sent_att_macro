# Script to create merged portfolio-ready data for later use in the trading_strategy.R script. The merged data will be saved to ./portfolios/results/port_results_<vers>_2.RData

# - loads market data (hf_market_... .RData)
# - loads model results (results_... .rds)
# - selects subset of model predictions using model.dict.csv and selection vector
# - merges selected model predictions with chosen original market variables
# - saves the final list to ./portfolios/results/port_results_<vers>_2.RData
#
# Usage: run interactively or via Rscript portfolio_data_merge.R

# Load helper functions from portfolio_data_functions.R
if (!require("pacman")) install.packages("pacman")
pacman::p_load(here, readr)

# Source portfolio functions (this file assumes it is placed in ./portfolios)
source(here::here("portfolios", "portfolio_data_functions.R"))

# -------------- PARAMETERS (edit as needed) ----------------
parent_wd <- getwd()              # root of repository / working dir
data_wd <- "./stockdata/final/other_versions"
model_results_wd <- "./models/results"
tables_wd <- "tables"           # where model.dict.csv lives
vers <- "5w"
depnum <- 22
estim.type <- "MSE"   # 'WLS1', 'WLS3', 'MSE'
print(paste0("Processing version: ", vers, ", depnum: ", depnum, ", estim.type: ", estim.type))


# Models to keep (nms) and which result list elements to use (selection)
nms = c('Date','rv','har', "cslr_har_km5", "cslr_har_km5_full",   
        "cslr_har_km5_rv", 'gen_dum', "gen_vix",      # date, rv, benchmark models
        'gen_att','cslr_att_km5','lasso_att','rf_att',# attention models
        'gen_posneg_fin','cslr_posneg_fin_km5','lasso_posneg_fin','rf_posneg_fin') # sentiment models

selection=c("gen.att","att","gen.pos.fin.gen.neg.fin",
            "pos.fin.neg.fin","gen.dum","superbench_full"
            ,"superbench","superbench_rv","gen.vix"
            )

# portfolios wd
portfolios_dir <- file.path(parent_wd, "portfolios")
# location to save final merged outputs
outdir <- file.path(parent_wd, "portfolios", "results")

# columns from stocks.market to append for portfolio work
cols_to_append <- c("Date", "aC", 'V.L1','V.L5','V.L22', 'V.H1','V.H5','V.H22')
# ----------------------------------------------------------

# Create portfolios folder + results subfolder, if they dont already exist
create_wd(portfolios_dir)
create_wd(outdir)

# Load market data and results
load_res <- load_market_and_results(parent_wd = parent_wd,
                                    data_wd = data_wd,
                                    model_results_wd = model_results_wd,
                                    vers = vers,
                                    depnum = depnum,
                                    estim.type = estim.type)

stocks.market <- load_res$stocks.market
results <- load_res$results
rm(load_res);gc()

# Load model.dict.csv
model_dict_path <- file.path(parent_wd, tables_wd, "model.dict.csv")
if (!file.exists(model_dict_path)) stop("model.dict.csv not found at: ", model_dict_path)
model_dict <- read_delim(model_dict_path, delim = ";", escape_double = FALSE, trim_ws = TRUE)
model_dict <- as.data.frame(model_dict)

# Select models from results (this will produce a list of data.frames with columns ordered as nms)
filtered_results <- select_models_from_results(results = results,
                                               selection = selection,
                                               model_dict = model_dict,
                                               nms = nms,
                                               calibri_col = "MSE")

# Merge filtered results with stocks.market variables
merged_list <- merge_filtered_with_market(filtered_results = filtered_results,
                                          stocks.market = stocks.market,
                                          cols_to_append = cols_to_append)

# Save the merged list into portfolios/results
save_portfolio_results(merged_list = merged_list,
                       outdir = outdir,
                       vers = vers,
                       depnum = depnum,
                       estim.type = estim.type,
                       filename_prefix = "port_results")

# Remove large objects and run garbage collection
rm(stocks.market, results, filtered_results, merged_list); gc()

message("Done. Merged results saved to: ", outdir)
