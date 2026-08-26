# Utility functions to load market data and model results, select model predictions
# and merge selected model predictions with original market variables.
#
# These are helpers used by portfolio_data_merge.R

# Load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(here, readr, dplyr, lubridate, tools)

# Try to source shared_functions.R if available (provides loadRData / create_wd in the repo)
try({
  src_sf <- here::here("shared_functions.R")
  if (file.exists(src_sf)) source(src_sf)
}, silent = TRUE)

# Fallback loadRData if not provided by shared_functions.R
if (!exists("loadRData")) {
  loadRData <- function(fileName){
    # load an RData file containing a single object and return it
    env <- new.env()
    nm <- load(fileName, envir = env)
    if(length(nm) == 1) return(env[[nm]])
    # if multiple objects, return a named list
    res <- mget(nm, envir = env)
    return(res)
  }
}

# Function: load_market_and_results
# - parent_wd: root of repo / working dir where script is launched
# - data_wd: relative path to folder containing hf_market_... RData
# - model_results_wd: relative path to folder containing results_... RDS
# - vers, depnum: version and dependent variable number
# Returns a list with elements: parent_wd, stocks.market, results
load_market_and_results <- function(parent_wd = getwd(),
                                    data_wd = "./stockdata/final/other_versions",
                                    model_results_wd = "./models/results",
                                    vers = "5w",
                                    depnum = 1,
                                    estim.type = "WLS3") {
  cur_wd <- getwd()
  
  # Move to data folder and load stocks.market
  setwd(parent_wd)
  # data_path <- file.path(parent_wd, data_wd)
  data_path <- normalizePath(file.path(parent_wd, sub("^\\./+", "", data_wd)),
                             winslash = "/", mustWork = FALSE)
  if (!dir.exists(data_path)) stop("data_wd not found: ", data_path)
  setwd(data_path)
  filename <- paste0("hf_market_", vers, "_2.RData")
  message("Loading market data: ", filename, " from ", data_path)
  stocks.market <- loadRData(filename); gc()
  message("Loaded file ", filename)
  
  # Return to parent
  setwd(parent_wd)
  
  # Load model results (RDS)
  # model_results_path <- file.path(parent_wd, model_results_wd)
  model_results_path <- normalizePath(file.path(parent_wd, sub("^\\./+", "", model_results_wd)),
                                      winslash = "/", mustWork = FALSE)
  if (!dir.exists(model_results_path)) stop("model_results_wd not found: ", model_results_path)
  filename <- paste0("results_", vers, "_H", depnum)
  if (estim.type != "WLS3") filename <- paste0(filename, "_", estim.type)
  message("Loading model results: ", filename, " from ", model_results_path)
  results <- readRDS(file.path(model_results_path, filename)); gc()
  message("Loaded file ", filename)
  
  # restore cwd
  setwd(cur_wd)
  
  return(list(stocks.market = stocks.market, results = results))
}


# Function: select_models_from_results
# - results: the list object loaded from results RDS (each element = list of dataframes per model)
# - selection: character vector of elements (names) to pick inside each stock result (like c("gen.att","att",...))
# - model_dict: data.frame read from tables/model.dict.csv (columns like MSE, QLIKE, MODELS)
# - nms: desired final column order (vector including 'Date' and 'rv' and model MODELS names)
# - calibri_col: which column in model_dict to use to map original column names (default "MSE")
# Returns: list of data.frames (one per stock) with columns = nms; rows = intersection of dates present in all selected elements
select_models_from_results <- function(results,
                                       selection,
                                       model_dict,
                                       nms,
                                       calibri_col = "MSE") {
  out_list <- list()
  # iterate over stocks in results
  for (i in seq_along(results)) {
    stock_name <- names(results)[i]
    # print the name of the stock being processed
    message("Processing stock: ", stock_name)
    dt <- results[[i]]
    # safety: if selection elements absent, skip stock with warning
    missing_sel <- setdiff(selection, names(dt))
    if (length(missing_sel) > 0) {
      warning("Stock ", stock_name, " missing selection elements: ", paste(missing_sel, collapse = ", "),
              " -> skipping this stock.")
      next
    }
    new.dt <- dt[selection]
    # Gather dates present in each element and keep only dates present in all selection members
    # This mirrors logic from tables::loss.dt
    gd <- NULL
    for (g in seq_along(new.dt)) {
      if (g == 1) {
        gd <- as.Date(new.dt[[g]][, "Date"])
      } else {
        gd <- c(gd, as.Date(new.dt[[g]][, "Date"]))
      }
    }
    # common dates across all selected dataframes
    gd_common <- as.Date(names(table(gd))[which(table(gd) == length(new.dt))], format = "%Y-%m-%d")
    # Subset each element to common dates. For the first element keep Date and true,
    # for others drop their Date and true columns (like loss.dt)
    for (g in seq_along(new.dt)) {
      if (g == 1) {
        new.dt[[g]] <- new.dt[[g]][new.dt[[g]][, "Date"] %in% gd_common, , drop = FALSE]
      } else {
        cols_remove <- which(colnames(new.dt[[g]]) %in% c("Date", "true"))
        if (length(cols_remove) > 0) {
          new.dt[[g]] <- new.dt[[g]][new.dt[[g]][, "Date"] %in% gd_common, -cols_remove, drop = FALSE]
        } else {
          new.dt[[g]] <- new.dt[[g]][new.dt[[g]][, "Date"] %in% gd_common, , drop = FALSE]
        }
      }
    }
    # cbind them together
    fin.dt <- do.call("cbind", new.dt)
    # Rename first two cols to Date and true (mirrors tables::loss.dt)
    if (ncol(fin.dt) >= 2) colnames(fin.dt)[1:2] <- c("Date", "true")
    # Now subset columns using model_dict mapping: choose model_dict rows where MODELS in nms,
    # and use calibri_col values to pick columns from fin.dt
    # Some safety checks:
    if (!(calibri_col %in% colnames(model_dict))) stop("calibri_col not found in model_dict: ", calibri_col)
    # Rows that map to desired models
    map_rows <- model_dict[model_dict$MODELS %in% nms, , drop = FALSE]
    # The calibri_col values must correspond to column names present in fin.dt
    # Use only those map_rows whose calibri_col exists in fin.dt
    valid_map <- map_rows[map_rows[[calibri_col]] %in% colnames(fin.dt), , drop = FALSE]
    if (nrow(valid_map) == 0) {
      warning("No mapping found between model_dict[", calibri_col, "] and fin.dt columns for stock ", stock_name,
              ". Skipping.")
      next
    }
    dt_sel <- fin.dt[, valid_map[[calibri_col]], drop = FALSE]
    # Rename columns to MODELS
    colnames(dt_sel) <- valid_map$MODELS
    # Add Date/true columns if not present (they should be derived from dt_sel if included in valid_map)
    # If Date/true are not currently in dt_sel, but present in fin.dt, prepend them
    if (!("Date" %in% colnames(dt_sel)) && ("Date" %in% colnames(fin.dt))) {
      dt_sel <- cbind(Date = fin.dt[, "Date"], dt_sel)
    }
    # If 
    if (!("true" %in% colnames(dt_sel)) && ("true" %in% colnames(fin.dt))) {
      dt_sel <- cbind(dt_sel[, 1, drop = FALSE], true = fin.dt[, "true"], dt_sel[, -1, drop = FALSE])
      # reorder so Date is first if present
      if ("Date" %in% colnames(dt_sel)) {
        dt_sel <- dt_sel[, c("Date", setdiff(colnames(dt_sel), "Date")), drop = FALSE]
      }
    }
    # Finally reorder columns to match nms. If a column from nms missing, fill with NA
    # Note: in nms user expects 'Date' and 'rv' (rv is coming from 'true' column in fin.dt -> rename)
    # If 'rv' is among nms, rename 'true' -> 'rv'
    if ("true" %in% colnames(dt_sel)) colnames(dt_sel)[colnames(dt_sel) == "true"] <- "rv"
    # Ensure all requested columns exist:
    missing_cols <- setdiff(nms, colnames(dt_sel))
    if (length(missing_cols) > 0) {
      # add NA columns
      dt_sel[, missing_cols] <- NA
    }
    # Reorder
    dt_sel <- dt_sel[, nms, drop = FALSE]
    # Ensure Date column is Date type
    if ("Date" %in% colnames(dt_sel)) dt_sel$Date <- as.Date(dt_sel$Date)
    out_list[[stock_name]] <- dt_sel
  }
  return(out_list)
}


# Function: merge_filtered_with_market
# - filtered_results: list of dataframes created by select_models_from_results (columns include Date and models)
# - stocks.market: list of dataframes with original market variables (names aligned with filtered_results names)
# - cols_to_append: vector of column names from stocks.market to append (including Date)
# For each stock in filtered_results: find corresponding stocks.market item by name,
# subset its rows where Date in filtered_results$Date, select cols_to_append, then left-join to filtered_results by Date.
# Returns list of merged data.frames (same names as filtered_results).
merge_filtered_with_market <- function(filtered_results,
                                       stocks.market,
                                       cols_to_append = c("Date", "aC", "V.L1", "V.L5", "V.L22", "V.H1", "V.H5", "V.H22")) {
  merged_list <- list()
  for (stock_name in names(filtered_results)) {
    # print the name of the stock being processed
    message("Processing stock: ", stock_name)
    
    res_df <- filtered_results[[stock_name]]
    # Attempt to find matching market data
    if (!(stock_name %in% names(stocks.market))) {
      warning("No market data found for stock ", stock_name, " - skipping merging for this stock.")
      merged_list[[stock_name]] <- res_df
      next
    }
    market_df <- stocks.market[[stock_name]]
    # Ensure Date column is Date class in both
    if (!("Date" %in% colnames(market_df))) {
      stop("Market data for ", stock_name, " does not contain 'Date' column.")
    }
    market_df$Date <- as.Date(market_df$Date)
    res_df$Date <- as.Date(res_df$Date)
    # Subset market_df to dates present in res_df
    market_sub <- market_df[market_df$Date %in% res_df$Date, , drop = FALSE]
    # Select only requested cols if present
    available_cols <- intersect(cols_to_append, colnames(market_sub))
    market_sub <- market_sub[, available_cols, drop = FALSE]
    # Merge: keep rows from res_df (left join)
    merged <- dplyr::left_join(res_df, market_sub, by = "Date")
    # If there are duplicated Date columns (unlikely), ensure Date only one
    merged_list[[stock_name]] <- merged
  }
  return(merged_list)
}


# Function: save_portfolio_results
# - merged_list: list to save
# - outdir: directory where to save (will be created if doesn't exist)
# - vers: version used in file naming
# - filename_prefix: optional prefix
save_portfolio_results <- function(merged_list,
                                   outdir = "./portfolios/results",
                                   vers = "5w",
                                   depnum = 1,
                                   estim.type = "WLS3",
                                   filename_prefix = "port_results") {
  
  if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
  
  fname_base <- paste0(filename_prefix, "_", vers, "_H", depnum)
  if (estim.type != "WLS3") {
    fname_base <- paste0(fname_base, "_", estim.type)
  }
  # Save as RDS and RData (version 2)
  saveRDS(merged_list, file = file.path(outdir,fname_base))
  save(merged_list, file = file.path(outdir, paste0(fname_base, ".RData")), version = 2)
  message("Saved portfolio results to ", outdir, " as: ", fname_base, ".RDS and .RData")
}
