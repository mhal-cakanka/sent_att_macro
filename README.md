# Forecasting U.S. equity market volatility with attention and sentiment to the economy

This repository complements the paper "Forecasting U.S. equity market volatility with attention and sentiment to the economy."

## Overview

This repository contains code and scripts to:

- Acquire and process attention and sentiment variables from different sources using various sentiment analysis methods.
- Process high-frequency price data into RV variables.
- Train models and predict the volatility of S&P 500 stocks.
- Evaluate results across all stocks in the sample.
- Create summary tables and figures.

**Note:** Some scripts rely on proprietary data (e.g., Bloomberg Terminal, ProQuest). These datasets are not included and must be acquired separately.


## Repository Structure

- `bloomberg/`: Processing data from Bloomberg Terminal.
- `google/`: Interfacing with Google Trends API.
- `wiki/`: Interfacing with Wikipedia page access statistics API.
- `twitter/`: (Now suspended) Academic Twitter API scripts.
- `proquest/`: Processing newspaper data from ProQuest.
- `finbert/`: Python code for sentiment analysis using FinBERT.
- `stockdata/`: Processing price data.
- `dataset/`: Storage for parts of the dataset.
- `bind_data.R`: Merges attention, sentiment, and price variation datasets.
- `models/`: Model estimation and forecasting scripts.
- `tables/`: Scripts to generate tables for the paper.
- `figures/`: Scripts to generate figures for the paper.
- `shared_functions.R`: Utility/helper functions.
- `*.sh`: Shell scripts for batch processing.

## Requirements

- R >= 4.2.2
- R packages: [see scripts for required packages, loaded via `pacman::p_load()`]
- Python >= 3.7 (for FinBERT analysis)
- Python packages: `transformers`, `torch`, `pandas`, `gc`, `time`
- Access to proprietary data sources (Bloomberg/ProQuest/Twitter data as described)
- Access to cluster computing resources (if running shell scripts as written; otherwise, adapt paths as needed)

## Usage

1. **Prepare Data**: Acquire all necessary data and place it in the appropriate subfolders using the structure above.
2. **Edit Scripts**: Update any hardcoded paths to suit your environment. Most scripts assume execution from the project root and use relative paths.
3. **Run Analysis**: Follow the script order as outlined in the paper or project notes. See individual script headers for details.
4. **Outputs**: Tables and figures will be saved to `tables/` and `figures/` folders, respectively.

## Notes for Anonymous Review

- All personal identifiers and absolute paths have been removed or replaced with placeholders.
- If you encounter environment-specific code (e.g., cluster module loading), adapt as appropriate for your system.
- Please refer to comments in each script for further guidance.
