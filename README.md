# Forecasting U.S. equity market volatility with attention and sentiment to the economy

This repository contains code to acquire and process the data used in the paper, namely attention and sentiment variables from different sources and using different sentiment analysis methods, and to process high-frequency price variation variables. This repository also contains code to train models and predict the volatility of individual S&P 500 stocks, evaluate the results across all stocks in the sample, and code to create the resulting tables and graphs.

## Structure
### Data acquisition and processing
- <code>bloomberg</code>: Processing data acquired from the bloomberg terminal.
- <code>google</code>: Interfacing with the Google Trends API to collect and process data.
- <code>wiki</code>: Interfacing with the Wikipedia page access statistics API to collect and process data.
- <code>twitter</code>: Interfacing with the (now suspended) Academic Twitter API to collect and process data.
- <code>proquest</code>: Processing newspaper data accessed through the ProQuest database.
- <code>finbert</code>: A Python script for processing news articles and tweets into sentiment measures via FinBERT.
- <code>stockdata</code>: Processing price data purchased from the FirstRate Data provider. 
- <code>dataset</code>: A folder for storing parts of the dataset acquired in the previous steps.
- <code>bind_data.R</code>: Script for merging attention, sentiment and price variation datasets.

### Model estimation and forecasting
- <code>models</code>: Model estimation and forecasting for individual stocks for all specifications.
  
### Model evaluation, tables and figures
- <code>tables</code>: Creation of tables for the preliminary analysis (descriptive statistics and in sample models) and tables to summarize and evaluate out-of-sample results, including tables from the appendix and supplementary materials.
- <code>figures</code>: Creation of all graphs, including the figure in the appendix.

### Utils
- <code>shared_functions.R</code>: Utility script and helper functions used throughout the project.
- Parts of the project also use helper function scripts specific to that folder/part of the project.


## Requirements
This code was developed using R version 4.2.2. At the beginning of each script or a helper function utils script, all functions are loaded via the “pacman“ Package management tool, which installs packages if necessary:
```
# Load necessary libraries
# Install pacman, if needed
if (!require("pacman")) install.packages("pacman")
# Check whether these libraries are installed, install if not and load all of them
pacman::p_load(list of libraries)
```
