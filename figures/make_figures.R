# SCRIPT TO CREATE FIGURES IN THE PAPER


################### Load datasets, functions and define wds  ################### 

#  Save the current working directory
parent_wd <- getwd()

# Load shared functions
if (!require("pacman")) install.packages("pacman")
pacman::p_load(here)
source(here::here('shared_functions.R'))

# Source model functions from folder "./models"
source(here::here('models','model_functions.R'))

# Source tables functions from folder "./tables"
source(here::here('tables','tables_functions.R'))


# (For Table 1)
# Load dataset with stock price variables
data_wd <- "./stockdata/final/other_versions"
# Load data - list of dataframes with all variables - each dataframe for one stock
filename=paste0(data_wd,"/hf_market_5w_2.RData")
stocks.market <- loadRData(filename);gc()
# Select a stock
s=which(names(stocks.market)=="AAPL")
stock_dataset<-stocks.market[[s]]; rm(stocks.market); gc()

# (For Table 2)
#  Load forecast results
results<-readRDS("./models/results/results_5w_H1");gc()
# Load in a dictionary of model names
model_dict <- read_delim("./tables/model.dict.csv",delim = ";", escape_double = FALSE, trim_ws = TRUE)
model_dict <-data.frame(model_dict)

# (For Table A1)
# Load nams.csv from folder "./dataset"
nams = read.csv(file='./dataset/nams.csv');nams = data.frame(nams)
# Load dataset from folder "./dataset"
load(file='./dataset/dataset.RData')


# Working directory for figures
figures_wd <- "./figures"
setwd(figures_wd)

# Load figures functions
source('figures_functions.R')

# Working directory for saving figures
results_wd <- "./results"
# Create if it doesn't exist
create_wd(results_wd)


################ Figures as referred to in the paper ################

# Figures in the main body of the paper:
# Figure 1 - grid of time series plots of four log-transformed general attention measures 
#            and the log-transformed RV calculated from high-frequency price data of Apple Inc.
figure1 <- figure1_function(stock_dataset,VRS=c("gg.1","wg.1","tg.1","pg.1","VI.H1.Log"),
                          legend_text_size=14,base_size=18,title_size=18,l_m=0.1,hjust=0.5,vjust=-10,
                          margins=c(-0.75, 0.05,-0.75,0.05),legend.position="none",ma_n=22,
                          filename="figure1.jpeg",results_wd=results_wd,
                          parent_wd=parent_wd, figures_wd=figures_wd)



# Figure 2 - Distribution of average (over 404 stocks) % forecast improvements of 4 top-performing models
# How many extreme observations to remove?
ext.rem = 12 # approx 2%

# Select names of individual models + date and realized volatility
nms = c('Date','rv','har',"cslr_har_km5",'gen_dum',   # date, rv, benchmark models
        'gen_att','cslr_att_km5','lasso_att','rf_att',# attention models
        'gen_posneg_fin','cslr_posneg_fin_km5','lasso_posneg_fin','rf_posneg_fin') # sentiment models

# Select dataframes from list of results
selection=c("gen.att","att","gen.pos.fin.gen.neg.fin",
            "pos.fin.neg.fin","gen.dum","superbench")
figure2 = figure2_function(results, ext.rem = 12,perc=1,quan=c("top"),selection,model_dict,
                           nms,losses=c("MSE","QLIKE","MAE","MAPE"),filename="figure2.jpeg",
                           results_wd=results_wd,parent_wd, figures_wd=figures_wd)



# Figures in the Appendix:
# Figure A1 - Timing table - macronews-specific attention measures around announcements
figureA1 <- timing_figure(dataset=dataset,nams=nams,parent_wd=parent_wd,figures_wd=figures_wd, 
                         bloomberg_partial_wd="./bloomberg/partial results", 
                         timing_wd="./timing", vers="att",filename="figureA1.png",results_wd="./results")
