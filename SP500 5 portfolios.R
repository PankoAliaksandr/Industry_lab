# change
# Libraries
library(stringr)
library(lubridate)
library(xts)
library(PerformanceAnalytics)
library(psych)

# Read data container with filter
source( "D:/pclasses.R")
imp_path <- imp_path <- file.path("D:/Data Containers/SPCOMP")
conR <- dbFsConInit(imp_path)
dc5 <- dbConRead(conR,dcInit(id="SPCOMP_IsinIX_W",dbCon=conR))
sp500_filter = as.data.frame(dc5$data)

# Period from 2003-06-01 to 2018-05-31
sp500_filter$Date = as.Date(row.names(sp500_filter))
sp500_filter = sp500_filter[(sp500_filter$Date>="2003-06-01" & sp500_filter$Date<="2018-05-31"),]

# Read data container with stock returns
dc5 <- dbConRead(conR,dcInit(id="SPCOMP_RIAbsSimpR_W_USD",dbCon=conR))
sp500_returns = as.data.frame(dc5$data)
sp500_returns = sp500_returns / 100

# Period from 2003-06-01 to 2018-05-31
sp500_returns$Date = as.Date(row.names(sp500_returns))
sp500_returns = sp500_returns[(sp500_returns$Date>="2003-06-01" & sp500_returns$Date<="2018-05-31"),]

# Read data container with benchmark returns
dc5 <- dbConRead(conR,dcInit(id="SPCOMP_Benchmark_RIAbsSimpR_W_USD",dbCon=conR))
benchmark_returns = as.data.frame(dc5$data)
benchmark_returns = benchmark_returns / 100

# Period from 2003-06-01 to 2018-05-31
benchmark_returns$Date = as.Date(row.names(benchmark_returns))
benchmark_returns = benchmark_returns[(benchmark_returns$Date>="2003-06-01" & benchmark_returns$Date<="2018-05-31"),]
benchmark_returns = xts(benchmark_returns$Index, order.by = benchmark_returns$Date)

# Read data container with weights
dc5 <- dbConRead(conR,dcInit(id="SPCOMP_WeightedIsinIX_W",dbCon=conR))
sp500_weights = as.data.frame(dc5$data)

# Period from 2003-06-01 to 2018-05-31
sp500_weights$Date = as.Date(row.names(sp500_weights))
sp500_weights = sp500_weights[(sp500_weights$Date>="2003-06-01" & sp500_weights$Date<="2018-05-31"),]

# Read data container with size score
dc5 <- dbConRead(conR,dcInit(id="SPCOMP_Score_Size",dbCon=conR))
sp500_size_score = as.data.frame(dc5$data)

# Period from 2003-06-01 to 2018-05-31
sp500_size_score$Date = as.Date(row.names(sp500_size_score))
sp500_size_score = sp500_size_score[(sp500_size_score$Date>="2003-06-01" & sp500_size_score$Date<="2018-05-31"),]

# Read data container with book-to-price score
dc5 <- dbConRead(conR,dcInit(id="SPCOMP_Score_BTP",dbCon=conR))
sp500_btp_score = as.data.frame(dc5$data)

# Period from 2003-06-01 to 2018-05-31
sp500_btp_score$Date = as.Date(row.names(sp500_btp_score))
sp500_btp_score = sp500_btp_score[(sp500_btp_score$Date>="2003-06-01" & sp500_btp_score$Date<="2018-05-31"),]

# Read data container with momentum score
dc5 <- dbConRead(conR,dcInit(id="SPCOMP_Score_Momentum",dbCon=conR))
sp500_mom_score = as.data.frame(dc5$data)

# Period from 2003-06-01 to 2018-05-31
sp500_mom_score$Date = as.Date(row.names(sp500_mom_score))
sp500_mom_score = sp500_mom_score[(sp500_mom_score$Date>="2003-06-01" & sp500_mom_score$Date<="2018-05-31"),]

# Read monthly ESG data
esg_sp500_monthly = read.csv(file = "D:/SPCOMP_E_S_G_ESGCS_monthly.csv",header = TRUE, sep =";", dec = ",")

# Read monthly simple ESG data
simple_esg_sp500_monthly = read.csv(file = "D:/SPCOMP_ESG_monthly.csv",header = TRUE, sep =";", dec = ",")

# Colnames
esg_colnames = colnames(esg_sp500_monthly)
simple_esg_colnames = colnames(simple_esg_sp500_monthly)

esg_monthly_dates = as.Date(esg_sp500_monthly$Date, "%d.%m.%Y")[-1]
simple_esg_monthly_dates = as.Date(simple_esg_sp500_monthly$Date, "%d.%m.%Y")[-1]

# Column indices of different scores
sp500_ESGCS_ind = grep(pattern = "ESG.Combined.Score", x = esg_colnames)
sp500_E_ind = grep(pattern = "Environmental", x = esg_colnames)
sp500_S_ind = grep(pattern = "Social", x = esg_colnames)
sp500_G_ind = grep(pattern = "Corporate.Governance", x = esg_colnames)
sp500_ESG_ind = grep(pattern = "ESG.Score", x = simple_esg_colnames)

# Create dataframe with specific scores only
sp500_ESGCS_data = esg_sp500_monthly[,sp500_ESGCS_ind]
sp500_E_data = esg_sp500_monthly[,sp500_E_ind]
sp500_S_data = esg_sp500_monthly[,sp500_S_ind]
sp500_G_data = esg_sp500_monthly[,sp500_G_ind]
sp500_ESG_data = simple_esg_sp500_monthly[,sp500_ESG_ind]

# Convert values to the best suitable format
sp500_ESGCS_data[] = lapply(sp500_ESGCS_data, function(x) type.convert(as.character(x), as.is = TRUE))
sp500_E_data[] = lapply(sp500_E_data, function(x) type.convert(as.character(x), as.is = TRUE))
sp500_S_data[] = lapply(sp500_S_data, function(x) type.convert(as.character(x), as.is = TRUE))
sp500_G_data[] = lapply(sp500_G_data, function(x) type.convert(as.character(x), as.is = TRUE))
sp500_ESG_data[] = lapply(sp500_ESG_data, function(x) type.convert(as.character(x), as.is = TRUE))


# Set colnames
colnames(sp500_ESGCS_data) = sp500_ESGCS_data[1,]
colnames(sp500_E_data) = sp500_E_data[1,]
colnames(sp500_S_data) = sp500_S_data[1,]
colnames(sp500_G_data) = sp500_G_data[1,]
colnames(sp500_ESG_data) = sp500_ESG_data[1,]

# Delete the first row
sp500_ESGCS_data = sp500_ESGCS_data[-1,]
sp500_E_data = sp500_E_data[-1,]
sp500_S_data = sp500_S_data[-1,]
sp500_G_data = sp500_G_data[-1,]
sp500_ESG_data = sp500_ESG_data[-1,]

# Rownames
rownames(sp500_ESGCS_data) = NULL
rownames(sp500_E_data) = NULL
rownames(sp500_S_data) = NULL
rownames(sp500_G_data) = NULL
rownames(sp500_ESG_data) = NULL

# Make values numeric
sp500_ESGCS_data = as.data.frame(apply(apply(sp500_ESGCS_data, 2, gsub, patt=",", replace="."), 2, as.numeric))
sp500_E_data = as.data.frame(apply(apply(sp500_E_data, 2, gsub, patt=",", replace="."), 2, as.numeric))
sp500_S_data= as.data.frame(apply(apply(sp500_S_data, 2, gsub, patt=",", replace="."), 2, as.numeric))
sp500_G_data = as.data.frame(apply(apply(sp500_G_data, 2, gsub, patt=",", replace="."), 2, as.numeric))
sp500_ESG_data = as.data.frame(apply(apply(sp500_ESG_data, 2, gsub, patt=",", replace="."), 2, as.numeric))
sp500_equal_ESG_data = (1/3)*(sp500_E_data + sp500_S_data + sp500_G_data)

# Add dates to dataframes
sp500_ESGCS_data$Date = esg_monthly_dates
sp500_E_data$Date = esg_monthly_dates
sp500_S_data$Date = esg_monthly_dates
sp500_G_data$Date = esg_monthly_dates
sp500_ESG_data$Date = simple_esg_monthly_dates
sp500_equal_ESG_data$Date = esg_monthly_dates

# Period from 2002-12-01 to 2017-12-31
sp500_ESGCS_data = sp500_ESGCS_data[(sp500_ESGCS_data$Date>="2002-12-01" & sp500_ESGCS_data$Date<="2017-12-31"),]
sp500_E_data = sp500_E_data[(sp500_E_data$Date>="2002-12-01" & sp500_E_data$Date<="2017-12-31"),]
sp500_S_data = sp500_S_data[(sp500_S_data$Date>="2002-12-01" & sp500_S_data$Date<="2017-12-31"),]
sp500_G_data = sp500_G_data[(sp500_G_data$Date>="2002-12-01" & sp500_G_data$Date<="2017-12-31"),]
sp500_ESG_data = sp500_ESG_data[(sp500_ESG_data$Date>="2002-12-01" & sp500_ESG_data$Date<="2017-12-31"),]
sp500_equal_ESG_data = sp500_equal_ESG_data[(sp500_equal_ESG_data$Date>="2002-12-01" & sp500_equal_ESG_data$Date<="2017-12-31"),]

# We need only ESG data in December of every year
sp500_ESGCS_december_data = sp500_ESGCS_data[month(sp500_ESGCS_data$Date) == 12,]
sp500_E_december_data = sp500_E_data[month(sp500_E_data$Date) == 12,]
sp500_S_december_data = sp500_S_data[month(sp500_S_data$Date) == 12,]
sp500_G_december_data = sp500_G_data[month(sp500_G_data$Date) == 12,]
sp500_ESG_december_data = sp500_ESG_data[month(sp500_ESG_data$Date) == 12,]
sp500_equal_ESG_december_data = sp500_equal_ESG_data[month(sp500_equal_ESG_data$Date) == 12,]

# Colnames
sp500_ESGCS_december_data_colnames = colnames(sp500_ESGCS_december_data)
sp500_E_december_data_colnames = colnames(sp500_E_december_data)
sp500_S_december_data_colnames = colnames(sp500_S_december_data)
sp500_G_december_data_colnames = colnames(sp500_G_december_data)
sp500_ESG_december_data_colnames = colnames(sp500_ESG_december_data)
sp500_equal_ESG_december_data_colnames = colnames(sp500_equal_ESG_december_data)

# Create xts objects for portfolio returns and ESG scores
number_of_rows = length(sp500_returns$Date)
portfolio_names = c("Portfolio 1", "Portfolio 2", "Portfolio 3",
                    "Portfolio 4", "Portfolio 5")

extended_portfolio_names = c(portfolio_names, c("All best", "Benchmark"))

# ESGCS
ESGCS_portfolios_returns_df = data.frame(matrix(NA,nrow = number_of_rows, ncol = 7))
colnames(ESGCS_portfolios_returns_df) = extended_portfolio_names
ESGCS_portfolios_returns = as.xts(ESGCS_portfolios_returns_df, order.by = sp500_returns$Date)

ESGCS_average_scores_df = data.frame(matrix(NA,nrow = number_of_rows, ncol = 5))
colnames(ESGCS_average_scores_df) = portfolio_names
ESGCS_average_scores = as.xts(ESGCS_average_scores_df, order.by = sp500_returns$Date)

ESGCS_portfolios_weights_df = data.frame(matrix(NA,nrow = number_of_rows, ncol = 5))
colnames(ESGCS_portfolios_weights_df) = portfolio_names
ESGCS_portfolios_weights = as.xts(ESGCS_portfolios_weights_df, order.by = sp500_returns$Date)

ESGCS_average_size_score_df = data.frame(matrix(NA,nrow = number_of_rows, ncol = 5))
colnames(ESGCS_average_size_score_df) = portfolio_names
ESGCS_average_size_score = as.xts(ESGCS_average_size_score_df, order.by = sp500_returns$Date)

ESGCS_average_btp_score_df = data.frame(matrix(NA,nrow = number_of_rows, ncol = 5))
colnames(ESGCS_average_btp_score_df) = portfolio_names
ESGCS_average_btp_score = as.xts(ESGCS_average_btp_score_df, order.by = sp500_returns$Date)

ESGCS_missing_btp_score_df = data.frame(matrix(NA,nrow = number_of_rows, ncol = 5))
colnames(ESGCS_missing_btp_score_df) = portfolio_names
ESGCS_missing_btp_score = as.xts(ESGCS_missing_btp_score_df, order.by = sp500_returns$Date)

ESGCS_average_mom_score_df = data.frame(matrix(NA,nrow = number_of_rows, ncol = 5))
colnames(ESGCS_average_mom_score_df) = portfolio_names
ESGCS_average_mom_score = as.xts(ESGCS_average_mom_score_df, order.by = sp500_returns$Date)

ESGCS_missing_mom_score_df = data.frame(matrix(NA,nrow = number_of_rows, ncol = 5))
colnames(ESGCS_missing_mom_score_df) = portfolio_names
ESGCS_missing_mom_score = as.xts(ESGCS_missing_mom_score_df, order.by = sp500_returns$Date)

# E
E_portfolios_returns_df = data.frame(matrix(NA,nrow = number_of_rows, ncol = 7))
colnames(E_portfolios_returns_df) = extended_portfolio_names
E_portfolios_returns = as.xts(E_portfolios_returns_df, order.by = sp500_returns$Date)

E_average_scores_df = data.frame(matrix(NA,nrow = number_of_rows, ncol = 5))
colnames(E_average_scores_df) = portfolio_names
E_average_scores = as.xts(E_average_scores_df, order.by = sp500_returns$Date)

E_portfolios_weights_df = data.frame(matrix(NA,nrow = number_of_rows, ncol = 5))
colnames(E_portfolios_weights_df) = portfolio_names
E_portfolios_weights = as.xts(E_portfolios_weights_df, order.by = sp500_returns$Date)

E_average_size_score_df = data.frame(matrix(NA,nrow = number_of_rows, ncol = 5))
colnames(E_average_size_score_df) = portfolio_names
E_average_size_score = as.xts(E_average_size_score_df, order.by = sp500_returns$Date)

E_average_btp_score_df = data.frame(matrix(NA,nrow = number_of_rows, ncol = 5))
colnames(E_average_btp_score_df) = portfolio_names
E_average_btp_score = as.xts(E_average_btp_score_df, order.by = sp500_returns$Date)

E_missing_btp_score_df = data.frame(matrix(NA,nrow = number_of_rows, ncol = 5))
colnames(E_missing_btp_score_df) = portfolio_names
E_missing_btp_score = as.xts(E_missing_btp_score_df, order.by = sp500_returns$Date)

E_average_mom_score_df = data.frame(matrix(NA,nrow = number_of_rows, ncol = 5))
colnames(E_average_mom_score_df) = portfolio_names
E_average_mom_score = as.xts(E_average_mom_score_df, order.by = sp500_returns$Date)

E_missing_mom_score_df = data.frame(matrix(NA,nrow = number_of_rows, ncol = 5))
colnames(E_missing_mom_score_df) = portfolio_names
E_missing_mom_score = as.xts(E_missing_mom_score_df, order.by = sp500_returns$Date)

# S
S_portfolios_returns_df = data.frame(matrix(NA,nrow = number_of_rows, ncol = 7))
colnames(S_portfolios_returns_df) = extended_portfolio_names
S_portfolios_returns = as.xts(S_portfolios_returns_df, order.by = sp500_returns$Date)

S_average_scores_df = data.frame(matrix(NA,nrow = number_of_rows, ncol = 5))
colnames(S_average_scores_df) = portfolio_names
S_average_scores = as.xts(S_average_scores_df, order.by = sp500_returns$Date)

S_portfolios_weights_df = data.frame(matrix(NA,nrow = number_of_rows, ncol = 5))
colnames(S_portfolios_weights_df) = portfolio_names
S_portfolios_weights = as.xts(S_portfolios_weights_df, order.by = sp500_returns$Date)

S_average_size_score_df = data.frame(matrix(NA,nrow = number_of_rows, ncol = 5))
colnames(S_average_size_score_df) = portfolio_names
S_average_size_score = as.xts(S_average_size_score_df, order.by = sp500_returns$Date)

S_average_btp_score_df = data.frame(matrix(NA,nrow = number_of_rows, ncol = 5))
colnames(S_average_btp_score_df) = portfolio_names
S_average_btp_score = as.xts(S_average_btp_score_df, order.by = sp500_returns$Date)

S_missing_btp_score_df = data.frame(matrix(NA,nrow = number_of_rows, ncol = 5))
colnames(S_missing_btp_score_df) = portfolio_names
S_missing_btp_score = as.xts(S_missing_btp_score_df, order.by = sp500_returns$Date)

S_average_mom_score_df = data.frame(matrix(NA,nrow = number_of_rows, ncol = 5))
colnames(S_average_mom_score_df) = portfolio_names
S_average_mom_score = as.xts(S_average_mom_score_df, order.by = sp500_returns$Date)

S_missing_mom_score_df = data.frame(matrix(NA,nrow = number_of_rows, ncol = 5))
colnames(S_missing_mom_score_df) = portfolio_names
S_missing_mom_score = as.xts(S_missing_mom_score_df, order.by = sp500_returns$Date)

# G
G_portfolios_returns_df = data.frame(matrix(NA,nrow = number_of_rows, ncol = 7))
colnames(G_portfolios_returns_df) = extended_portfolio_names
G_portfolios_returns = as.xts(G_portfolios_returns_df, order.by = sp500_returns$Date)

G_average_scores_df = data.frame(matrix(NA,nrow = number_of_rows, ncol = 5))
colnames(G_average_scores_df) = portfolio_names
G_average_scores = as.xts(G_average_scores_df, order.by = sp500_returns$Date)

G_portfolios_weights_df = data.frame(matrix(NA,nrow = number_of_rows, ncol = 5))
colnames(G_portfolios_weights_df) = portfolio_names
G_portfolios_weights = as.xts(G_portfolios_weights_df, order.by = sp500_returns$Date)

G_average_size_score_df = data.frame(matrix(NA,nrow = number_of_rows, ncol = 5))
colnames(G_average_size_score_df) = portfolio_names
G_average_size_score = as.xts(G_average_size_score_df, order.by = sp500_returns$Date)

G_average_btp_score_df = data.frame(matrix(NA,nrow = number_of_rows, ncol = 5))
colnames(G_average_btp_score_df) = portfolio_names
G_average_btp_score = as.xts(G_average_btp_score_df, order.by = sp500_returns$Date)

G_missing_btp_score_df = data.frame(matrix(NA,nrow = number_of_rows, ncol = 5))
colnames(G_missing_btp_score_df) = portfolio_names
G_missing_btp_score = as.xts(G_missing_btp_score_df, order.by = sp500_returns$Date)

G_average_mom_score_df = data.frame(matrix(NA,nrow = number_of_rows, ncol = 5))
colnames(G_average_mom_score_df) = portfolio_names
G_average_mom_score = as.xts(G_average_mom_score_df, order.by = sp500_returns$Date)

G_missing_mom_score_df = data.frame(matrix(NA,nrow = number_of_rows, ncol = 5))
colnames(G_missing_mom_score_df) = portfolio_names
G_missing_mom_score = as.xts(G_missing_mom_score_df, order.by = sp500_returns$Date)

# ESG
ESG_portfolios_returns_df = data.frame(matrix(NA,nrow = number_of_rows, ncol = 7))
colnames(ESG_portfolios_returns_df) = extended_portfolio_names
ESG_portfolios_returns = as.xts(ESG_portfolios_returns_df, order.by = sp500_returns$Date)

ESG_average_scores_df = data.frame(matrix(NA,nrow = number_of_rows, ncol = 5))
colnames(ESG_average_scores_df) = portfolio_names
ESG_average_scores = as.xts(ESG_average_scores_df, order.by = sp500_returns$Date)

ESG_portfolios_weights_df = data.frame(matrix(NA,nrow = number_of_rows, ncol = 5))
colnames(ESG_portfolios_weights_df) = portfolio_names
ESG_portfolios_weights = as.xts(ESG_portfolios_weights_df, order.by = sp500_returns$Date)

ESG_average_size_score_df = data.frame(matrix(NA,nrow = number_of_rows, ncol = 5))
colnames(ESG_average_size_score_df) = portfolio_names
ESG_average_size_score = as.xts(ESG_average_size_score_df, order.by = sp500_returns$Date)

ESG_average_btp_score_df = data.frame(matrix(NA,nrow = number_of_rows, ncol = 5))
colnames(ESG_average_btp_score_df) = portfolio_names
ESG_average_btp_score = as.xts(ESG_average_btp_score_df, order.by = sp500_returns$Date)

ESG_missing_btp_score_df = data.frame(matrix(NA,nrow = number_of_rows, ncol = 5))
colnames(ESG_missing_btp_score_df) = portfolio_names
ESG_missing_btp_score = as.xts(ESG_missing_btp_score_df, order.by = sp500_returns$Date)

ESG_average_mom_score_df = data.frame(matrix(NA,nrow = number_of_rows, ncol = 5))
colnames(ESG_average_mom_score_df) = portfolio_names
ESG_average_mom_score = as.xts(ESG_average_mom_score_df, order.by = sp500_returns$Date)

ESG_missing_mom_score_df = data.frame(matrix(NA,nrow = number_of_rows, ncol = 5))
colnames(ESG_missing_mom_score_df) = portfolio_names
ESG_missing_mom_score = as.xts(ESG_missing_mom_score_df, order.by = sp500_returns$Date)


# ESG
equal_ESG_portfolios_returns_df = data.frame(matrix(NA,nrow = number_of_rows, ncol = 7))
colnames(equal_ESG_portfolios_returns_df) = extended_portfolio_names
equal_ESG_portfolios_returns = as.xts(equal_ESG_portfolios_returns_df, order.by = sp500_returns$Date)

equal_ESG_average_scores_df = data.frame(matrix(NA,nrow = number_of_rows, ncol = 5))
colnames(equal_ESG_average_scores_df) = portfolio_names
equal_ESG_average_scores = as.xts(equal_ESG_average_scores_df, order.by = sp500_returns$Date)

equal_ESG_portfolios_weights_df = data.frame(matrix(NA,nrow = number_of_rows, ncol = 5))
colnames(equal_ESG_portfolios_weights_df) = portfolio_names
equal_ESG_portfolios_weights = as.xts(equal_ESG_portfolios_weights_df, order.by = sp500_returns$Date)

equal_ESG_average_size_score_df = data.frame(matrix(NA,nrow = number_of_rows, ncol = 5))
colnames(equal_ESG_average_size_score_df) = portfolio_names
equal_ESG_average_size_score = as.xts(equal_ESG_average_size_score_df, order.by = sp500_returns$Date)

equal_ESG_average_btp_score_df = data.frame(matrix(NA,nrow = number_of_rows, ncol = 5))
colnames(equal_ESG_average_btp_score_df) = portfolio_names
equal_ESG_average_btp_score = as.xts(equal_ESG_average_btp_score_df, order.by = sp500_returns$Date)

equal_ESG_missing_btp_score_df = data.frame(matrix(NA,nrow = number_of_rows, ncol = 5))
colnames(equal_ESG_missing_btp_score_df) = portfolio_names
equal_ESG_missing_btp_score = as.xts(equal_ESG_missing_btp_score_df, order.by = sp500_returns$Date)

equal_ESG_average_mom_score_df = data.frame(matrix(NA,nrow = number_of_rows, ncol = 5))
colnames(equal_ESG_average_mom_score_df) = portfolio_names
equal_ESG_average_mom_score = as.xts(equal_ESG_average_mom_score_df, order.by = sp500_returns$Date)

equal_ESG_missing_mom_score_df = data.frame(matrix(NA,nrow = number_of_rows, ncol = 5))
colnames(equal_ESG_missing_mom_score_df) = portfolio_names
equal_ESG_missing_mom_score = as.xts(equal_ESG_missing_mom_score_df, order.by = sp500_returns$Date)

# All best stocks
all_best_stocks_number_df = data.frame(matrix(NA,nrow = number_of_rows, ncol = 1))
colnames(all_best_stocks_number_df) = "Number of stocks"
all_best_stocks_number = as.xts(all_best_stocks_number_df, order.by = sp500_returns$Date)

all_best_portfolio_returns_df = data.frame(matrix(NA,nrow = number_of_rows, ncol = 1))
colnames(all_best_portfolio_returns_df) = "Return"
all_best_portfolio_returns = as.xts(all_best_portfolio_returns_df, order.by = sp500_returns$Date)
 
# Statistics_df
annual_return_df = data.frame(matrix(NA,nrow = 5, ncol = 6))
annual_sd_df = data.frame(matrix(NA,nrow = 5, ncol = 6))
average_score_df = data.frame(matrix(NA,nrow = 5, ncol = 6))
average_portfolio_weight_df = data.frame(matrix(NA,nrow = 5, ncol = 6))
average_size_score_df = data.frame(matrix(NA,nrow = 5, ncol = 6))
average_btp_score_df = data.frame(matrix(NA,nrow = 5, ncol = 6))
average_mom_score_df = data.frame(matrix(NA,nrow = 5, ncol = 6))
beta_df = data.frame(matrix(NA,nrow = 5, ncol = 6))
bull_beta_df = data.frame(matrix(NA,nrow = 5, ncol = 6))
bear_beta_df = data.frame(matrix(NA,nrow = 5, ncol = 6))

# Set rownames
rownames(annual_return_df) = portfolio_names
rownames(annual_sd_df) = portfolio_names
rownames(average_score_df) = portfolio_names
rownames(average_portfolio_weight_df) = portfolio_names
rownames(average_size_score_df) = portfolio_names
rownames(average_btp_score_df) = portfolio_names
rownames(average_mom_score_df) = portfolio_names
rownames(beta_df) = portfolio_names
rownames(bull_beta_df) = portfolio_names
rownames(bear_beta_df) = portfolio_names

# Set colnames
score_names = c("ESGCS", "Environmental", "Social","Corporate.Governance", "ESG", "Equal.ESG")
colnames(annual_return_df) = score_names 
colnames(annual_sd_df) = score_names 
colnames(average_score_df) = score_names
colnames(average_portfolio_weight_df) = score_names
colnames(average_size_score_df) = score_names
colnames(average_btp_score_df) = score_names
colnames(average_mom_score_df) = score_names
colnames(beta_df) = score_names
colnames(bull_beta_df) = score_names
colnames(bear_beta_df) = score_names

# Loop from the year 2003 to the year 2017
week_number = 0
years = 2003:2017

for(i in 1:length(years)){
  
  year = years[i]
  previous_year = year - 1
  next_year = year + 1
  
  first_date_current_year = as.Date(paste0(year, "-06-01"))
  last_date_current_year = as.Date(paste0(next_year, "-05-31"))
  
  sp500_filter_current_year = sp500_filter[(sp500_filter$Date>=first_date_current_year & sp500_filter$Date<=last_date_current_year),]
  sp500_returns_current_year = sp500_returns[(sp500_returns$Date>=first_date_current_year & sp500_returns$Date<=last_date_current_year),]
  sp500_weights_current_year = sp500_weights[(sp500_weights$Date>=first_date_current_year & sp500_weights$Date<=last_date_current_year),]
  sp500_size_score_current_year = sp500_size_score[(sp500_size_score$Date>=first_date_current_year & sp500_size_score$Date<=last_date_current_year),]
  sp500_btp_score_current_year = sp500_btp_score[(sp500_btp_score$Date>=first_date_current_year & sp500_btp_score$Date<=last_date_current_year),]
  sp500_mom_score_current_year = sp500_mom_score[(sp500_mom_score$Date>=first_date_current_year & sp500_mom_score$Date<=last_date_current_year),]
  
  # Loop through every week of current year
  for (j in 1:nrow(sp500_returns_current_year)){
    
    # To observe how loop is running
    week_number = week_number + 1
    cat("Row" , week_number, "out of", nrow(sp500_returns), "\n")
    
    ESGCS_col_indices = c()
    E_col_indices = c()
    S_col_indices = c()
    G_col_indices = c()
    ESG_col_indices = c()
    equal_ESG_col_indices = c()
    
    returns_col_indices_for_ESGCS = c()
    returns_col_indices_for_E = c()
    returns_col_indices_for_S = c()
    returns_col_indices_for_G = c()
    returns_col_indices_for_ESG = c()
    returns_col_indices_for_equal_ESG = c()
    
    sp500_filter_current_week = sp500_filter_current_year[j,]
    sp500_returns_current_week = sp500_returns_current_year[j,]
    sp500_weights_current_week = sp500_weights_current_year[j,]
    sp500_size_score_current_week = sp500_size_score_current_year[j,]
    sp500_btp_score_current_week = sp500_btp_score_current_year[j,]
    sp500_mom_score_current_week = sp500_mom_score_current_year[j,]
    
    sp500_ret_cur_w_in_index = sp500_returns_current_week[which(sp500_filter_current_week == 1)]
    sp500_weights_cur_w_in_index = sp500_weights_current_week[which(sp500_filter_current_week == 1)]
    sp500_size_score_cur_w_in_index = sp500_size_score_current_week[which(sp500_filter_current_week == 1)]
    sp500_btp_score_cur_w_in_index = sp500_btp_score_current_week[which(sp500_filter_current_week == 1)]
    sp500_mom_score_cur_w_in_index = sp500_mom_score_current_week[which(sp500_filter_current_week == 1)]
    
    stock_codes = colnames(sp500_ret_cur_w_in_index)
    
    # Loop through all companies that are in index current week
    for(k in 1:length(stock_codes)){
      
      stock_code = stock_codes[k]
      
      # Column index with ESGCS 
      ESGCS_col_index = grep(pattern = stock_code, x = sp500_ESGCS_december_data_colnames)

      if(length(ESGCS_col_index)>0) {
        
        stock_ESGCS = sp500_ESGCS_december_data[i,ESGCS_col_index]
        
        # If this ESGCS exists
        if(is.na(stock_ESGCS)==FALSE){
          
          ESGCS_col_indices = c(ESGCS_col_indices, ESGCS_col_index)
          returns_col_indices_for_ESGCS = c(returns_col_indices_for_ESGCS, k)
          
        }
      }
      
      
      # Column index with E
      E_col_index = grep(pattern = stock_code, x = sp500_E_december_data_colnames)
      
      if(length(E_col_index)>0) {
        
        stock_E = sp500_E_december_data[i,E_col_index]
        
        # If this Environmental score exists
        if(is.na(stock_E)==FALSE){
          
          E_col_indices = c(E_col_indices, E_col_index)
          returns_col_indices_for_E = c(returns_col_indices_for_E, k)
          
        }
      }
      
      # Column index with S
      S_col_index = grep(pattern = stock_code, x = sp500_S_december_data_colnames)
      
      if(length(S_col_index)>0) {
        
        stock_S = sp500_S_december_data[i,S_col_index]
        
        # If this Socil score exists
        if(is.na(stock_S)==FALSE){
          
          S_col_indices = c(S_col_indices, S_col_index)
          returns_col_indices_for_S = c(returns_col_indices_for_S, k)
          
        }
      }
      
      # Column index with G
      G_col_index = grep(pattern = stock_code, x = sp500_G_december_data_colnames)
      
      if(length(G_col_index)>0) {
        
        stock_G = sp500_G_december_data[i,G_col_index]
        
        # If this Corporate Governance score exists
        if(is.na(stock_G)==FALSE){
          
          G_col_indices = c(G_col_indices, G_col_index)
          returns_col_indices_for_G = c(returns_col_indices_for_G, k)
          
        }
      }
      
      # Column index with ESG 
      ESG_col_index = grep(pattern = stock_code, x = sp500_ESG_december_data_colnames)
      
      if(length(ESG_col_index)>0) {
        
        stock_ESG = sp500_ESG_december_data[i,ESG_col_index]
        
        # If this ESG exists
        if(is.na(stock_ESG)==FALSE){
          
          ESG_col_indices = c(ESG_col_indices, ESG_col_index)
          returns_col_indices_for_ESG = c(returns_col_indices_for_ESG, k)
          
        }
      }
      
      # Column index with equal ESG 
      equal_ESG_col_index = grep(pattern = stock_code, x = sp500_equal_ESG_december_data_colnames)
      
      if(length(equal_ESG_col_index)>0) {
        
        stock_equal_ESG = sp500_equal_ESG_december_data[i,equal_ESG_col_index]
        
        # If this equal ESG exists
        if(is.na(stock_equal_ESG)==FALSE){
          
          equal_ESG_col_indices = c(equal_ESG_col_indices, equal_ESG_col_index)
          returns_col_indices_for_equal_ESG = c(returns_col_indices_for_equal_ESG, k)
          
        }
      }
    }
    
    #################################### ESGCS portfolios Start #####################################
    
    num_comp_in_index_with_ESGCS_cur_w = length(ESGCS_col_indices)
    
    analyzed_returns_for_ESGCS = sp500_ret_cur_w_in_index[returns_col_indices_for_ESGCS]
    analyzed_weights_for_ESGCS = sp500_weights_cur_w_in_index[returns_col_indices_for_ESGCS]
    analyzed_size_score_for_ESGCS = sp500_size_score_cur_w_in_index[returns_col_indices_for_ESGCS]
    analyzed_btp_score_for_ESGCS = sp500_btp_score_cur_w_in_index[returns_col_indices_for_ESGCS]
    analyzed_mom_score_for_ESGCS = sp500_mom_score_cur_w_in_index[returns_col_indices_for_ESGCS]
    analyzed_ESGCS = unlist(sp500_ESGCS_december_data[i,ESGCS_col_indices])
    
    number_of_companies_in_ESGCS_portfolio_current_week = num_comp_in_index_with_ESGCS_cur_w %/% 5
    
    for(l in 1:(ncol(ESGCS_portfolios_returns)-2)){
      
      # Find constituents of portfolio 
      portfolio_ESGCS_indices = which(analyzed_ESGCS <= sort(analyzed_ESGCS, decreasing=F)[number_of_companies_in_ESGCS_portfolio_current_week], arr.ind=TRUE)[1:number_of_companies_in_ESGCS_portfolio_current_week]
      
      # Find ESGCS values
      ESGCS_values = analyzed_ESGCS[portfolio_ESGCS_indices]
      
      # Calculate average ESGCS in portfolio current week
      ESGCS_average_scores[week_number,l] = mean(ESGCS_values)
      
      # Find returns of stocks in portfolio 
      portfolio_ESGCS_stock_returns = unlist(analyzed_returns_for_ESGCS[portfolio_ESGCS_indices])
      
      # Find weights of companies in portfolio 
      portfolio_ESGCS_company_weights = unlist(analyzed_weights_for_ESGCS[portfolio_ESGCS_indices])
      
      # Find size score of companies in portfolio 
      portfolio_ESGCS_company_size_score = unlist(analyzed_size_score_for_ESGCS[portfolio_ESGCS_indices])
      
      # Find btp score of companies in portfolio 
      portfolio_ESGCS_company_btp_score = unlist(analyzed_btp_score_for_ESGCS[portfolio_ESGCS_indices])
      
      # Find momentum score of companies in portfolio 
      portfolio_ESGCS_company_mom_score = unlist(analyzed_mom_score_for_ESGCS[portfolio_ESGCS_indices])

      # Calculate portfolo return current week
      ESGCS_portfolios_returns[week_number,l] = mean(portfolio_ESGCS_stock_returns)
      
      # Calculate portfolo weight current week
      ESGCS_portfolios_weights[week_number,l] = sum(portfolio_ESGCS_company_weights)
      
      # Calculate portfolo size score current week
      ESGCS_average_size_score[week_number,l] = mean(portfolio_ESGCS_company_size_score)
      
      # Calculate portfolo btp score current week
      ESGCS_average_btp_score[week_number,l] = mean(portfolio_ESGCS_company_btp_score, na.rm = TRUE)
      
      # Calculate portfolo momentum score current week
      ESGCS_average_mom_score[week_number,l] = mean(portfolio_ESGCS_company_mom_score, na.rm = TRUE)
      
      # Missing values
      ESGCS_missing_btp_score[week_number,l] = 100 * sum(is.na(portfolio_ESGCS_company_btp_score)) / number_of_companies_in_ESGCS_portfolio_current_week
      ESGCS_missing_mom_score[week_number,l] = 100 * sum(is.na(portfolio_ESGCS_company_mom_score)) / number_of_companies_in_ESGCS_portfolio_current_week
      
      # Replace ESGCS in portfolio with very big number
      analyzed_ESGCS[portfolio_ESGCS_indices] = 1000000
      
    }
    #################################### ESGCS portfolios End #####################################
    
    #################################### Environmental portfolios Start #####################################
    
    num_comp_in_index_with_E_cur_w = length(E_col_indices)
    
    analyzed_returns_for_E = sp500_ret_cur_w_in_index[returns_col_indices_for_E]
    analyzed_weights_for_E = sp500_weights_cur_w_in_index[returns_col_indices_for_E]
    analyzed_size_score_for_E = sp500_size_score_cur_w_in_index[returns_col_indices_for_E]
    analyzed_btp_score_for_E = sp500_btp_score_cur_w_in_index[returns_col_indices_for_E]
    analyzed_mom_score_for_E = sp500_mom_score_cur_w_in_index[returns_col_indices_for_E]
    analyzed_E = unlist(sp500_E_december_data[i,E_col_indices])
    
    number_of_companies_in_E_portfolio_current_week = num_comp_in_index_with_E_cur_w %/% 5
    best_E_companies_current_week = c()
    best_S_companies_current_week = c()
    best_G_companies_current_week = c()
    
    for(l in 1:(ncol(E_portfolios_returns)-2)){
      
      # Find constituents of portfolio 
      portfolio_E_indices = which(analyzed_E <= sort(analyzed_E, decreasing=F)[number_of_companies_in_E_portfolio_current_week], arr.ind=TRUE)[1:number_of_companies_in_E_portfolio_current_week]
      
      # Find E values
      E_values = analyzed_E[portfolio_E_indices]
      
      # Find best E stocks
      if(l==5){
        best_E_stocks_current_week = names(analyzed_E)[portfolio_E_indices]
      }
      
      # Calculate average E in portfolio current week
      E_average_scores[week_number,l] = mean(E_values)
      
      # Find returns of stocks in portfolio 
      portfolio_E_stock_returns = unlist(analyzed_returns_for_E[portfolio_E_indices])
      
      # Find weights of companies in portfolio 
      portfolio_E_company_weights = unlist(analyzed_weights_for_E[portfolio_E_indices])
      
      # Find size score of companies in portfolio 
      portfolio_E_company_size_score = unlist(analyzed_size_score_for_E[portfolio_E_indices])
      
      # Find btp score of companies in portfolio 
      portfolio_E_company_btp_score = unlist(analyzed_btp_score_for_E[portfolio_E_indices])
      
      # Find momentum score of companies in portfolio 
      portfolio_E_company_mom_score = unlist(analyzed_mom_score_for_E[portfolio_E_indices])
      
      # Calculate portfolo return current week
      E_portfolios_returns[week_number,l] = mean(portfolio_E_stock_returns)
      
      # Calculate portfolo weight current week
      E_portfolios_weights[week_number,l] = sum(portfolio_E_company_weights)
      
      # Calculate portfolo size score current week
      E_average_size_score[week_number,l] = mean(portfolio_E_company_size_score)
      
      # Calculate portfolo btp score current week
      E_average_btp_score[week_number,l] = mean(portfolio_E_company_btp_score, na.rm = TRUE)
      
      # Calculate portfolo momentum score current week
      E_average_mom_score[week_number,l] = mean(portfolio_E_company_mom_score, na.rm = TRUE)
      
      # Missing values
      E_missing_btp_score[week_number,l] = 100 * sum(is.na(portfolio_E_company_btp_score)) / number_of_companies_in_E_portfolio_current_week
      E_missing_mom_score[week_number,l] = 100 * sum(is.na(portfolio_E_company_mom_score)) / number_of_companies_in_E_portfolio_current_week
      
      # Replace Environmental score in portfolio with very big number
      analyzed_E[portfolio_E_indices] = 1000000
      
    }
    #################################### Environmental portfolios End #####################################
    
    
    #################################### Social portfolios Start #####################################
    
    num_comp_in_index_with_S_cur_w = length(S_col_indices)
    
    analyzed_returns_for_S = sp500_ret_cur_w_in_index[returns_col_indices_for_S]
    analyzed_weights_for_S = sp500_weights_cur_w_in_index[returns_col_indices_for_S]
    analyzed_size_score_for_S = sp500_size_score_cur_w_in_index[returns_col_indices_for_S]
    analyzed_btp_score_for_S = sp500_btp_score_cur_w_in_index[returns_col_indices_for_S]
    analyzed_mom_score_for_S = sp500_mom_score_cur_w_in_index[returns_col_indices_for_S]
    analyzed_S = unlist(sp500_S_december_data[i,S_col_indices])
    
    number_of_companies_in_S_portfolio_current_week = num_comp_in_index_with_S_cur_w %/% 5
    
    for(l in 1:(ncol(S_portfolios_returns)-2)){
      
      # Find constituents of portfolio 
      portfolio_S_indices = which(analyzed_S <= sort(analyzed_S, decreasing=F)[number_of_companies_in_S_portfolio_current_week], arr.ind=TRUE)[1:number_of_companies_in_S_portfolio_current_week]
      
      # Find S values
      S_values = analyzed_S[portfolio_S_indices]
      
      # Find best S stocks
      if(l==5){
        best_S_stocks_current_week = names(analyzed_S)[portfolio_S_indices]
      }
      
      # Calculate average S in portfolio current week
      S_average_scores[week_number,l] = mean(S_values)
      
      # Find returns of stocks in portfolio 
      portfolio_S_stock_returns = unlist(analyzed_returns_for_S[portfolio_S_indices])
      
      # Find weights of companies in portfolio 
      portfolio_S_company_weights = unlist(analyzed_weights_for_S[portfolio_S_indices])
      
      # Find size score of companies in portfolio 
      portfolio_S_company_size_score = unlist(analyzed_size_score_for_S[portfolio_S_indices])
      
      # Find btp score of companies in portfolio 
      portfolio_S_company_btp_score = unlist(analyzed_btp_score_for_S[portfolio_S_indices])
      
      # Find momentum score of companies in portfolio 
      portfolio_S_company_mom_score = unlist(analyzed_mom_score_for_S[portfolio_S_indices])
      
      # Calculate portfolo return current week
      S_portfolios_returns[week_number,l] = mean(portfolio_S_stock_returns)
      
      # Calculate portfolo weight current week
      S_portfolios_weights[week_number,l] = sum(portfolio_S_company_weights)
      
      # Calculate portfolo size score current week
      S_average_size_score[week_number,l] = mean(portfolio_S_company_size_score)
      
      # Calculate portfolo btp score current week
      S_average_btp_score[week_number,l] = mean(portfolio_S_company_btp_score, na.rm = TRUE)
      
      # Calculate portfolo momentum score current week
      S_average_mom_score[week_number,l] = mean(portfolio_S_company_mom_score, na.rm = TRUE)
      
      # Missing values
      S_missing_btp_score[week_number,l] = 100 * sum(is.na(portfolio_S_company_btp_score)) / number_of_companies_in_S_portfolio_current_week
      S_missing_mom_score[week_number,l] = 100 * sum(is.na(portfolio_S_company_mom_score)) / number_of_companies_in_S_portfolio_current_week
      
      # Replace Social score in portfolio with very big number
      analyzed_S[portfolio_S_indices] = 1000000
      
    }
    #################################### Social portfolios End #####################################
    
    
    #################################### Corporate Governance Start #####################################
    
    num_comp_in_index_with_G_cur_w = length(G_col_indices)
    
    analyzed_returns_for_G = sp500_ret_cur_w_in_index[returns_col_indices_for_G]
    analyzed_weights_for_G = sp500_weights_cur_w_in_index[returns_col_indices_for_G]
    analyzed_size_score_for_G = sp500_size_score_cur_w_in_index[returns_col_indices_for_G]
    analyzed_btp_score_for_G = sp500_btp_score_cur_w_in_index[returns_col_indices_for_G]
    analyzed_mom_score_for_G = sp500_mom_score_cur_w_in_index[returns_col_indices_for_G]
    analyzed_G = unlist(sp500_G_december_data[i,G_col_indices])
    
    number_of_companies_in_G_portfolio_current_week = num_comp_in_index_with_G_cur_w %/% 5
    
    for(l in 1:(ncol(G_portfolios_returns)-2)){
      
      # Find constituents of portfolio 
      portfolio_G_indices = which(analyzed_G <= sort(analyzed_G, decreasing=F)[number_of_companies_in_G_portfolio_current_week], arr.ind=TRUE)[1:number_of_companies_in_G_portfolio_current_week]
      
      # Find G values
      G_values = analyzed_G[portfolio_G_indices]
      
      # Find best G stocks
      if(l==5){
        best_G_stocks_current_week = names(analyzed_G)[portfolio_G_indices]
      }
      
      # Calculate average G in portfolio current week
      G_average_scores[week_number,l] = mean(G_values)
      
      # Find returns of stocks in portfolio 
      portfolio_G_stock_returns = unlist(analyzed_returns_for_G[portfolio_G_indices])
      
      # Find weights of companies in portfolio 
      portfolio_G_company_weights = unlist(analyzed_weights_for_G[portfolio_G_indices])
      
      # Find size score of companies in portfolio 
      portfolio_G_company_size_score = unlist(analyzed_size_score_for_G[portfolio_G_indices])
      
      # Find btp score of companies in portfolio 
      portfolio_G_company_btp_score = unlist(analyzed_btp_score_for_G[portfolio_G_indices])
      
      # Find momentum score of companies in portfolio 
      portfolio_G_company_mom_score = unlist(analyzed_mom_score_for_G[portfolio_G_indices])
      
      # Calculate portfolo return current week
      G_portfolios_returns[week_number,l] = mean(portfolio_G_stock_returns)
      
      # Calculate portfolo weight current week
      G_portfolios_weights[week_number,l] = sum(portfolio_G_company_weights)
      
      # Calculate portfolo size score current week
      G_average_size_score[week_number,l] = mean(portfolio_G_company_size_score)
      
      # Calculate portfolo btp score current week
      G_average_btp_score[week_number,l] = mean(portfolio_G_company_btp_score, na.rm = TRUE)
      
      # Calculate portfolo momentum score current week
      G_average_mom_score[week_number,l] = mean(portfolio_G_company_mom_score, na.rm = TRUE)
      
      # Missing values
      G_missing_btp_score[week_number,l] = 100 * sum(is.na(portfolio_G_company_btp_score)) / number_of_companies_in_G_portfolio_current_week
      G_missing_mom_score[week_number,l] = 100 * sum(is.na(portfolio_G_company_mom_score)) / number_of_companies_in_G_portfolio_current_week
      
      # Replace Corporate Governance score in portfolio with very big number
      analyzed_G[portfolio_G_indices] = 1000000
      
    }
    #################################### Corporate Governance portfolios End #####################################
 
    # Find number of all best stocks
    all_best_stocks = Reduce(intersect, list(best_E_stocks_current_week, best_S_stocks_current_week, best_G_stocks_current_week))
    all_best_stocks_number[week_number,"Number of stocks"] = length(all_best_stocks)
    
    # Find returns of all best stocks
    all_best_stocks_indices = unique (grep(paste(all_best_stocks,collapse="|"), stock_codes))
    all_best_stocks_returns = sp500_ret_cur_w_in_index[all_best_stocks_indices]
    all_best_portfolio_returns[week_number,"Return"] = mean(all_best_stocks_returns)
    
    
    #################################### ESG portfolios Start #####################################
    
    num_comp_in_index_with_ESG_cur_w = length(ESG_col_indices)
    
    analyzed_returns_for_ESG = sp500_ret_cur_w_in_index[returns_col_indices_for_ESG]
    analyzed_weights_for_ESG = sp500_weights_cur_w_in_index[returns_col_indices_for_ESG]
    analyzed_size_score_for_ESG = sp500_size_score_cur_w_in_index[returns_col_indices_for_ESG]
    analyzed_btp_score_for_ESG = sp500_btp_score_cur_w_in_index[returns_col_indices_for_ESG]
    analyzed_mom_score_for_ESG = sp500_mom_score_cur_w_in_index[returns_col_indices_for_ESG]
    analyzed_ESG = unlist(sp500_ESG_december_data[i,ESG_col_indices])
    
    number_of_companies_in_ESG_portfolio_current_week = num_comp_in_index_with_ESG_cur_w %/% 5
    
    for(l in 1:(ncol(ESG_portfolios_returns)-2)){
      
      # Find constituents of portfolio 
      portfolio_ESG_indices = which(analyzed_ESG <= sort(analyzed_ESG, decreasing=F)[number_of_companies_in_ESG_portfolio_current_week], arr.ind=TRUE)[1:number_of_companies_in_ESG_portfolio_current_week]
      
      # Find ESG values
      ESG_values = analyzed_ESG[portfolio_ESG_indices]
      
      # Calculate average ESG in portfolio current week
      ESG_average_scores[week_number,l] = mean(ESG_values)
      
      # Find returns of stocks in portfolio 
      portfolio_ESG_stock_returns = unlist(analyzed_returns_for_ESG[portfolio_ESG_indices])
      
      # Find weights of companies in portfolio 
      portfolio_ESG_company_weights = unlist(analyzed_weights_for_ESG[portfolio_ESG_indices])
      
      # Find size score of companies in portfolio 
      portfolio_ESG_company_size_score = unlist(analyzed_size_score_for_ESG[portfolio_ESG_indices])
      
      # Find btp score of companies in portfolio 
      portfolio_ESG_company_btp_score = unlist(analyzed_btp_score_for_ESG[portfolio_ESG_indices])
      
      # Find momentum score of companies in portfolio 
      portfolio_ESG_company_mom_score = unlist(analyzed_mom_score_for_ESG[portfolio_ESG_indices])
      
      # Calculate portfolo return current week
      ESG_portfolios_returns[week_number,l] = mean(portfolio_ESG_stock_returns)
      
      # Calculate portfolo weight current week
      ESG_portfolios_weights[week_number,l] = sum(portfolio_ESG_company_weights)
      
      # Calculate portfolo size score current week
      ESG_average_size_score[week_number,l] = mean(portfolio_ESG_company_size_score)
      
      # Calculate portfolo btp score current week
      ESG_average_btp_score[week_number,l] = mean(portfolio_ESG_company_btp_score, na.rm = TRUE)
      
      # Calculate portfolo momentum score current week
      ESG_average_mom_score[week_number,l] = mean(portfolio_ESG_company_mom_score, na.rm = TRUE)
      
      # Missing values
      ESG_missing_btp_score[week_number,l] = 100 * sum(is.na(portfolio_ESG_company_btp_score)) / number_of_companies_in_ESG_portfolio_current_week
      ESG_missing_mom_score[week_number,l] = 100 * sum(is.na(portfolio_ESG_company_mom_score)) / number_of_companies_in_ESG_portfolio_current_week
      
      # Replace ESG in portfolio with very big number
      analyzed_ESG[portfolio_ESG_indices] = 1000000
      
    }
    #################################### ESG portfolios End #####################################
    
    #################################### Equal ESG portfolios Start #####################################
    
    num_comp_in_index_with_equal_ESG_cur_w = length(equal_ESG_col_indices)
    
    analyzed_returns_for_equal_ESG = sp500_ret_cur_w_in_index[returns_col_indices_for_equal_ESG]
    analyzed_weights_for_equal_ESG = sp500_weights_cur_w_in_index[returns_col_indices_for_equal_ESG]
    analyzed_size_score_for_equal_ESG = sp500_size_score_cur_w_in_index[returns_col_indices_for_equal_ESG]
    analyzed_btp_score_for_equal_ESG = sp500_btp_score_cur_w_in_index[returns_col_indices_for_equal_ESG]
    analyzed_mom_score_for_equal_ESG = sp500_mom_score_cur_w_in_index[returns_col_indices_for_equal_ESG]
    analyzed_equal_ESG = unlist(sp500_equal_ESG_december_data[i,equal_ESG_col_indices])
    
    number_of_companies_in_equal_ESG_portfolio_current_week = num_comp_in_index_with_equal_ESG_cur_w %/% 5
    
    for(l in 1:(ncol(equal_ESG_portfolios_returns)-2)){
      
      # Find constituents of portfolio 
      portfolio_equal_ESG_indices = which(analyzed_equal_ESG <= sort(analyzed_equal_ESG, decreasing=F)[number_of_companies_in_equal_ESG_portfolio_current_week], arr.ind=TRUE)[1:number_of_companies_in_equal_ESG_portfolio_current_week]
      
      # Find Equal ESG values
      equal_ESG_values = analyzed_equal_ESG[portfolio_equal_ESG_indices]
      
      # Calculate average Equal ESG in portfolio current week
      equal_ESG_average_scores[week_number,l] = mean(equal_ESG_values)
      
      # Find returns of stocks in portfolio 
      portfolio_equal_ESG_stock_returns = unlist(analyzed_returns_for_equal_ESG[portfolio_equal_ESG_indices])
      
      # Find weights of companies in portfolio 
      portfolio_equal_ESG_company_weights = unlist(analyzed_weights_for_equal_ESG[portfolio_equal_ESG_indices])
      
      # Find size score of companies in portfolio 
      portfolio_equal_ESG_company_size_score = unlist(analyzed_size_score_for_equal_ESG[portfolio_equal_ESG_indices])
      
      # Find btp score of companies in portfolio 
      portfolio_equal_ESG_company_btp_score = unlist(analyzed_btp_score_for_equal_ESG[portfolio_equal_ESG_indices])
      
      # Find momentum score of companies in portfolio 
      portfolio_equal_ESG_company_mom_score = unlist(analyzed_mom_score_for_equal_ESG[portfolio_equal_ESG_indices])
      
      # Calculate portfolo return current week
      equal_ESG_portfolios_returns[week_number,l] = mean(portfolio_equal_ESG_stock_returns)
      
      # Calculate portfolo weight current week
      equal_ESG_portfolios_weights[week_number,l] = sum(portfolio_equal_ESG_company_weights)
      
      # Calculate portfolo size score current week
      equal_ESG_average_size_score[week_number,l] = mean(portfolio_equal_ESG_company_size_score)
      
      # Calculate portfolo btp score current week
      equal_ESG_average_btp_score[week_number,l] = mean(portfolio_equal_ESG_company_btp_score, na.rm = TRUE)
      
      # Calculate portfolo momentum score current week
      equal_ESG_average_mom_score[week_number,l] = mean(portfolio_equal_ESG_company_mom_score, na.rm = TRUE)
      
      # Missing values
      equal_ESG_missing_btp_score[week_number,l] = 100 * sum(is.na(portfolio_equal_ESG_company_btp_score)) / number_of_companies_in_equal_ESG_portfolio_current_week
      equal_ESG_missing_mom_score[week_number,l] = 100 * sum(is.na(portfolio_equal_ESG_company_mom_score)) / number_of_companies_in_equal_ESG_portfolio_current_week
      
      # Replace Equal ESG in portfolio with very big number
      analyzed_equal_ESG[portfolio_equal_ESG_indices] = 1000000
      
    }
    #################################### Equal ESG portfolios End #####################################
    
    
  }
}


# Add All Best Portfolio returns
ESGCS_portfolios_returns$`All best` = all_best_portfolio_returns
E_portfolios_returns$`All best` = all_best_portfolio_returns
S_portfolios_returns$`All best` = all_best_portfolio_returns
G_portfolios_returns$`All best` = all_best_portfolio_returns
ESG_portfolios_returns$`All best` = all_best_portfolio_returns
equal_ESG_portfolios_returns$`All best` = all_best_portfolio_returns


# Add Benchmark returns
ESGCS_portfolios_returns$Benchmark = benchmark_returns
E_portfolios_returns$Benchmark = benchmark_returns
S_portfolios_returns$Benchmark = benchmark_returns
G_portfolios_returns$Benchmark = benchmark_returns
ESG_portfolios_returns$Benchmark = benchmark_returns
equal_ESG_portfolios_returns$Benchmark = benchmark_returns


# Statistics

# Benchmark
benchmark_annual_return = round(mean(benchmark_returns) * 5200, 2)
benchmark_annual_sd = round(sd(benchmark_returns) * sqrt(52) * 100, 2)

# All best
all_best_portfolio_annual_return = round(mean(all_best_portfolio_returns) * 5200, 2)
all_best_portfolio_annual_sd = round(sd(all_best_portfolio_returns) * sqrt(52) * 100, 2)


# ESGCS
annual_return_df$ESGCS = round(colMeans(ESGCS_portfolios_returns[,1:5]) * 5200, 2)
annual_sd_df$ESGCS = round(unlist(lapply(ESGCS_portfolios_returns[,1:5], sd)) * sqrt(52) * 100, 2)
average_score_df$ESGCS = round(colMeans(ESGCS_average_scores), 2)
average_portfolio_weight_df$ESGCS = round(colMeans(ESGCS_portfolios_weights), 2)
average_size_score_df$ESGCS = round(colMeans(ESGCS_average_size_score), 2)
average_btp_score_df$ESGCS = round(colMeans(ESGCS_average_btp_score), 2)
average_mom_score_df$ESGCS = round(colMeans(ESGCS_average_mom_score), 2)
beta_df$ESGCS = round(t(CAPM.beta(ESGCS_portfolios_returns[,1:5], benchmark_returns, Rf = 0)), 2)
bull_beta_df$ESGCS = round(t(CAPM.beta.bull(ESGCS_portfolios_returns[,1:5], benchmark_returns, Rf = 0)), 2)
bear_beta_df$ESGCS = round(t(CAPM.beta.bear(ESGCS_portfolios_returns[,1:5], benchmark_returns, Rf = 0)), 2)

# Environmental
annual_return_df$Environmental = round(colMeans(E_portfolios_returns[,1:5]) * 5200, 2)
annual_sd_df$Environmental = round(unlist(lapply(E_portfolios_returns[,1:5], sd)) * sqrt(52) * 100, 2)
average_score_df$Environmental = round(colMeans(E_average_scores), 2)
average_portfolio_weight_df$Environmental = round(colMeans(E_portfolios_weights), 2)
average_size_score_df$Environmental = round(colMeans(E_average_size_score), 2)
average_btp_score_df$Environmental = round(colMeans(E_average_btp_score), 2)
average_mom_score_df$Environmental = round(colMeans(E_average_mom_score), 2)
beta_df$Environmental = round(t(CAPM.beta(E_portfolios_returns[,1:5], benchmark_returns, Rf = 0)), 2)
bull_beta_df$Environmental = round(t(CAPM.beta.bull(E_portfolios_returns[,1:5], benchmark_returns, Rf = 0)), 2)
bear_beta_df$Environmental = round(t(CAPM.beta.bear(E_portfolios_returns[,1:5], benchmark_returns, Rf = 0)), 2)

# Social
annual_return_df$Social = round(colMeans(S_portfolios_returns[,1:5]) * 5200, 2)
annual_sd_df$Social = round(unlist(lapply(S_portfolios_returns[,1:5], sd)) * sqrt(52) * 100, 2)
average_score_df$Social = round(colMeans(S_average_scores), 2)
average_portfolio_weight_df$Social = round(colMeans(S_portfolios_weights), 2)
average_size_score_df$Social= round(colMeans(S_average_size_score), 2)
average_btp_score_df$Social = round(colMeans(S_average_btp_score), 2)
average_mom_score_df$Social = round(colMeans(S_average_mom_score), 2)
beta_df$Social = round(t(CAPM.beta(S_portfolios_returns[,1:5], benchmark_returns, Rf = 0)), 2)
bull_beta_df$Social = round(t(CAPM.beta.bull(S_portfolios_returns[,1:5], benchmark_returns, Rf = 0)), 2)
bear_beta_df$Social = round(t(CAPM.beta.bear(S_portfolios_returns[,1:5], benchmark_returns, Rf = 0)), 2)

# Corporate Governance
annual_return_df$Corporate.Governance = round(colMeans(G_portfolios_returns[,1:5]) * 5200, 2)
annual_sd_df$Corporate.Governance = round(unlist(lapply(G_portfolios_returns[,1:5], sd)) * sqrt(52) * 100, 2)
average_score_df$Corporate.Governance = round(colMeans(G_average_scores), 2)
average_portfolio_weight_df$Corporate.Governance = round(colMeans(G_portfolios_weights), 2)
average_size_score_df$Corporate.Governance= round(colMeans(G_average_size_score), 2)
average_btp_score_df$Corporate.Governance = round(colMeans(G_average_btp_score), 2)
average_mom_score_df$Corporate.Governance = round(colMeans(G_average_mom_score), 2)
beta_df$Corporate.Governance = round(t(CAPM.beta(G_portfolios_returns[,1:5], benchmark_returns, Rf = 0)), 2)
bull_beta_df$Corporate.Governance = round(t(CAPM.beta.bull(G_portfolios_returns[,1:5], benchmark_returns, Rf = 0)), 2)
bear_beta_df$Corporate.Governance = round(t(CAPM.beta.bear(G_portfolios_returns[,1:5], benchmark_returns, Rf = 0)), 2)

# ESG
annual_return_df$ESG = round(colMeans(ESG_portfolios_returns[,1:5]) * 5200, 2)
annual_sd_df$ESG = round(unlist(lapply(ESG_portfolios_returns[,1:5], sd)) * sqrt(52) * 100, 2)
average_score_df$ESG = round(colMeans(ESG_average_scores), 2)
average_portfolio_weight_df$ESG = round(colMeans(ESG_portfolios_weights), 2)
average_size_score_df$ESG = round(colMeans(ESG_average_size_score), 2)
average_btp_score_df$ESG = round(colMeans(ESG_average_btp_score), 2)
average_mom_score_df$ESG = round(colMeans(ESG_average_mom_score), 2)
beta_df$ESG = round(t(CAPM.beta(ESG_portfolios_returns[,1:5], benchmark_returns, Rf = 0)), 2)
bull_beta_df$ESG = round(t(CAPM.beta.bull(ESG_portfolios_returns[,1:5], benchmark_returns, Rf = 0)), 2)
bear_beta_df$ESG = round(t(CAPM.beta.bear(ESG_portfolios_returns[,1:5], benchmark_returns, Rf = 0)), 2)

# Equal ESG
annual_return_df$Equal.ESG = round(colMeans(equal_ESG_portfolios_returns[,1:5]) * 5200, 2)
annual_sd_df$Equal.ESG = round(unlist(lapply(equal_ESG_portfolios_returns[,1:5], sd)) * sqrt(52) * 100, 2)
average_score_df$Equal.ESG = round(colMeans(equal_ESG_average_scores), 2)
average_portfolio_weight_df$Equal.ESG = round(colMeans(equal_ESG_portfolios_weights), 2)
average_size_score_df$Equal.ESG = round(colMeans(equal_ESG_average_size_score), 2)
average_btp_score_df$Equal.ESG = round(colMeans(equal_ESG_average_btp_score), 2)
average_mom_score_df$Equal.ESG = round(colMeans(equal_ESG_average_mom_score), 2)
beta_df$Equal.ESG = round(t(CAPM.beta(equal_ESG_portfolios_returns[,1:5], benchmark_returns, Rf = 0)), 2)
bull_beta_df$Equal.ESG = round(t(CAPM.beta.bull(equal_ESG_portfolios_returns[,1:5], benchmark_returns, Rf = 0)), 2)
bear_beta_df$Equal.ESG = round(t(CAPM.beta.bear(equal_ESG_portfolios_returns[,1:5], benchmark_returns, Rf = 0)), 2)

# Calculate cumulative returns
cum_ESGCS_portfolios_returns = cumprod(1+ESGCS_portfolios_returns) 
cum_E_portfolios_returns = cumprod(1+E_portfolios_returns) 
cum_S_portfolios_returns = cumprod(1+S_portfolios_returns) 
cum_G_portfolios_returns = cumprod(1+G_portfolios_returns)
cum_ESG_portfolios_returns = cumprod(1+ESG_portfolios_returns)
cum_equal_ESG_portfolios_returns = cumprod(1+equal_ESG_portfolios_returns)


# Plot
Sys.setlocale("LC_ALL", "English")
plot(cum_ESGCS_portfolios_returns, legend.loc = "left", main = "SP500 ESGCS Portfolios")
plot(cum_E_portfolios_returns, legend.loc = "left", main = "SP500 Environmental Portfolios")
plot(cum_S_portfolios_returns, legend.loc = "left", main = "SP500 Social Portfolios")
plot(cum_G_portfolios_returns, legend.loc = "left", main = "SP500 Corporate Governance Portfolios")
plot(cum_ESG_portfolios_returns, legend.loc = "left", main = "SP500 ESG Portfolios")
plot(cum_equal_ESG_portfolios_returns, legend.loc = "left", main = "SP500 Equal ESG Portfolios")

plot(ESGCS_average_scores, legend.loc = "left", main = "SP500 ESGCS Average Score")
plot(E_average_scores, legend.loc = "left", main = "SP500 Environmental Average Score")
plot(S_average_scores, legend.loc = "left", main = "SP500 Social Average Score")
plot(G_average_scores, legend.loc = "left", main = "SP500 Corporate Governance Average Score")
plot(ESG_average_scores, legend.loc = "left", main = "SP500 ESG Average Score")
plot(equal_ESG_average_scores, legend.loc = "left", main = "SP500 Equal ESG Average Score")

plot(ESGCS_portfolios_weights, legend.loc = "left", main = "SP500 ESGCS Portfolio Weight")
plot(E_portfolios_weights, legend.loc = "left", main = "SP500 Environmental Portfolio Weight")
plot(S_portfolios_weights, legend.loc = "left", main = "SP500 Social Portfolio Weight")
plot(G_portfolios_weights, legend.loc = "left", main = "SP500 Corporate Governance Portfolio Weight")
plot(ESG_portfolios_weights, legend.loc = "left", main = "SP500 ESG Portfolio Weight")
plot(equal_ESG_portfolios_weights, legend.loc = "left", main = "SP500 Equal ESG Portfolio Weight")

plot(ESGCS_average_size_score, legend.loc = "left", main = "SP500 ESGCS Average Size Score")
plot(E_average_size_score, legend.loc = "left", main = "SP500 Environmental Average Size Score")
plot(S_average_size_score, legend.loc = "left", main = "SP500 Social Average Size Score")
plot(G_average_size_score, legend.loc = "left", main = "SP500 Corporate Governance Average Size Score")
plot(ESG_average_size_score, legend.loc = "left", main = "SP500 ESG Average Size Score")
plot(equal_ESG_average_size_score, legend.loc = "left", main = "SP500 Equal ESG Average Size Score")

plot(ESGCS_average_btp_score, legend.loc = "left", main = "SP500 ESGCS Average BTP Score")
plot(E_average_btp_score, legend.loc = "left", main = "SP500 Environmental Average BTP Score")
plot(S_average_btp_score, legend.loc = "left", main = "SP500 Social Average BTP Score")
plot(G_average_btp_score, legend.loc = "left", main = "SP500 Corporate Governance Average BTP Score")
plot(ESG_average_btp_score, legend.loc = "left", main = "SP500 ESG Average BTP Score")
plot(equal_ESG_average_btp_score, legend.loc = "left", main = "SP500 Equal ESG Average BTP Score")

plot(ESGCS_average_mom_score, legend.loc = "left", main = "SP500 ESGCS Average Momentum Score")
plot(E_average_mom_score, legend.loc = "left", main = "SP500 Environmental Average Momentum Score")
plot(S_average_mom_score, legend.loc = "left", main = "SP500 Social Average Momentum Score")
plot(G_average_mom_score, legend.loc = "left", main = "SP500 Corporate Governance Average Momentum Score")
plot(ESG_average_mom_score, legend.loc = "left", main = "SP500 ESG Average Momentum Score")
plot(equal_ESG_average_mom_score, legend.loc = "left", main = "SP500 Equal ESG Average Momentum Score")

plot(ESGCS_missing_btp_score, legend.loc = "left", main = "SP500 ESGCS Missing BTP Score (%)")
plot(E_missing_btp_score, legend.loc = "left", main = "SP500 Environmental Missing BTP Score (%)")
plot(S_missing_btp_score, legend.loc = "left", main = "SP500 Social Missing BTP Score (%)")
plot(G_missing_btp_score, legend.loc = "left", main = "SP500 Corporate Governance Missing BTP Score (%)")
plot(ESG_missing_btp_score, legend.loc = "left", main = "SP500 ESG Missing BTP Score (%)")
plot(equal_ESG_missing_btp_score, legend.loc = "left", main = "SP500 Equal ESG Missing BTP Score (%)")

plot(ESGCS_missing_mom_score, legend.loc = "left", main = "SP500 ESGCS Missing Momentum Score (%)")
plot(E_missing_mom_score, legend.loc = "left", main = "SP500 Environmental Missing Momentum Score (%)")
plot(S_missing_mom_score, legend.loc = "left", main = "SP500 Social Missing Momentum Score (%)")
plot(G_missing_mom_score, legend.loc = "left", main = "SP500 Corporate Governance Missing Momentum Score (%)")
plot(ESG_missing_mom_score, legend.loc = "left", main = "SP500 ESG Missing Momentum Score (%)")
plot(equal_ESG_missing_mom_score, legend.loc = "left", main = "SP500 Equal ESG Missing Momentum Score (%)")

plot(all_best_stocks_number, main = "SP500 Number of all best companies")
