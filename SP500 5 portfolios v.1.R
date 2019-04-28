# Libraries
# library(stringr)
library(lubridate)
library(xts)
# library(PerformanceAnalytics)
# library(psych)
# library(openxlsx)

wd ="D:/Projects/IN PROGRESS/GitHub/Industry_lab_user2/"
source(paste0(wd,"pclasses.R"))

start_date = "2003-06-01"
end_date = "2018-05-31"

start_date_esg = "2002-12-01"
end_date_esg = "2017-12-31"

data_containers_dir = file.path(paste0(wd,"Data Containers/SPCOMP"))
conR = dbFsConInit(data_containers_dir)

# Read data container with filter
data_containers_names = c("SPCOMP_IsinIX_W",
                          "SPCOMP_RIAbsSimpR_W_USD",
                          "SPCOMP_Benchmark_RIAbsSimpR_W_USD",
                          "SPCOMP_Score_Size",
                          "SPCOMP_Score_BTP",
                          "SPCOMP_Score_Momentum"
                          )

xts_names = c("filter",
              "stock_returns",
              "benchmark_returns",
              "size_score",
              "btp_score",
              "mom_score"
              )

# Create xts objects from containers
for(i in 1:length(data_containers_names)){
  dc5 = dbConRead(conR, dcInit(id = data_containers_names[i], dbCon = conR))
  df_name = paste0(xts_names[i],"_df")
  
  # Create data frame and xts object with predefined names
  assign(df_name, as.data.frame(dc5$data))
  temp_df = get(df_name)
  xts_object = xts(x = temp_df, order.by = as.Date(rownames(temp_df)))
  # Analyzed Period
  xts_object = window(x = xts_object, start = start_date, end = end_date)
  assign(xts_names[i], xts_object)
}


# Convert percent to number
stock_returns = stock_returns / 100
benchmark_returns = benchmark_returns / 100

# sp500 universe info 
all_stocks_num = ncol(stock_returns)
all_stocks_codes = colnames(stock_returns)
weeks_num_in_period = nrow(stock_returns)
period = index(stock_returns)


# Read monthly ESG scores
esg_scores_df = read.csv(file = paste0(wd,"SPCOMP_scores_monthly.csv"),header = TRUE, sep =";", dec = ",")
# Convert to the best suitable format
esg_scores_df[] = lapply(esg_scores_df, function(x) type.convert(as.character(x), as.is = TRUE))
# Delete first "Date" column
esg_scores_colnames = colnames(esg_scores_df)[-1]
esg_stock_codes = esg_scores_df[1,-1]

# Delete the first row with stock codes
esg_scores_df = esg_scores_df[-1,]
esg_scores_df$Date = as.Date(esg_scores_df$Date, format = "%d.%m.%Y")
# Create xts object
esg_scores = xts(x = esg_scores_df[,-1], order.by = esg_scores_df[,1])
# Analyzed period
esg_scores = window(x = esg_scores, start = start_date_esg, end = end_date_esg)
# Take only decembers
esg_scores_dec = esg_scores[month(index(esg_scores))==12]
dec_period = index(esg_scores_dec)
# Make values numeric
esg_scores_dec_df = as.data.frame(apply(apply(esg_scores_dec, 2, gsub, patt=",", replace="."), 2, as.numeric))
esg_scores_dec = xts(x = esg_scores_dec_df, order.by = dec_period)

# Split esg scores into 5 groups: ESGCS, ESG, E , S , G
esg_scores_patterns = c("ESG.Combined.Score",
                         "ESG.Score",
                         "Environmental",
                         "Social",
                         "Corporate.Governance")

esg_scores = c("esgcs",
                "esg",
                "e",
                "s",
                "g")

esg_scores_num = length(esg_scores)

#  Create 5 xts objects for 5 diff scores                        
for(i in 1:esg_scores_num){
  # Apply pattern
  stock_ind  = grep(pattern = esg_scores_patterns[i], x = esg_scores_colnames)
  # Take only required score data
  xts_object = esg_scores_dec[, stock_ind]
  colnames(xts_object) = esg_stock_codes[stock_ind]
  # Assign name to new xts object
  assign(x = esg_scores[i], value = xts_object)
}

# Create equal esg
equal_esg = (1/3)*(e + s + g)

# Add equal esg
esg_scores = c(esg_scores,
               "equal_esg")
esg_scores_num = length(esg_scores)

# Portfolios info
portfolio_names = c("Portfolio 1",
                    "Portfolio 2",
                    "Portfolio 3",
                    "Portfolio 4",
                    "Portfolio 5")

portfolios_num = length(portfolio_names)

extended_portfolio_names = c(portfolio_names,
                             "All best",
                             "Benchmark")

extended_portfolios_num = length(extended_portfolio_names)

# Create xts objects to save results

# Create xts object to save portfolio returns
df1 = data.frame(matrix(NA,nrow = weeks_num_in_period, ncol = extended_portfolios_num))
colnames(df1) = extended_portfolio_names
xts_object1 = xts(x = df1, order.by = period)
rm(df1)
# Create xts object containing filter(NA,1,2,3,4,5) to show in which portfolio is stock in current week
df2 = data.frame(matrix(NA,nrow = weeks_num_in_period, ncol = all_stocks_num))
colnames(df2) = all_stocks_codes
xts_object2 = xts(x = df2, order.by = period)
rm(df2)

for(esg_score in esg_scores){
  
  # Create xts object to save portfolio returns
  xts_object1_name = paste0(esg_score, "_portfolio_returns")
  assign(x = xts_object1_name, value = xts_object1)
  
  # Create xts object to save in which portfolio is particular stock in particular week
  xts_object2_name = paste0(esg_score, "_filter")
  assign(x = xts_object2_name, value = xts_object2)
}

# Create xts objects to save average scores
avg_scores = c("_avg_esg_score",
               "_avg_size_score",
               "_avg_btp_score",
               "_avg_mom_score")

df = data.frame(matrix(NA, nrow = weeks_num_in_period, ncol = portfolios_num))
colnames(df) = portfolio_names
xts_object = xts(x = df, order.by = period)
rm(df)

for(esg_score in esg_scores){
  for(avg_score in avg_scores){
    xts_object_name = paste0(esg_score, avg_score)
    assign(x = xts_object_name, value = xts_object)
  }
}


# Create xts objects to save results for "all best" stocks
# Number of "all best" stocks in current week
all_best_stocks_num = xts(x = rep(NA, weeks_num_in_period), order.by = period)
colnames(all_best_stocks_num) = "Number of stocks"

all_best_portfolio_returns = xts(x = rep(NA, weeks_num_in_period), order.by = period)
colnames(all_best_portfolio_returns) = "Return"

# Create xts object containing filter (NA, 1) to show if stock in "all best" in current week
all_best_stock_filter_df = data.frame(matrix(NA,nrow = weeks_num_in_period, ncol = all_stocks_num))
colnames(all_best_stock_filter_df) = all_stocks_codes
all_best_stock_filter = xts(x = all_best_stock_filter_df, order.by = period)

# Create xts object to show number of stocks in index with existing esg score in current week
stocks_num_with_esg_score_df = data.frame(matrix(NA,nrow = weeks_num_in_period, ncol = esg_scores_num ))
colnames(stocks_num_with_esg_score_df) = esg_scores
stocks_num_with_esg_score = xts(x = stocks_num_with_esg_score_df, order.by = period)

# Create statistics df
# For the whole period (not for a week)
statistics_df_names = c("annual_return_df",
                        "annual_sd_df",
                        "avg_esg_score_df",
                        "avg_size_score_df",
                        "avg_btp_score_df",
                        "avg_mom_score_df",
                        "beta_df",
                        "bull_beta_df",
                        "bear_beta_df")

df = data.frame(matrix(NA, nrow = portfolios_num, ncol = esg_scores_num))
rownames(df) = portfolio_names
colnames(df) = esg_scores

for(statistics_df_name in statistics_df_names)
{
  assign(x = statistics_df_name, value = df)
}
rm(df)

# Scores are changing once a  (in Dec)
week_num = 0
# get years
years = year(dec_period)[-1]

# Create vectors to save stock codes for stocks that are in index and have esg sore (current week)
vector = c()
for(esg_score in esg_scores){
  vector_name = paste0("stock_codes_with_", esg_score)
  assign(x = vector_name, value = vector)
}

for(i in 1:length(years)){
  
  year = years[i]
  previous_year = year - 1
  next_year = year + 1
  
  # Data of current year
  first_date_curr_year = as.Date(paste0(year, "-06-01"))
  last_date_curr_year = as.Date(paste0(next_year, "-05-31"))
  
  xts_names = xts_names[xts_names!= "benchmark_returns"]
  num_weeks_curr_year = 0
  # Create xts objects (filter, stock returns,size/btp/mom scores) for current investment year
  for(xts_name in xts_names){
    # Load data from data container (whole period)
    xts_object = get(xts_name)
    # Get only current investment year
    xts_object_curr_year = window(x = xts_object, start = first_date_curr_year, end = last_date_curr_year)
    xts_object_curr_year_name = paste0(xts_name, "_curr_year")
    assign(x = xts_object_curr_year_name, value = xts_object_curr_year)
    if(num_weeks_curr_year == 0){
      num_weeks_curr_year = nrow(xts_object_curr_year)
    }
  }
  
  # Loop through every week of current investment year
  for (j in 1:num_weeks_curr_year){
    
    # To observe how loop is running
    week_num = week_num + 1
    cat("Row" , week_num, "out of", weeks_num_in_period, "\n")
    
    # Data for current week
    for(xts_name in xts_names){
      # Load data for current year
      xts_object_curr_year = get(paste0(xts_name, "_curr_year"))
      
      # Create data for current week
      xts_object_curr_week = xts_object_curr_year[j,]
      xts_object_curr_week_name = paste0(xts_name, "_curr_week")
      assign(x = xts_object_curr_week_name, value = xts_object_curr_week)
    }
    
    # For each type of score: esgcs, esg, s,g, equal_esg
    for(esg_score in esg_scores){
      # Get relevant df by name (stores a score Dec data)
      esg_score_xts = get(esg_score)
      # Duplicate 
      stock_codes_with_esg_score = get(paste0("stock_codes_with_", esg_score))
      
      stock_codes_in_index = colnames(filter_curr_week)[which(as.vector(filter_curr_week)== 1)]
      
      # Loop through all stocks
      for(stock_code in stock_codes_in_index){
          # Returns stock code or numeric(0)
          stock_code_with_any_esg_score = grep(pattern = stock_code, x = colnames(esg_score_xts), value = T)    
          # In case a stock has a score or NA
          if(length(stock_code_with_any_esg_score) > 0){
            # Returns esg score for a stock (or NA)
            any_esg_score = esg_score_xts[i,stock_code_with_any_esg_score]
            # If this esg scores exists (not NA)
            if(is.na(any_esg_score) == F){
              # Add this stock code to vector of stock codes that are in index and have esg score (current week)
              stock_codes_with_esg_score = c(stock_codes_with_esg_score, stock_code)
            }#endif not NA
          }#endif not esg exists
      }#endfor all stocks

      stocks_num_with_esg_score[week_num, esg_score] = length(stock_codes_with_esg_score)
      
      # Load data
      esg_score_portfolio_returns = get(paste0(esg_score, "_portfolio_returns"))
      esg_score_avg_esg_score = get(paste0(esg_score, "_avg_esg_score"))
      esg_score_avg_size_score = get(paste0(esg_score, "_avg_size_score"))
      esg_score_avg_btp_score = get(paste0(esg_score, "_avg_btp_score"))
      esg_score_avg_mom_score = get(paste0(esg_score, "_avg_mom_score"))
      esg_score_filter = get(paste0(esg_score, "_filter"))

      
      # Calculate number of stocks in one portfolio
      stocks_num_in_esg_score_portfolio = length(stock_codes_with_esg_score) %/% 5
      # esg score we analyze current week
      esg_score_curr_week = esg_score_xts[i,stock_codes_with_esg_score]

      
      for(l in 1:portfolios_num){
        
        # Find constituents of portfolio 
        largest_esg_score_portfolio_l = sort(as.vector(esg_score_curr_week), decreasing = F)[stocks_num_in_esg_score_portfolio]
        esg_score_portfolio_l_ind = which(esg_score_curr_week <= largest_esg_score_portfolio_l)
        # Could be several scores with the same value on the "border"
        esg_score_portfolio_l_ind = esg_score_portfolio_l_ind[1:stocks_num_in_esg_score_portfolio]
        esg_score_portfolio_l= esg_score_curr_week[, esg_score_portfolio_l_ind]
        
        esg_score_portfolio_l_stock_codes  = colnames(esg_score_portfolio_l)
        # esg_score_portfolio_l_stock_codes = colnames(as.data.frame(esg_score_curr_week)[esg_score_portfolio_l_ind])
        
        # Calculate portfolio return current week
        esg_score_portfolio_returns[week_num,l] = mean(stock_returns_curr_week[, esg_score_portfolio_l_stock_codes])
       
        # Calculate average esg score in portfolio current week
        esg_score_avg_esg_score[week_num,l] = mean(esg_score_portfolio_l, na.rm = T)
        
        # Calculate average size score in portfolio current week
        esg_score_avg_size_score[week_num,l] = mean(size_score_curr_week[, esg_score_portfolio_l_stock_codes], na.rm = T)
        
        # Calculate average btp score in portfolio current week
        esg_score_avg_btp_score[week_num,l] = mean(btp_score_curr_week[, esg_score_portfolio_l_stock_codes], na.rm = T)
        
        # Calculate average momentum score in portfolio current week
        esg_score_avg_mom_score[week_num,l] = mean(mom_score_curr_week[, esg_score_portfolio_l_stock_codes], na.rm = T)
        
        # Fill filter
        esg_score_filter[week_num, esg_score_portfolio_l_stock_codes] = l

        # Replace esg scores in portfolio with very big number
        esg_score_curr_week[esg_score_portfolio_l_stock_codes] = 1000000
        
        
        # Find best E stocks
        if(esg_score == "e" & l== 5){
          best_e_stocks = esg_score_portfolio_l_stock_codes
        }
        
        if(esg_score == "s" & l== 5){
          best_s_stocks = esg_score_portfolio_l_stock_codes
        }
        
        if(esg_score == "g" & l== 5){
          best_g_stocks = esg_score_portfolio_l_stock_codes
        }
        
      }#endfor portfolio num
      assign(x = paste0("stock_codes_with_", esg_score), value = stock_codes_with_esg_score)
      assign(x = esg_score, value = esg_score_xts)
      assign(x = paste0(esg_score,"_portfolio_returns"), value = esg_score_portfolio_returns)
      assign(x = paste0(esg_score,"_avg_esg_score"), value = esg_score_avg_esg_score)
      assign(x = paste0(esg_score,"_avg_size_score"), value = esg_score_avg_size_score)
      assign(x = paste0(esg_score,"_avg_btp_score"), value = esg_score_avg_btp_score)
      assign(x = paste0(esg_score,"_avg_mom_score"), value = esg_score_avg_mom_score)
      assign(x = paste0(esg_score,"_filter"), value = esg_score_filter)
    }#endfor all esg scores


    # Find number of all best stocks
    all_best_stocks = Reduce(intersect, list(best_e_stocks, best_s_stocks, best_g_stocks))
    all_best_stocks_num[week_num,"Number of stocks"] = length(all_best_stocks)
    # Fill filter
    all_best_stock_filter[week_num, all_best_stocks] = 1
    
    # Find returns of all best stocks
    all_best_portfolio_returns[week_num,"Return"] =  mean(stock_returns_curr_week[, all_best_stocks])

  }#end of curr. week
}#end of curr. year

# Add All Best Portfolio returns and Benchmark returns
for(esg_score in esg_scores){
  # Load data
  esg_score_portfolio_returns = get(paste0(esg_score, "_portfolio_returns"))
  
  esg_score_portfolio_returns$`All best` = all_best_portfolio_returns
  esg_score_portfolio_returns$Benchmark = benchmark_returns
  assign(x = paste0(esg_score,"_portfolio_returns"), value = esg_score_portfolio_returns)
}#end for "all best"
































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

plot(all_best_stocks_num, main = "SP500 Number of all best companies")

# Create final workbook
wb = createWorkbook()

# Export ESGCS_filter_df to Excel
addWorksheet(wb, "ESGCS_filter_df")
writeData(wb, "ESGCS_filter_df", ESGCS_filter_df)
setColWidths(wb, sheet = "ESGCS_filter_df", cols = 1:ncol(ESGCS_filter_df), widths = "auto")

# Export E_filter_df to Excel
addWorksheet(wb, "E_filter_df")
writeData(wb, "E_filter_df", E_filter_df)
setColWidths(wb, sheet = "E_filter_df", cols = 1:ncol(E_filter_df), widths = "auto")

# Export S_filter_df to Excel
addWorksheet(wb, "S_filter_df")
writeData(wb, "S_filter_df", S_filter_df)
setColWidths(wb, sheet = "S_filter_df", cols = 1:ncol(S_filter_df), widths = "auto")

# Export G_filter_df to Excel
addWorksheet(wb, "G_filter_df")
writeData(wb, "G_filter_df", G_filter_df)
setColWidths(wb, sheet = "G_filter_df", cols = 1:ncol(G_filter_df), widths = "auto")

# Export ESG_filter_df to Excel
addWorksheet(wb, "ESG_filter_df")
writeData(wb, "ESG_filter_df", ESG_filter_df)
setColWidths(wb, sheet = "ESG_filter_df", cols = 1:ncol(ESG_filter_df), widths = "auto")

# Export equal_ESG_filter_df to Excel
addWorksheet(wb, "equal_ESG_filter_df")
writeData(wb, "equal_ESG_filter_df", equal_ESG_filter_df)
setColWidths(wb, sheet = "equal_ESG_filter_df", cols = 1:ncol(equal_ESG_filter_df), widths = "auto")

# Export all_best_stock_filter_df to Excel
addWorksheet(wb, "all_best_stock_filter_df")
writeData(wb, "all_best_stock_filter_df", all_best_stock_filter_df)
setColWidths(wb, sheet = "all_best_stock_filter_df", cols = 1:ncol(all_best_stock_filter_df), widths = "auto")

# Save final workbook
final_file_path = 'C:/Users/WZHYAK/Desktop/Industry Lab/ Final.xlsx'
saveWorkbook(wb, final_file_path, overwrite = TRUE)
