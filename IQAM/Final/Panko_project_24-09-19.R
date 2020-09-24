# Part 1:
#  Represent the influence of the SustFilter on the number of
#  companies available and ratios (frequency and market capitalization)

# Part 2:
#  Construct long-short factors and determine the influence of 
#  the Survivorship bias and SustFilter on the average scores

# Part 3:
#  Calculate long-short and long only factor returns and determine the
#  influence of the Survivorship bias and SustFilter on the performance

library(PerformanceAnalytics)

create_xts <- function(df){
  # This function deletes rows with zeroes only
  # and then creates xts object
  
  row_sub = apply(df, 1, function(row) all(row == 0 | is.na(row)))
  df_nzr <-  df[!row_sub,]
  if(is.null(rownames(df_nzr)) == TRUE){
    # this means that is it a benchmark (only 1 column)
    res_xts <- xts(x = df_nzr, order.by = as.Date(names(df_nzr)))  
  }
  else{
    # normal df
    res_xts <- xts(x = df_nzr, order.by = as.Date(rownames(df_nzr)))  
  }
  
  return(res_xts)
}

invert_filter <- function(filter){
  # The function creates a complement filter
  
  # Step 1: save names, ncol, nrows
  col_names <- colnames(filter)
  row_names <- index(filter)
  n_col <- ncol(filter)
  n_row <- nrow(filter)
  
  # Step 2: convert to matrix (with 1,0)
  num_ma <- as.matrix(filter)
  
  # Step 3: convert to T/F
  log_vec <- as.logical(num_ma)
  
  # Step 4: invert t/f to f/t
  inv_log_vec <- !log_vec
  
  # Step 5: convert to numeric vector
  inv_num_vec <- as.numeric(inv_log_vec)
  
  # Step 6: convert to matrix
  inv_ma <- matrix(inv_num_vec, nrow = n_row, ncol = n_col)
  
  # Step 7: to data frame
  inv_filter <- xts(x = inv_ma, order.by = row_names)
  
  # add colnames and rownames
  colnames(inv_filter) <- col_names
  
  return(inv_filter)
}

load_containers <- function(path){
  # The function loads original data
  
  setwd(path)
  
  # Main filter (in or not in a portfolio).
  load("dcIsinIX.Rraw")
  isin <- dcIsinIX$data
  
  # Upload GCCR filter. we need NA, since they can explain jumps
  load("dcSustFilter.Rraw")
  gccr <- dcSustFilter$data
  
  # Market capitalization: we need to substitute NA with 0 to let us sum up
  load("dcMCAP.Rraw")
  mcap <- dcMCAP$data
  mcap[is.na(mcap)] <- 0
  
  # Momentum score
  # Here we need NA, since score can be 0
  load("dcScore_SimpMo1Y.Rraw")
  mom <- dcScore_SimpMo1Y$data
  
  # Size core
  # Here we need NA, since score can be 0
  load("dcScore_Size.Rraw")
  size <- dcScore_Size$data
  
  # Returns container, weekly in %
  if(path == "H:/Alex/DB20190630"){
    # For bonds container name is different
    load("dcRIRET1M.Rraw")
    returns <- dcRIRET1M$data
    
    load("dcBond_NumberRating.Rraw")
    rating <- dcBond_NumberRating$data
    
    colnames(rating) <- colnames(returns)
    
    # TODO For bonds we don't have a benchmark
    bm <- NULL
    # TODO For bonds we don't have value factor
    value <- NULL
    
    # Bug in isin for Bonds in names mapping
    colnames(isin) <- colnames(mcap)
    
  }else{
    
    # we don't have bond rating for equity
    rating <- NULL
    
    load("dcRIRET.Rraw")
    returns <- dcRIRET$data
    
    # Benchmark. we don't need NA
    load("dcBenchmark_RIRET.Rraw")
    bm <- dcBenchmark_RIRET$data
    
    bm[is.na(bm)] <- 0
    bm[is.infinite(bm)] <- 0
    bm <- bm/100
    
    # Value score
    # Here we need NA, since score can be 0
    load("dcScoreShifted_BookToPrice.Rraw")
    value <- dcScoreShifted_BookToPrice$data
    
  }
  return_list <- list(isin = isin,
                      gccr = gccr,
                      mcap = mcap,
                      returns = returns,
                      bm = bm,
                      value = value,
                      mom = mom,
                      rating = rating,
                      size = size)
  return(return_list)
}

frame_all_xts <- function(path,
                          isin,
                          gccr,
                          mcap,
                          mom,
                          returns,
                          rating = NULL,
                          bm = NULL,
                          value = NULL,
                          size){
  
  # The function frames all time series to represent the same period
  
  if(path == "H:/Alex/DB20190630"){
    
    start_dates <- c(start(isin),
                     start(gccr),
                     start(mcap),
                     start(mom),
                     start(size),
                     start(returns),
                     start(rating))
    
    end_dates <- c(end(isin),
                   end(gccr),
                   end(mcap),
                   end(mom),
                   end(size),
                   end(returns),
                   end(rating))
    
    start_date <- max(start_dates)
    end_date <- min(end_dates)
    
    rating <- window(x = rating, start = start_date, end = end_date)
  }
  else{

    start_dates <- c(start(isin), 
                     start(gccr),
                     start(mcap),
                     start(mom),
                     start(size),
                     start(returns),
                     start(bm),
                     start(value))
    
    end_dates <- c(end(isin),
                   end(gccr),
                   end(mcap),
                   end(mom),
                   end(size),
                   end(returns),
                   end(bm),
                   end(value))
    
    start_date <- max(start_dates)
    end_date <- min(end_dates)
    
    bm <- window(x = bm, start = start_date, end = end_date)
    value <- window(x = value, start = start_date, end = end_date)
    
  }
  
  # Frame time series
  isin <- window(x = isin, start = start_date, end = end_date)
  gccr <- window(x = gccr, start = start_date, end = end_date)
  mcap <- window(x = mcap, start = start_date, end = end_date)
  mom <- window(x = mom, start = start_date, end = end_date)
  size <- window(x = size, start = start_date, end = end_date)
  returns <- window(x = returns, start = start_date, end = end_date)
  
  
  return_list <- list(isin = isin,
                      gccr = gccr,
                      mcap = mcap,
                      returns = returns,
                      bm = bm,
                      value = value,
                      mom = mom,
                      rating = rating,
                      size = size)
  return(return_list)
  
}

create_filers <- function(task,
                          path,
                          isin,
                          gccr,
                          mcap,
                          mom,
                          returns,
                          rating = NULL,
                          bm = NULL,
                          value = NULL,
                          size){
  # The function prepares BASIC filters from the original data
  
  # Step 1: GCCR and ISIN universes are not the same
  #         we need to take inerception
  
  # Step 2: For bonds create index (isin) either Junk  or IG
  
  # Step 3: Create filters taking into account GCCR info
  
  
  
  # Step 1
  indices <- which(colnames(isin) %in% colnames(gccr))
  col_names <- colnames(isin[,indices])
  
  isin <- isin[, col_names]
  gccr <- gccr[, col_names]
  mcap <- mcap[, col_names]
  mom <- mom[, col_names]
  size <- size[, col_names]
  returns <- returns[, col_names]
  
  
  # Step 2
  if (path == "H:/Alex/DB20190630") {
    rating <- rating[, col_names]
    
    # 10 is a threshold
    if(task == "IGBonds"){
      ig_bonds <- rating
      ig_bonds[ig_bonds <= 10] <- 1
      ig_bonds[ig_bonds > 10] <- 0
      ig_bonds <- isin * ig_bonds
      isin <- ig_bonds
    }
    else{
      j_bonds <- rating
      j_bonds[j_bonds <= 10] <- 0
      j_bonds[j_bonds > 10] <- 1
      j_bonds <- isin * j_bonds
      isin <- j_bonds
    }
    # isin shouldn't have NA
    isin[is.na(isin)] <- 0
  }
  else{
    value <- value[, col_names]
  }
  
  
  # Step 3
  # simple number (originally data is in %)
  returns <- returns / 100
  
  # remove NA
  returns[is.na(returns)] <- 0
  returns[is.infinite(returns)] <- 0
  
  # Required for long portf
  # Apply filter of GCCR. 0,1,NA is here
  isin_gccr <- isin * gccr
  
  # Create filter
  filter_NA <- gccr * 0
  filter_NA[filter_NA == 0] <- 1
  filter_NA[is.na(filter_NA)] <- 0
  
  # From MSCI exclude ones that are not represented in GCCR (NA)
  isin_wo_NA <- isin * filter_NA
  
  # MSCI w/o 0 and NA in GCCR
  isin_wo_NA0 <- isin_gccr
  isin_wo_NA0[is.na(isin_wo_NA0)] <- 0
  
  isin_no_gccr <- gccr
  isin_no_gccr[isin_no_gccr == 1] <- NA
  isin_no_gccr[isin_no_gccr == 0] <- 1
  isin_no_gccr[is.na(isin_no_gccr)] <- 0
  
  return_list <- list(isin = isin,
                      gccr = gccr,
                      isin_gccr = isin_gccr,
                      isin_no_gccr = isin_no_gccr,
                      isin_wo_NA = isin_wo_NA,
                      isin_wo_NA0 = isin_wo_NA0,
                      mcap = mcap,
                      returns = returns,
                      bm = bm,
                      value = value,
                      mom = mom,
                      size = size)
  
  return(return_list)
}

prepare_data <- function(path, task){
  # the function delivers time series and filters of 
  # original data which are used in perfomance and 
  # average score analysis
  
  containers <- load_containers(path)
  
  # Since the data are time series we'd better convert df->xts
  isin <- create_xts(containers$isin)
  gccr <- create_xts(containers$gccr)
  mcap <- create_xts(containers$mcap)
  mom <- create_xts(containers$mom)
  size <- create_xts(containers$size)
  returns <- create_xts(containers$returns)
  if(path == "H:/Alex/DB20190630"){
    # Bonds: Junk and Investment grade
    rating <- create_xts(containers$rating)
    bm <- NULL
    value <- NULL
  }
  else{
    # Equity
    bm <- create_xts(containers$bm)
    value <- create_xts(containers$value)
    rating <- NULL
  }
  
  containers <- frame_all_xts(path = path,
                              isin = isin,
                              gccr = gccr,
                              mcap = mcap,
                              mom = mom,
                              returns = returns,
                              rating = rating,
                              bm = bm,
                              value = value,
                              size = size)

 filters <- create_filers(task = task,
                          path = path,
                          isin = containers$isin,
                          gccr = containers$gccr,
                          mcap = containers$mcap,
                          mom = containers$mom,
                          returns = containers$returns,
                          rating = containers$rating,
                          bm = containers$bm,
                          value = containers$value,
                          size = containers$size)
 
 return(filters)
}

analyze_index_performance <- function(ret, isin, isin_wo_NA, isin_wo_NA0){
  # The function produces 3 returns time series for 
      # 1) equally-weighed original index (isin_p_ret)
      # 2) equally-weighed survivorship biased index
      # 3) equally-weighed "good-only" index 
    
  
  # Step 1: create equal weights
  # Step 2: create returns xts
  # Step 3: run portfolio return function
  # Step 4: bind together and create 1 xts object
  
  
  # Step 1
  isin_w <- create_eq_weights(isin)
  index_wo_NA_w <- create_eq_weights(isin_wo_NA)
  isin_wo_NA0_w <- create_eq_weights(isin_wo_NA0)
  
  # Step 2
  isin_p <- ret * isin
  index_wo_NA_p <- ret * isin_wo_NA
  isin_wo_NA0_p <- ret * isin_wo_NA0
  
  # Step 3
  isin_p_ret <- Return.portfolio(R = isin_p,
                                 weights = isin_w,
                                 geometric = TRUE)
  
  index_wo_NA_p_ret <- Return.portfolio(R = index_wo_NA_p,
                                      weights = index_wo_NA_w,
                                      geometric = TRUE)
  
  isin_wo_NA0_p_ret <- Return.portfolio(R = isin_wo_NA0_p,
                                        weights = isin_wo_NA0_w,
                                        geometric = TRUE)
  # Step 4
  ind_portf_ret <- cbind(isin_p_ret,
                         index_wo_NA_p_ret,
                         isin_wo_NA0_p_ret)
  
  colnames(ind_portf_ret) <- c("isin_p_ret",
                               "index_wo_NA_p_ret",
                               "isin_wo_NA0_p_ret")
  return(ind_portf_ret)
}

apply_gccr <- function(isin, isin_wo_NA, isin_wo_NA0, isin_gccr, mcap){
  # The function represent the influence of the SustFilter on the number of
  # companies available and ratios (frequency and market capitalization):
  
  # 1) How many companies are in index?
  # 2) How many remains if delete ones which have NA in GCCR. 
  #    This makes sence since we don't know anything about them
  # 3) How many are deleted by step 2. (number of NAs)
  # 4) How many remains if delete all 0 and NA in GCCR, i.e. assume NA = 0
  # 5) What is the frequency ratio? Only 1 in GCCR VS all index
  # 6) What is the market cap raio? Only 1 in GCCR VS all index
  
  # Note all the filters are w/o NA
  
  return_list <- list()
  
  # TASK 1
  n_ind <- rowSums(isin)
  n_ind <- xts(x = n_ind, order.by = index(isin))
  colnames(n_ind) <- c("N_ind")
  
  # TASK 2
  n_ind_wo_NA <- rowSums(isin_wo_NA)
  n_ind_wo_NA <- xts(x = n_ind_wo_NA, order.by = index(isin_wo_NA))
  colnames(n_ind_wo_NA) <- c("N_ind_wo_NA")
  
  # TASK 3
  n_NA <- n_ind - n_ind_wo_NA
  colnames(n_NA) <- c("N_NA")
  
  # TASK 4
  n_ind_wo_NA0 <- rowSums(isin_wo_NA0)
  n_ind_wo_NA0 <- xts(x = n_ind_wo_NA0, order.by = index(isin_wo_NA0))
  colnames(n_ind_wo_NA0) <- c("N_ind_wo_NA0")
  
  # TASK 5
  ratio_freq <- n_ind_wo_NA0 / n_ind
  colnames(ratio_freq) <- c("F_Ratio")

  # TASK 6
  isin_mcap <- mcap * isin
  isin_wo_NA0_mcap <- isin_wo_NA0 * mcap
  total_isin_mcap <- rowSums(isin_mcap)
  total_isin_wo_NA0_mcap <- rowSums(isin_wo_NA0_mcap)
  ratio_cap <- total_isin_wo_NA0_mcap / total_isin_mcap
  ratio_cap <- xts(x = ratio_cap, order.by = index(isin_wo_NA0))
  colnames(ratio_cap) <- c("MC_Ratio")
  
  # Prepare result
  result_xts <- cbind(n_ind,
                      n_ind_wo_NA,
                      n_NA,
                      n_ind_wo_NA0,
                      ratio_freq,
                      ratio_cap)

  return(result_xts)
}

create_eq_weights <- function(filter){
  # The function creates equal weights for a portfolio
  # Note: a filter must not contain NA and zero rows
  
  weights <- filter
  stocks_num  <- rowSums(filter)
  
  multiplier <- (1 / stocks_num)
  
  # every row is multiplied with corresponding multiplier
  # since filter includes only 1 and 0 we can do it in this way
  weights <- multiplier * weights
  
  return(weights)
}

calculate_factor_return <- function(ret, filters_l, long_only){
  # The function calculates a factor return for 2 types of factors
    # 1) long-short
    # 2) long only
  
  if(long_only == FALSE){
    
  # To calculate portfolio return PerformanceAnalytics::Return.portfolio
  # is used. This function has a feature that sum of weights MUST be = 1
  # in ALL cases.
  
  long_weights_xts <- create_eq_weights(filter = filters_l$long)
  short_weights_xts <- create_eq_weights(filter = filters_l$short)
  
  # Calculate long-short weights
  w <- long_weights_xts - short_weights_xts
  
  names <- colnames(w)
  
  # To avoid this feature, a dummy asset with constant weight 1 is required
  # Additionally the return of this asset should be constant 0
  dummy_w <- w[, 1]
  w <- cbind(dummy_w, w)
  colnames(w) <- c("dummy", names)
  w$dummy <- 1
  
  # add dummy with 0 returns
  dummy_r <- ret[, 1]
  ret <- cbind(dummy_r, ret)
  colnames(ret) <- c("dummy", names)
  ret$dummy <- 0
  
  
  factor_ret_xts <- Return.portfolio(R = ret,
                                     weights = w,
                                     geometric = TRUE)
  }
  else{
    # long_only == TRUE
    # Our fund not always is allowed to short, that is why we are 
    # interested in long only factor performanc
    
    long_weights_xts <- create_eq_weights(filter = filters_l$long)
    
    factor_ret_xts <- Return.portfolio(R = ret,
                                       weights = long_weights_xts,
                                       geometric = TRUE)
  }
  
  return(factor_ret_xts)
  
}

construct_factor_filter_wo_gccr <- function(score_df, per){
  # The function gives companies to form a factor
  # The original universe is used
  
  # all values are normalized beween [0,1]
  # per - is a step, e.g 0.1 or 0.2
  
  # long: set 1 to top % 
  factor_filer_l <- score_df
  factor_filer_l[factor_filer_l < (1-per)] <- NA
  factor_filer_l[!(factor_filer_l < (1-per))] <- 1
  factor_filer_l[is.na(factor_filer_l)] <- 0
  # short: set 1 to bottom % 
  factor_filer_s <- score_df
  factor_filer_s[factor_filer_s > per] <- NA
  factor_filer_s[!(factor_filer_s > per)] <- 1
  factor_filer_s[is.na(factor_filer_s)] <- 0
  
  return_list <- list(long = factor_filer_l, short = factor_filer_s)
  
  return(return_list)
}

calculate_avg_ls_score <- function(path, value, mom, size, factor_filters_l){
  # The function calculates the average scores of
  # the long and short parts of a factor
  
  # There are 3 types of factors: 
    # 1) based on original universe
    # 2) based on survived universe
    # 3) based on "good-only" universe
  
  avg_mom <- calculate_avg_ls_score_single(score_xts = mom,
                                           filters_l = factor_filters_l$mom)
  
  avg_mom_surv <- calculate_avg_ls_score_single(score_xts = mom,
                                                filters_l = factor_filters_l$mom_surv)
  
  avg_mom_gccr <- calculate_avg_ls_score_single(score_xts = mom,
                                                filters_l = factor_filters_l$mom_gccr)
  
  avg_size <- calculate_avg_ls_score_single(score_xts = size,
                                            filters_l = factor_filters_l$size)
  
  avg_size_surv <- calculate_avg_ls_score_single(score_xts = size,
                                                 filters_l = factor_filters_l$size_surv)
  
  avg_size_gccr <- calculate_avg_ls_score_single(score_xts = size,
                                                 filters_l = factor_filters_l$size_gccr)
  
  if(path != "H:/Alex/DB20190630"){
    
    avg_val <- calculate_avg_ls_score_single(score_xts = value,
                                             filters_l = factor_filters_l$value)
    
    avg_val_surv <- calculate_avg_ls_score_single(score_xts = value,
                                                  filters_l = factor_filters_l$value_surv)
    
    avg_val_gccr <- calculate_avg_ls_score_single(score_xts = value,
                                             filters_l = factor_filters_l$value_gccr)
    
    avg_score <- cbind(Mom = avg_mom, MomGCCR = avg_mom_gccr, MomSurv =  avg_mom_surv,
                       Size = avg_size, SizeGCCR = avg_size_gccr, SizeSurv = avg_size_surv,
                       Value = avg_val, ValueGCCR = avg_val_gccr, ValueSurv = avg_val_surv)
    
    names(avg_score) <- c("Mom L", "Mom S", "MomGCCR L", "MomGCCR S", "MomSurv L", "MomSurv S",
                          "Size L", "Size S", "SizeGCCR L", "SizeGCCR S", "SizeSurv L", "SizeSurv S",
                          "Value L", "Value S", "ValueGCCR L", "ValueGCCR S", "ValueSurv L", "ValueSurv S")
  }
  else{
    # Exclude Value
    avg_score <- cbind(Mom = avg_mom, MomGCCR = avg_mom_gccr, MomSurv =  avg_mom_surv,
                       Size = avg_size, SizeGCCR = avg_size_gccr, SizeSurv = avg_size_surv)
    
    names(avg_score) <- c("Mom L", "Mom S", "MomGCCR L", "MomGCCR S", "MomSurv L", "MomSurv S",
                          "Size L", "Size S", "SizeGCCR L", "SizeGCCR S", "SizeSurv L", "SizeSurv S")
  }
  

  
  return(avg_score)
}

calculate_avg_ls_score_single <- function(score_xts, filters_l){
  # The function calculates the average scores of
  # the long and short parts of a factor
  
  # a score contains NA, filters do not
  # we need to set NA to zeros to avoid a mistake in rowMeans
  filters_l$long[filters_l$long == 0] <- NA
  filters_l$short[filters_l$short == 0] <- NA
  
  avg_score_long <- rowMeans(score_xts * filters_l$long, na.rm = T)
  avg_score_long <- xts(avg_score_long, order.by = index(score_xts))
  
  avg_score_short <- rowMeans(score_xts * filters_l$short, na.rm = T)
  avg_score_short <- xts(avg_score_short, order.by = index(score_xts))
  
  # Combine 2 xts
  avg_scores_xts <- cbind(long = avg_score_long, short = avg_score_short)
 
  return(avg_scores_xts)
}

construct_factor_filter_gccr <- function(score_df, per, filter){
  # The function gives companies to form a factor
  # The universe used is created manually to acount for 
  # SustFilter (type 1: survivior biased universe, type 2: "Good-only")
  
  # Here filter can be only 1s or (1s and 0s)
  
  # Apply filter to a score 
  filtered_score <- score_df * filter
  
  # Initialize long and short filers
  long_filter <- filter
  short_filter <- filter
  
  # Take top and bottom PER(%) and form a portfolio
  for (i in 1:nrow(filtered_score)) {
    
    # We need to save and assign names
    col_names <- colnames(filtered_score)
    v <- as.vector(filtered_score[i,])
    names(v) <- col_names
    # exclude zeroes and NAs
    v <- v[v != 0 & !is.na(v)]
    # sort
    v <- sort(v, decreasing = FALSE)
    # determine the number of stocks(total, top and bottom)
    n <- ceiling(length(v) * per)
    # determine top and bottom
    bottom <- head(v, n)
    bottom_names <- names(bottom)
    
    top <- tail(v, n)
    top_names <- names(top)
    
    # update ls filters
    long_filter[i,][colnames(long_filter) %in% top_names] <- 1
    long_filter[i,][!(colnames(long_filter) %in% top_names)] <- 0
    
    short_filter[i,][colnames(long_filter) %in% bottom_names] <- 1
    short_filter[i,][!(colnames(long_filter) %in% bottom_names)] <- 0
  }
  
  # Create return list

  filters_l <- list(long = long_filter, short = short_filter)

  return(filters_l)
}

construct_factor_ls_filters <- function(isin_wo_NA0,
                                        isin_wo_NA,
                                        isin_no_gccr,
                                        value_s,
                                        mom_s,
                                        size_s,
                                        per,
                                        path){
  # The function constructs filters for long and short portfolios
  # These filters are used  to calculate the factors performance and
  # the average scores for long and short portfolios
  
  filters_gccr <- list(long = isin_wo_NA0, short = isin_no_gccr)
  
  # Original factor
  filters_mom <- construct_factor_filter_wo_gccr(mom_s, per)
  # Factor with Survivorship bias
  filters_mom_surv <- construct_factor_filter_gccr(mom_s, per, isin_wo_NA)
  # Factor with Survivorship bias and GCCR filtering (only 1)
  filters_mom_gccr <- construct_factor_filter_gccr(mom_s, per, isin_wo_NA0)
  
  filters_size <- construct_factor_filter_wo_gccr(size_s, per)
  filters_size_surv <- construct_factor_filter_gccr(size_s, per, isin_wo_NA)
  filters_size_gccr <- construct_factor_filter_gccr(size_s, per, isin_wo_NA0)
  
  if(path != "H:/Alex/DB20190630"){
    # we don't have value data for bonds
    filters_value <- construct_factor_filter_wo_gccr(value_s, per)
    filters_value_surv <- construct_factor_filter_gccr(value_s, per, isin_wo_NA)
    filters_value_gccr <- construct_factor_filter_gccr(value_s, per, isin_wo_NA0)
  }
  else{
    filters_value <- NULL
    filters_value_surv <- NULL
    filters_value_gccr <- NULL
  }
    
  
  return_list <- list(gccr = filters_gccr,
                      mom = filters_mom,
                      mom_surv = filters_mom_surv,
                      mom_gccr = filters_mom_gccr,
                      size = filters_size,
                      size_surv = filters_size_surv,
                      size_gccr = filters_size_gccr,
                      value = filters_value,
                      value_surv = filters_value_surv,
                      value_gccr = filters_value_gccr)
  
  return(return_list)
}

analyze_factors_performance <- function(returns,
                                        bm,
                                        isin_p_ret,
                                        filters_l,
                                        path,
                                        long_only){
  # The function calculates and combines in 1 xts object 
  # long-short factors performance and also benchmarks
  
  # Factors are: 
  #   gccr (long 1, short 0) factor
  #   mom(3 types)
  #   value(3 types)
  #   size(3 types) 
  
  # Types of factors:
      # 1) original
      # 2) Survived (0s and 1s)
      # 3) Good-only (only 1s)
  
  # Benchmarks are:
      # 1) bm = Original index
      # 2) isin_p_ret = equaly weighted index
  
  # long_only(TRUE/FALSE) shows if factor is long-short or long only
  # long-short GCCR factor
  
  ind_gccr <- calculate_factor_return(ret = returns,
                                      filters_l = filters_l$gccr,
                                      long_only = long_only)
  
  # Factor Momentum w/o GCCR
  mom <- calculate_factor_return(ret = returns,
                                 filters_l = filters_l$mom,
                                 long_only = long_only)
  # Factor Momentum Survived
  mom_surv <- calculate_factor_return(ret = returns,
                                 filters_l = filters_l$mom_surv,
                                 long_only = long_only)
  
  # Factor Momentum with GCCR
  mom_gccr <- calculate_factor_return(ret = returns,
                                      filters_l = filters_l$mom_gccr,
                                      long_only = long_only)
  
  # Factor Size w/o GCCR
  size <- calculate_factor_return(ret = returns,
                                  filters_l = filters_l$size,
                                  long_only = long_only)
  
  # Factor Size Survived
  size_surv <- calculate_factor_return(ret = returns,
                                  filters_l = filters_l$size_surv,
                                  long_only = long_only)
  
  # Factor Size with GCCR
  size_gccr <- calculate_factor_return(ret = returns,
                                       filters_l = filters_l$size_gccr,
                                       long_only = long_only)
  
  if(path != "H:/Alex/DB20190630"){
    
    # Factor Value w/o GCCR filer
    value <- calculate_factor_return(ret = returns,
                                     filters_l = filters_l$value,
                                     long_only = long_only)
    
    # Factor Value Survived
    value_surv <- calculate_factor_return(ret = returns,
                                     filters_l = filters_l$value_surv,
                                     long_only = long_only)
    # Factor Value with GCCR filer
    value_gccr <- calculate_factor_return(ret = returns,
                                          filters_l = filters_l$value_gccr,
                                          long_only = long_only)
    
    # Combine xts
    f_perf <- cbind(bm,
                    isin_p_ret,
                    value,
                    mom,
                    size,
                    ind_gccr,
                    value_gccr,
                    mom_gccr,
                    size_gccr,
                    value_surv,
                    mom_surv,
                    size_surv)
    
    colnames(f_perf) <- c("bm","ind","value","mom","size",
                          "ind_gccr","value_gccr","mom_gccr", "size_gccr",
                          "value_surv","mom_surv", "size_surv")
  }
  else{
    # Bonds
    # Combine xts
    f_perf <- cbind(isin_p_ret,
                    mom,
                    size,
                    ind_gccr,
                    mom_gccr,
                    size_gccr,
                    mom_surv,
                    size_surv)
    
    colnames(f_perf) <- c("ind","mom", "size", "ind_gccr",
                          "mom_gccr", "size_gccr",
                          "mom_surv", "size_surv")
  }
  
  return(f_perf)
}
  

visualize <- function(l){
  # The function contains code to plot all required results
  
  library(xts)
  
  plot(l$ratios$USA[,1:4],
       main = "Number of companies after filtration",
       cex.lab=1.5,
       cex.axis=1.5,
       cex.main=1.5,
       cex.sub = 1.5,
       col = c("black", "green", "blue", "red"))
  
  addLegend(legend.loc = "topleft", on = 1,
            legend.names = c("index", "index w/o NA","number of NA", "index w/o NA and 0"),
            lty= rep(1, times = 4),
            lwd= rep(2.5, times = 4),
            col = c("black", "green", "blue", "red"),
            cex = 1.5)
  
  
  plot(l$ratios$USA[,5:6],
       main = "Number of companies after filtration",
       cex.lab=1.5,
       cex.axis=1.5,
       cex.main=1.5,
       legend.loc = "left",
       col = c("black", "red"))
  
  addLegend(legend.loc = "topleft", on = 1,
            legend.names = c("Frequency Ratio", "Market Cap. Ratio"),
            lty= rep(1, times = 2),
            lwd= rep(2.5, times = 2),
            col = c("black", "red"),
            cex = 1.5)
  
  plot(l$ratios$EUR[,1:4],
       main = "Number of companies after filtration",
       cex.lab=1.5,
       cex.axis=1.5,
       cex.main=1.5,
       cex.sub = 1.5,
       col = c("black", "green", "blue", "red"))
  
  addLegend(legend.loc = "topleft", on = 1,
            legend.names = c("index", "index w/o NA","number of NA", "index w/o NA and 0"),
            lty= rep(1, times = 4),
            lwd= rep(2.5, times = 4),
            col = c("black", "green", "blue", "red"),
            cex = 1.5)
  
  plot(l$ratios$EUR[,5:6],
       main = "Number of companies after filtration",
       cex.lab=1.5,
       cex.axis=1.5,
       cex.main=1.5,
       legend.loc = "left",
       col = c("black", "red"))
  
  addLegend(legend.loc = "topleft", on = 1,
            legend.names = c("Frequency Ratio", "Market Cap. Ratio"),
            lty= rep(1, times = 2),
            lwd= rep(2.5, times = 2),
            col = c("black", "red"),
            cex = 1.5)
  
  plot(l$ratios$EM[,1:4],
       main = "Number of companies after filtration",
       cex.lab=1.5,
       cex.axis=1.5,
       cex.main=1.5,
       cex.sub = 1.5,
       col = c("black", "green", "blue", "red"))
  
  addLegend(legend.loc = "topleft", on = 1,
            legend.names = c("index", "index w/o NA","number of NA", "index w/o NA and 0"),
            lty= rep(1, times = 4),
            lwd= rep(2.5, times = 4),
            col = c("black", "green", "blue", "red"),
            cex = 1.5)
  
  plot(l$ratios$EM[,5:6],
       main = "Number of companies after filtration",
       cex.lab=1.5,
       cex.axis=1.5,
       cex.main=1.5,
       legend.loc = "left",
       col = c("black", "red"))
  
  addLegend(legend.loc = "topleft", on = 1,
            legend.names = c("Frequency Ratio", "Market Cap. Ratio"),
            lty= rep(1, times = 2),
            lwd= rep(2.5, times = 2),
            col = c("black", "red"),
            cex = 1.5)
  
  plot(l$ratios$JBonds[,1:4],
       main = "Number of companies after filtration",
       cex.lab=1.5,
       cex.axis=1.5,
       cex.main=1.5,
       cex.sub = 1.5,
       col = c("black", "green", "blue", "red"))
  
  addLegend(legend.loc = "topleft", on = 1,
            legend.names = c("index", "index w/o NA","number of NA", "index w/o NA and 0"),
            lty= rep(1, times = 4),
            lwd= rep(2.5, times = 4),
            col = c("black", "green", "blue", "red"),
            cex = 1.5)
  
  plot(l$ratios$JBonds[,5:6],
       main = "Number of companies after filtration",
       cex.lab=1.5,
       cex.axis=1.5,
       cex.main=1.5,
       legend.loc = "left",
       col = c("black", "red"))
  
  addLegend(legend.loc = "topleft", on = 1,
            legend.names = c("Frequency Ratio", "Market Cap. Ratio"),
            lty= rep(1, times = 2),
            lwd= rep(2.5, times = 2),
            col = c("black", "red"),
            cex = 1.5)
  
  plot(l$ratios$IGBonds[,1:4],
       main = "Number of companies after filtration",
       cex.lab=1.5,
       cex.axis=1.5,
       cex.main=1.5,
       cex.sub = 1.5,
       col = c("black", "green", "blue", "red"))
  
  addLegend(legend.loc = "topleft", on = 1,
            legend.names = c("index", "index w/o NA","number of NA", "index w/o NA and 0"),
            lty= rep(1, times = 4),
            lwd= rep(2.5, times = 4),
            col = c("black", "green", "blue", "red"),
            cex = 1.5)
  
  plot(l$ratios$IGBonds[,5:6],
       main = "Number of companies after filtration",
       cex.lab=1.5,
       cex.axis=1.5,
       cex.main=1.5,
       legend.loc = "left",
       col = c("black", "red"))
  
  addLegend(legend.loc = "topleft", on = 1,
            legend.names = c("Frequency Ratio", "Market Cap. Ratio"),
            lty= rep(1, times = 2),
            lwd= rep(2.5, times = 2),
            col = c("black", "red"),
            cex = 1.5)
  
  # Index return
  chart.CumReturns(l$ind_perf$USA,
                   main = "MSCI Index USA Cumulative Return",
                   cex.lab=1, cex.axis=1, cex.main=1, legend.loc = "left")
  
  charts.PerformanceSummary(l$ind_perf$USA,
                            main = "MSCI Index USA Cumulative Return",
                            cex.lab=1, cex.axis=1, cex.main=1, legend.loc = "left")
  
  chart.CumReturns(l$ind_perf$EUR,
                   main = "MSCI Index USA Cumulative Return",
                   cex.lab=1, cex.axis=1, cex.main=1, legend.loc = "left")
  chart.CumReturns(l$ind_perf$EM,
                   main = "MSCI Index USA Cumulative Return",
                   cex.lab=1, cex.axis=1, cex.main=1, legend.loc = "left")
  chart.CumReturns(l$ind_perf$JBonds,
                   main = "MSCI Index USA Cumulative Return",
                   cex.lab=1, cex.axis=1, cex.main=1, legend.loc = "left")
  chart.CumReturns(l$ind_perf$IGBonds,
                   main = "MSCI Index USA Cumulative Return",
                   cex.lab=1, cex.axis=1, cex.main=1, legend.loc = "left")
  
  # AVG Value scores
  plot(l$avg_scores$USA[,c(10,12)],
       main = "USA: Average GCCR Filtered Value Score",
       cex.lab=1.5, cex.axis=1.5, cex.main=1.5, legend.loc = "left")
  
  plot(l$avg_scores$EUR ,
       main = "USA: Average GCCR Filtered Value Score",
       cex.lab=1.5, cex.axis=1.5, cex.main=1.5, legend.loc = "left")
  
  plot(l$avg_scores$EM,
       main = "USA: Average GCCR Filtered Value Score",
       cex.lab=1.5, cex.axis=1.5, cex.main=1.5, legend.loc = "left")
  
  # Factors Performance
  chart.CumReturns(l$factors_perf$USA, cex.lab=1, cex.axis=1, cex.main=1, legend.loc = "left")
  chart.CumReturns(l$factors_perf$EUR, cex.lab=1, cex.axis=1, cex.main=1, legend.loc = "left")
  chart.CumReturns(l$factors_perf$EM, cex.lab=1, cex.axis=1, cex.main=1, legend.loc = "left")
  chart.CumReturns(l$factors_perf$Bonds, cex.lab=1, cex.axis=1, cex.main=1, legend.loc = "left")
  
}

main <- function(){

  # Paths to containers
  path_usa <- "H:/Alex/MSCIUSASTD"
  path_eur <- "H:/Alex/MSCIEUROPESTD"
  path_em <- "H:/Alex/MSCIEMSTD"
  path_jbonds <-  "H:/Alex/DB20190630"
  path_igbonds <-  "H:/Alex/DB20190630"
  paths_v <- c(path_usa, path_eur, path_em, path_jbonds, path_igbonds)
  u_names <- c("USA", "EUR","EM", "JBonds","IGBonds")
  # paths_v <- c(path_usa)
  # paths_v <- c(path_jbonds, path_igbonds)
  # u_names <- c("JBonds","IGBonds")
  # u_names <- c("USA")

  names_ls <- c("val_l","val_s",
                "mom_l","mom_s",
                "size_l", "size_s")
  
  
  # e_u_names <- u_names[1:3]
  
  res_l_names <- c("ratios",
                   "ind_perf",
                   "factors_perf",
                   "factors_perf_lo",
                   "avg_scores")
  
  gccr_n_comp_l <- list()
  ind_portf_ret_l <- list()
  factors_ret_l <- list()
  factors_ret_long_only_l <- list()
  avg_score_l <- list()

  for(i in 1:length(u_names)){
    
    cat("Working with ", u_names[i], "\n")
    
    filters <- prepare_data(path = paths_v[i], task = u_names[i])
    
    ind_portf_ret <- analyze_index_performance(
      ret = filters$returns,
      isin = filters$isin,
      isin_wo_NA = filters$isin_wo_NA,
      isin_wo_NA0 =  filters$isin_wo_NA0
    )
    
    ind_portf_ret_l[[i]] <- ind_portf_ret
    
    
    res_matrix <- apply_gccr(
      isin = filters$isin,
      isin_gccr = filters$isin_gccr,
      isin_wo_NA = filters$isin_wo_NA,
      isin_wo_NA0 =  filters$isin_wo_NA0,
      mcap = filters$mcap
    )

    gccr_n_comp_l[[i]] <- res_matrix
    
    factor_filters_l <- construct_factor_ls_filters(
      isin_wo_NA0 = filters$isin_wo_NA0,
      isin_wo_NA = filters$isin_wo_NA,
      isin_no_gccr = filters$isin_no_gccr,
      value_s = filters$value,
      mom_s = filters$mom,
      size_s = filters$size,
      per = 0.2,
      path = paths_v[i]
    )
      
    factors_ret <- analyze_factors_performance(
      returns = filters$returns,
      bm = filters$bm,
      isin_p_ret = ind_portf_ret$isin_p_ret,
      filters_l = factor_filters_l,
      path = paths_v[i],
      long_only = FALSE
    )
    
    factors_ret_l[[i]] <- factors_ret
    
    factors_ret_long_only <- analyze_factors_performance(
      returns = filters$returns,
      bm = filters$bm,
      isin_p_ret = ind_portf_ret$isin_p_ret,
      filters_l = factor_filters_l,
      path = paths_v[i],
      long_only = TRUE
    )
    
    factors_ret_long_only_l[[i]] <- factors_ret_long_only

    avg_score <- calculate_avg_ls_score(
      path = paths_v[i],
      value = filters$value,
      mom = filters$mom,
      size = filters$size,
      factor_filters_l = factor_filters_l
    )

    avg_score_l[[i]] <- avg_score
  }

  names(gccr_n_comp_l) <- u_names
  names(ind_portf_ret_l) <- u_names
  names(factors_ret_l) <- u_names
  names(factors_ret_long_only_l) <- u_names
  names(avg_score_l) <- u_names

  
  ret_l <- list(gccr_n_comp_l,
                ind_portf_ret_l,
                factors_ret_l,
                factors_ret_long_only_l,
                avg_score_l)

  names(ret_l) <- res_l_names

  #visualize(ret_l)
  
  return(ret_l)

}

l <- main()

setwd("H:/R project ESG")
save.image(file = "GCCR.Rraw")




