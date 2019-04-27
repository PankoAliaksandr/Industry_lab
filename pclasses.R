#AW new modular approach to prepareResults
# * Callable from backtestTool to generate same results as before (collecting information from envBacktests)
# * Also that retrieve results from envBackte
# * Potentially also results 

#### Functions mostly used in html report generation _________________________________________________________####
getStandardParametersFromResultsFile <- function(resultsFilePath = "standard", 
                                                 envResName = "envResults") {
  if (resultsFilePath == "standard") {
    resFile <- file.path(globalParams$exp_path,globalParams$backtest_name,"envResults.Rdata")
  } else {
    resFile <- resultsFilePath
  }
  if (!is.character(resFile)) stop ("result file must be given as character")
  if (!file_test("-f", resFile)) stop ("results file cannot be found")
  
  load(resFile)
  if (!is.environment(get(envResName))) stop("results file must contain the results as environment")
  envR <- get(envResName)
  
  parsForDashboard <- list(
    envResName             = envResName,
    # ParamFile              = envR[["envConfig"]][["path.paramFile"]],       #"params.PAA.EQT.LiveRemote"
    # backtest_name          = envR[["globalParams"]][["backtest_name"]],     # "PAA_EQT_1701_NoChile"
    # exp_pathR              = envR[["globalParams"]][["exp_path"]],          # "X:\\Research\\Modelle\\BSCV\\results\\"
    psNames                = envR[["params.nn"]],
    allnames               = envR[["allnames"]],
    # path.reportDirectory   = envR[["envConfig"]][["path.reportDirectory"]],
    # path.backtestDirectory = envR[["envConfig"]][["path.backtestDirectory"]],
    # prepare_results_params = envR[["globalParams"]][["prepare_results_params"]],
    # report_params          = envR[["globalParams"]][["report_params"]]
    # globalParams           = envR[["globalParams"]]
    envConfig              = envR[["envConfig"]],
    ParamSet               = envR[["ParamSet"]],
    globalParams           = envR[["globalParams"]]
  )
  return(parsForDashboard)
}

# AW dataContainer to Matrix (with dates as rownames)
dc2mat <- function(dc) {
  if (!"dataContainer" %in% attr(dc, "class")) {
    warning("Attempt to convert dataContainer to matrix ignored")
    return(dc)
  }
  m <- dc[["data"]]
  rownames(m) <- as.character(dc[["date"]])
  return(m)
}



#AW For use in report generation
datesIfCommon <- function(matlist) {
  dates <- as.Date(rownames(matlist[[1]]))
  if (length(matlist) > 1) {
    allDatesEqual <- all(sapply(matlist[-1], FUN = function(mat) {
      identical(as.Date(rownames(mat)), dates)
    }))
    if (!allDatesEqual) stop("rownames of matrices do not match as dates")
  }
  return(dates)
}

#AW For use in report generation
firstColIfCommon <- function(mat, newColName = NULL, drop = TRUE) {
  if (!all(round(na.omit(mat), 10)[ , 1]==round(na.omit(mat), 10))) {
    # return(NULL)
    stop("entries in bm or rf do not match for all param sets")
  }
  m1 <- mat[ ,1, drop=drop]
  if (drop == FALSE && !is.null(newColName)) {
    colnames(m1) <- newColName
  }
  return(m1)
}

#AW For use in report generation
addHtmlLineBreaks <- function(vectorString, breakAfter = 10) {
  addSingle <- function(singleString, breakAfter = 10) {
    singleSplit <- strsplit(singleString, 
                            paste0("(?<=.{",breakAfter,"})"), 
                            perl=TRUE)
    return(paste(unlist(singleSplit), collapse="<br/>"))
  }
  sapply(vectorString, FUN = addSingle, breakAfter = breakAfter)
}

#AW For use in report generation
# adapted formattable function from https://stackoverflow.com/questions/48870990/r-formattable-apply-two-format-to-an-area
normalize2 <- function (x, min = 0, max = 1, na.rm = FALSE, typMin = 0, typMax = 0.1) 
{
  if (all(is.na(x))) 
    return(rep(0, length(x)))
  if (!is.numeric(x)) 
    stop("x must be numeric")
  x <- unclass(x)
  if (min > max) 
    stop("min <= max must be satisfied")
  if (all(x == 0, na.rm = na.rm)) 
    return(x)
  # xmax <- max(x, na.rm = na.rm)  #AW
  xmax <- max(c(x, typMax), na.rm = na.rm)  #AW
  # xmin <- min(x, na.rm = na.rm)
  xmin <- min(c(x, typMin), na.rm = na.rm)  #AW
  if (xmax == xmin) 
    return(rep(1, length(x)))
  min + (max - min) * (x - xmin)/(xmax - xmin)
}

#AW For use in report generation
# adapted formattable function from https://stackoverflow.com/questions/48870990/r-formattable-apply-two-format-to-an-area
color_bar2  <- function(color = "lightgray", fun = "comma", digits = 0, typMin = 0, typMax = 0.1) {
  fun <- match.fun(fun)
  formatter("span", x ~ fun(x, digits = digits),
            style = function(y) style(
              display = "inline-block",
              # direction = "rtl", #aw
              "border-radius" = "4px",
              "padding-right" = "2px",
              # "background-color" = csscolor(color),
              "background-color" = color,  #AW
              width = percent(normalize2(as.numeric(y), na.rm = TRUE, typMin = typMin, typMax = typMax)) #AW
              # width = percent(proportion(as.numeric(y), na.rm = TRUE))
              # width = percent(normalize(as.numeric(y), na.rm = TRUE)) #AW
            )
  )
}

#AW For use in report generation
# custom function to transpose data frame while preserving names
transpose_df <- function(df) {
  t_df <- data.table::transpose(df)
  colnames(t_df) <- rownames(df)
  rownames(t_df) <- colnames(df)
  return(t_df)
}

#AW For use in report generation

# adapted from https://stackoverflow.com/questions/41196823/embed-csv-in-html-rmarkdown)
# rData can be pasted similarly https://rmarkdown.rstudio.com/articles_rdata.html 
# ToDo: extract from plotly, ggplot
embed_data= function(x= mtcars, filename= "file.csv", label= "download"){
  
  # Create encoded Base64 datastream
  encode_data= function(x){
    write.csv2(x, "./file.csv")
    enc= sprintf('data:application/vnd.ms-excel;base64,%s', openssl::base64_encode(paste0(readLines("./file.csv"), collapse="\n")) )
    # enc= sprintf('data:text/csv;base64,%s', openssl::base64_encode(paste0(readLines("./file.csv"), collapse="\n")) )
    unlink("./file.csv")
    return(enc)
  }
  
  # String result ready to be placed in rmarkdown
  # paste0("<a download='", filename, "' href=", encode_data(x), ">", label, "</a>")
  paste0("<a download='", filename, "' href=", encode_data(x), ">", label, "</a>")
}


### Benchmark replacement _______________________________________________________________####

replaceBenchmarkByDSquery <- function(replaceBmBy, alldates, colname = "newBM",
                                      bmDates=alldates, TriRdsFile=NULL) {
  if (!is.null(TriRdsFile) && file.exists(TriRdsFile)) {
    bmTri <- readRDS(TriRdsFile)
  } else {
    bmTri <- getDailyTotalRetIndexFromDS(names(replaceBmBy), saveAsRDSfile = TriRdsFile)
  }
library(tidyquant)
  bmInd <- bmTri[bmTri$Date %in% as.Date(alldates), ] %>% 
    dplyr::group_by(uid) %>%
    tidyquant::tq_mutate(select = Data,
                         mutate_fun = periodReturn,
                         period = "daily",
                         leading = FALSE,
                         col_rename = "ret") %>%
    dplyr::select(uid, Date, ret)  %>%
    tidyr::spread(key=uid, value=ret) %>%
    timetk::tk_xts(silent=TRUE)
  bmRet <- PerformanceAnalytics::Return.portfolio(R = bmInd[ , names(replaceBmBy)], 
                                                  weights = replaceBmBy,
                                                  rebalance_on = "days",
                                                  verbose = FALSE)
  bmRet[1, ]      <- NA # is wrong zero otherwise
  bmRet           <- as.matrix(bmRet)
  bmRet           <- bmRet[rownames(bmRet) %in% bmDates, , drop=FALSE]
  colnames(bmRet) <- colname
  return(bmRet)
}


getDailyTotalRetIndexFromDS <- function(DSID, DSTYPE = "RI~E0", DSFROM = "1969-12-31", 
                                        DSTO = as.character(Sys.Date()), DSFREQ = "D", Update = TRUE,
                                        saveAsRDSfile = NULL)
{
library(assertr)
  DSRequestTable <-
    data.frame(DSID, DSTYPE, DSFROM, DSTO, DSFREQ, ID=DSID, Update) %>% 
    dplyr::filter(Update==TRUE) %>% 
    dplyr::select(DSID, DSTYPE, DSFROM, DSTO, DSFREQ, ID, Update)
  
  DSDataList <- try(getDSHistData(DSRequestTable), silent = FALSE)
  
  if(class(DSDataList)[[1]]=='try-error') {
    stop("Error ocured while dowloading DS time series data.")
  }
  if(nrow(DSRequestTable) != length(DSDataList)) {
    stop('Inconsistent dimensions of DSDataList and DSRequestTable!')
  }
  names(DSDataList) <- DSRequestTable[["DSID"]]
  dfl <-
    dplyr::bind_rows(DSDataList, .id = "uid")  %>%
    dplyr::mutate(Data = if_else(Data == "NaN", NA_character_, Data))   %>%
    assertr::assert(function(x)  (!grepl(x, pattern="[[:alpha:]]")), Data) %>%
    # confirm that no letters (from DS failures) are present
    dplyr::mutate(Date = as.Date(Date))   %>%
    dplyr::mutate(Data = as.numeric(Data)) %>%
    dplyr::filter(!is.na(Data))
  
  if (!is.null(saveAsRDSfile)) {
    saveRDS(object = dfl, file = saveAsRDSfile) # "H:/Temp/test.rds"
    writeLines(paste0("Benchmark Daily Total Return Indices saved as ", saveAsRDSfile))
  } 
  return(dfl)
}




### Create data frame for transforming result matrices _____________________________________________________####
createTransDF <- function(windowNames = "standard")  {

  if (length(windowNames)==1 && windowNames == "standard") {
    wNames      <- c("all", "3M", "1Y", "3Y", "5Y", "exp", "yearly", "YTD")
  } else if (length(windowNames)==1 && windowNames == "noRoll") {
    wNames      <- c("all", "exp", "yearly", "YTD")
  } else if ("all" %in% windowNames && 
             all(windowNames %in%  c("all", "3M", "1Y", "3Y", "5Y", "exp", "yearly", "YTD"))) {
    wNames      <- windowNames
  } else stop("window names provided to createTransDF function are (partly) invalid")
  
  retNames    <- c("pf.ret", "pf.ret.tc", "bm.ret", "bm.ret.tc", "rf.ret",
                   "pf.exretbm", "pf.exretbm.tc", "pf.exretrf", "pf.exretrf.tc", 
                   "bm.exretrf", "bm.exretrf.tc")
  

  df <- data.frame(stringsAsFactors=FALSE,
                 newName = c("pf.tc", "bm.tc", "pf.exretbm", "pf.exretbm.tc", "pf.exretrf",
                             "pf.exretrf.tc", "bm.exretrf", "bm.exretrf.tc",
                             "pf.perf", "pf.perf.tc", "bm.perf", "bm.perf.tc", "rf.perf",
                             "pf.experfbm", "pf.experfbm.tc", "pf.experfrf",
                             "pf.experfrf.tc", "bm.experfrf", "bm.experfrf.tc"),
                 fcts = c("-", "-", "-", "-", "-", "-", "-", "-",
                          "calculate.performance", "calculate.performance",
                          "calculate.performance", "calculate.performance",
                          "calculate.performance", "calculate.performance",
                          "calculate.performance", "calculate.performance", "calculate.performance",
                          "calculate.performance", "calculate.performance"),
                 mainSrs = c("pf.ret", "bm.ret", "pf.ret", "pf.ret.tc", "pf.ret",
                             "pf.ret.tc", "bm.ret", "bm.ret.tc", "pf.ret",
                             "pf.ret.tc", "bm.ret", "bm.ret.tc", "rf.ret", "pf.exretbm",
                             "pf.exretbm.tc", "pf.exretrf", "pf.exretrf.tc", "bm.exretrf",
                             "bm.exretrf.tc"),
                 auxSrs = c("pf.ret.tc", "bm.ret.tc", "bm.ret", "bm.ret.tc", "rf.ret",
                            "rf.ret", "rf.ret", "rf.ret", NA, NA, NA, NA, NA, 
                            NA, NA, NA, NA, NA, NA),
                 windowPar = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                               NA, NA, NA, NA),
                 funPar = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                            NA, NA, NA, NA)
  )
  
  # perf2ret
  df.perf2ret <- data.frame(stringsAsFactors = FALSE,
                            # newName   = do.call("paste", c(expand.grid(retNames, wNames), sep=".")),
                            newName   = do.call("paste", c(expand.grid(
                              c("pf.ret", "pf.ret.tc", "bm.ret", "bm.ret.tc", "rf.ret"), wNames), sep=".")),
                            fcts      = "statByWindow",
                            mainSrs   = rep(c("pf.perf", "pf.perf.tc", "bm.perf", "bm.perf.tc", "rf.perf"), length(wNames)),
                            auxSrs    = NA,
                            windowPar = rep(wNames, each=5),
                            funPar    = "ret")
  # subtract Window Names
  df.exret <- data.frame(stringsAsFactors = FALSE,
                         newName   = do.call("paste", c(expand.grid(df$newName[3:8], wNames), sep=".")),
                         fcts      = "-",
                         mainSrs   = do.call("paste", c(expand.grid(df$mainSrs[3:8], wNames), sep=".")),
                         auxSrs    = do.call("paste", c(expand.grid(df$auxSrs [3:8], wNames), sep=".")),
                         windowPar = NA,
                         funPar    = NA)
  # ret2vola
  df.ret2vola <- data.frame(stringsAsFactors = FALSE,
                            newName   = do.call("paste", c(expand.grid(sub("ret", "vola", retNames), wNames), sep=".")),
                            fcts      = "statByWindow",
                            mainSrs   = rep(retNames, length(wNames)),
                            auxSrs    = NA,
                            windowPar = rep(wNames, each=length(retNames)),
                            funPar    = "vola")
  # sum
  df.sumSelf  <- data.frame(stringsAsFactors = FALSE,
                            newName   = do.call("paste", c(expand.grid(c("pf.tc", "pf.tickets", "pf.turnover"), wNames), sep=".")),
                            fcts      = "statByWindow",
                            mainSrs   = rep(c("pf.tc", "pf.tickets", "pf.turnover"), length(wNames)),
                            auxSrs    = NA,
                            windowPar = rep(wNames, each=3),
                            funPar    = rep(c("tc", "tickets", "turnover"), length(wNames)))
  # max drawdown
  df.perf2mdd <- data.frame(stringsAsFactors = FALSE,
                            newName   = do.call("paste", c(expand.grid(sub("ret", "mdd", retNames), wNames), sep=".")),
                            fcts      = "statByWindow",
                            mainSrs   = rep(sub("ret", "perf", retNames), length(wNames)),
                            auxSrs    = NA,
                            windowPar = rep(wNames, each=length(retNames)),
                            funPar    = "mdd")
  df.ir    <- data.frame(stringsAsFactors = FALSE,
                         newName   = do.call("paste", c(expand.grid(c("pf.ir"     , "pf.ir.tc"     ), wNames), sep=".")),
                         fcts      = "/",
                         mainSrs   = do.call("paste", c(expand.grid(c("pf.exretbm" , "pf.exretbm.tc") , wNames), sep=".")),
                         auxSrs    = do.call("paste", c(expand.grid(c("pf.exvolabm", "pf.exvolabm.tc"), wNames), sep=".")),
                         windowPar = NA,
                         funPar    = NA)
  df.sr    <- data.frame(stringsAsFactors = FALSE,
                         newName   = do.call("paste", c(expand.grid(
                           c("pf.sharpe"     , "pf.sharpe.tc", "bm.sharpe", "bm.sharpe.tc"), wNames), sep=".")),
                         fcts      = "/",
                         mainSrs   = do.call("paste", c(expand.grid(
                           c("pf.exretrf", "pf.exretrf.tc", "bm.exretrf", "bm.exretrf.tc"), wNames), sep=".")),
                         auxSrs    = do.call("paste", c(expand.grid(
                           c("pf.vola", "pf.vola.tc", "bm.vola", "bm.vola.tc"), wNames), sep=".")),
                         windowPar = NA,
                         funPar    = NA)

  return(rbind(df, df.perf2ret, df.exret, df.ret2vola, df.sumSelf, df.perf2mdd, df.ir, df.sr ))
}

#### Individual calculations ______________________________________________________________________ ####
"geomean" <- function(x,na.rm=FALSE){
	
	if(na.rm){
		sel <- !is.na(x) & !is.nan(x)
		x <- x[sel]
	}
	ret <- prod(x)^(1/length(x))
	return(ret)
	
}

#AW Total net return over full (window) period
periodRet <- function(x) {
  x[length(x)]/x[1]
}


shiftIt<-function(data,lag){
	# print(nrow(data))
	# print(ncol(data))
	# print(lag)
	
	if(!(is.data.frame(data)||is.array(data)||is.vector(data)||is.matrix(data))) {
		print("cannot shift the elements of this object")
		return(NULL)
	}
	if(is.data.frame(data)) {
		mx = as.matrix(data)
	}
	else if(is.array(data)) {
		mx = as.matrix(data)
	}
	else if(is.vector(data)) {
		mx = as.matrix(data)
	}
	colnames(mx)<-NULL
	rownames(mx)<-NULL
	
	if(lag==0){
		return(mx)
	}
	if(abs(lag)>=nrow(mx)){
		ret=matrix(NA,nrow=nrow(mx),ncol=ncol(mx))
		return(ret)
	}
	if(lag>0){
		ret=matrix(mx[1:(nrow(mx)-lag),],ncol=ncol(mx))
		ret=rbind(matrix(NA,nrow=lag,ncol=ncol(mx)),ret)
	}
	else{
		lag=-lag
		ret=matrix(mx[(lag+1):nrow(mx),],ncol=ncol(mx))
		ret=rbind(ret,matrix(NA,nrow=lag,ncol=ncol(mx)))
	}
	if(is.data.frame(data)||is.matrix(data)) {
		colnames(ret) = colnames(data)
	}
	
	return(ret)
}

#'AW dropped unused paramters
"mdd" <- function(x) {
	return(-min(x/cummax(x)-1))
}

"calculate.returns.byWindow" <- function(perf, alldates, windows) {
	ret <- matrix(NA, nrow(windows), ncol(perf))
	rownames(ret) <- rownames(windows)
	colnames(ret) <- colnames(perf)
	
	for(tind in 1:nrow(windows)) {
		start.ind <- which(alldates %in% windows[tind, "start"])
		end.ind <- which(alldates %in% windows[tind, "end"])
		ret[tind, ] <- perf[end.ind,]/perf[start.ind,]-1
	}
	return(ret)
}

"calculate.performance" <- function(dcRet) {
  if (is.vector(dcRet)) dcRet <- as.matrix(dcRet)
	first <- as.matrix(apply(dcRet, 2, function(x){min(which(!is.na(x)))}))
	last  <- as.matrix(apply(dcRet, 2, function(x){max(which(!is.na(x)))}))
	
	rr <-1 + dcRet	
	for(iCol in 1:ncol(dcRet)) {
		if(first[iCol]>1) {
			rr[1:(first[iCol]-1),iCol] <- 1
		}
	}
	
	perf <- apply(rr, 2, cumprod)
	
	
	for(iCol in 1:ncol(perf)) {
		if(first[iCol]>2) {
			perf[1:(first[iCol]-2),iCol] <- NA
		}
	}
	
	start.ind <- as.matrix(apply(perf, 2, function(x){min(which(!is.na(x)))}))
	end.ind <- as.matrix(apply(perf, 2, function(x){max(which(!is.na(x)))}))
	if(max(start.ind) < min(end.ind)) {
		reference.ind <- max(start.ind)
		perf.renorm <- apply(perf, 1, function(x) {return(x/perf[reference.ind,,drop=F])})
	} else {
		perf.renorm <- perf
	}
	if(ncol(rr)>1) {
		perf.renorm <- as.matrix(t(perf.renorm))
	} else {
		perf.renorm <- as.matrix(perf.renorm)
	}

	colnames(perf.renorm) <- colnames(dcRet)
	rownames(perf.renorm) <- rownames(dcRet)
	return(perf.renorm)

}

"calculate.movingWindow" <- function(df, window, scale.year=365.24, FUN=mean, ...) {
	ret <- matrix(NA, nrow(window), ncol(df))
	rownames(ret) <- rownames(window)
	colnames(ret) <- colnames(df)	
	for(j in 1:ncol(df)) {	
		#start.data <-  min(which(!is.na(df[,j])))
		ind <- pmatch(window[,2],window[,1], duplicates.ok=TRUE)
		#ind[ind < start.data] <- NA		
		if(!is.na(scale.year)) {
			for(i in which(!is.na(ind))) {
				current.window <- ind[i]:i
				xx <- df[current.window,j]
				if(any(!is.na(xx))) {
					scale <- as.numeric(window[i,1]-window[i,2])/scale.year
					ret[i,j] <- FUN(xx,scale,...)
					
				}
			}
		} else {
			for(i in which(!is.na(ind))) {
				current.window <- ind[i]:i
				xx <- df[current.window,j]
				if(any(!is.na(xx))) {
					
					ret[i,j] <- FUN(xx,...)
					
				}
			}
		}
	}
	return(ret)
}

"calculate.beta" <- function(pf.ret, bm.ret, alldates, window, 
                             regressorNumber = 2, # 1 is intercept, 2 first "beta" etc.
                             returnPValue     = FALSE #AW
                             ) {
  if(is.vector(pf.ret)) pf.ret <- as.matrix(pf.ret)
  if(is.vector(bm.ret)) bm.ret <- as.matrix(bm.ret)
  if(is.vector(window)) window <- as.matrix(window)
  
  bbb <- matrix(NA, nrow(window), ncol(pf.ret))
	rownames(bbb) <- rownames(window)
	colnames(bbb) <- colnames(pf.ret)
	try({
	for(tind in 1:nrow(window)) {
		current.window <- window[tind,]
		if(all(!is.na(current.window))) {			
			# ind <- (alldates >= current.window[1]) & (alldates <= current.window[2]) #AW
			ind <- (alldates >= current.window[[1]]) & (alldates <= current.window[[2]]) #AW
			if(any(ind)) {
				for(j in 1:ncol(pf.ret)) {
				  reg <- lm(pf.ret[,j]~bm.ret[,j],subset=ind)
			    if (returnPValue == FALSE) {
			      bbb[tind,j] <- coefficients(reg)[regressorNumber]
			    } else {
    				bbb[tind,j] <- summary(reg)[["coefficients"]][, "Pr(>|t|)"][regressorNumber]
				  }
				}
			}
		} 
	}
})->err
	if(identical(class(err),"try-error")) browser()
	return(bbb)
}

#### Meta Window Functions _________________________________________________________________ ####
# AW Added default calculations for alldates and windows 
calculate.byWindow <- function(data, alldates=NULL, windows, FUN=sum, ...) {
  ## AW Addition start
  if (!is.matrix(data)) stop("data needs to be a matrix")
  if (is.null(alldates)) {
    if (is.null(rownames(data)))             stop("rownames missing in data")
    if (any(is.na(as.Date(rownames(data))))) stop("rownames not convertible to dates")
    alldates <- as.Date(rownames(data))
  }
  if (is.character(windows)) {
    stopifnot(windows %in% c("all", "3m", "1Y", "3Y", "5Y", "exp", "yearly", "YTD"))
    windows <- windowByName(alldates, windowName = windows)
  }
  #AW Addition end
  ret <- matrix(NA, nrow(windows), ncol(data))
  rownames(ret) <- rownames(windows)
  for(ind in 1:nrow(windows)) {
    if(!is.na(windows[ind,"start"])&!is.na(windows[ind,"end"])) {
      start.ind <- which(alldates %in% windows[ind, "start"])
      end.ind <- which(alldates %in% windows[ind, "end"])
      ret[ind, ] <- apply(data[start.ind:end.ind,,drop=F],2, FUN, ...)
    }
  }
  colnames(ret) <- colnames(data)
  return(ret)
}


"calculate.windows"  <- function(dates, years=1, start.ind=NA) {
	window.ind <- 1
	timediff.years <- c(0,diff(as.numeric(dates))/365.24)
	window.dates <- data.frame(dates)
	
	current.window <- dates
	if(years==Inf) {			
		if(is.na(start.ind)) {
			current.window[1:length(current.window)] <- dates[1]
		} else {
			current.window[1:length(current.window)] <- dates[start.ind]
			
			if(start.ind>1) current.window[1:(start.ind-1)] <- NA
		}
	} else {
		if(is.na(start.ind)) start.ind<-1
		for(tind in (start.ind):length(dates)) {
			window.range <- window.ind:tind
			tt.diff <- timediff.years[window.ind+1]
			if(sum(timediff.years[window.range]) >= years-tt.diff/2) {
				current.window[tind] <- dates[window.ind]
				window.ind <- window.ind+1
			} else {
				current.window[tind] <- NA
			}
		}
	}
	window.dates <- cbind(window.dates,current.window)
	
	rownames(window.dates) <- as.character(dates)
	yy.labels <- sprintf("%.fY", years)
	if(years==Inf) yy.labels <- "all"
	colnames(window.dates) <- yy.labels
	return(window.dates)
}

"create.yearlywindows" <- function(dates) {
	y <- unique(format(dates,"%Y"))
	pmatch(y, format(dates,"%Y"))
	start.dates <- dates[pmax(pmatch(y, format(dates,"%Y"))-1,1)]
	end.dates  <-  c(start.dates[-1],dates[length(dates)])
	df <- data.frame(start=start.dates,end=end.dates)
	rownames(df) <- sprintf("%s", y)
	rownames(df)[1] <- sprintf("%s (ITD)", y[1])
	if(nrow(df) > 1) {
		rownames(df)[nrow(df)] <- sprintf("YTD")
	} else {
		rownames(df)[1] <- sprintf("ITD")
	}
	return(df)
}

"create.yearlyYTDwindows" <- function(dates) {
	y <- unique(format(dates,"%Y"))
	pmatch(y, format(dates,"%Y"))
	start.dates <- dates[pmax(pmatch(y, format(dates,"%Y"))-1,1)]
	end.dates  <-  rep(dates[length(dates)], length(start.dates))
	df <- data.frame(start=start.dates,end=end.dates)
	rownames(df) <- sprintf("%s-", y)
	rownames(df)[1] <- "all"
	rownames(df)[nrow(df)] <- sprintf("YTD")
	return(df)
}


"create.movingwindows" <- function(dates, years=1, start.ind=NA) {
	window.ind <- 1
	timediff.years <- c(0,diff(as.numeric(dates))/365.2425)
	window.dates <- data.frame(dates)
	
	current.window <- dates
	if(years==Inf) {			
		if(is.na(start.ind)) {
			current.window[1:length(current.window)] <- dates[1]
		} else {
			current.window[1:length(current.window)] <- dates[start.ind]
			
			if(start.ind>1) current.window[1:(start.ind-1)] <- NA
		}
	} else {
		if(is.na(start.ind)) start.ind<-1
		for(tind in (start.ind):length(dates)) {
			window.range <- window.ind:tind
			tt.diff <- timediff.years[window.ind+1]
			if(sum(timediff.years[window.range]) >= years-tt.diff/2) {
				current.window[tind] <- dates[window.ind]
				window.ind <- window.ind+1
			} else {
				current.window[tind] <- NA
			}
		}
	}
	window.dates <- cbind(current.window,window.dates)
	
	rownames(window.dates) <- as.character(dates)
	yy.labels <- c("start","end")
	
	colnames(window.dates) <- yy.labels
	#window.dates <- window.dates[apply(window.dates,1,function(x) all(!is.na(x))),]
	return(window.dates)
}

#' alldates as string or as date vector
windowByName <- function(alldates, 
                         windowName=c("all", "3M", "1Y", "3Y", "5Y",
                                      "exp", "yearly", "YTD")) {
  if (any(is.na(as.Date(alldates)))) stop("alldates not (convertible to) dates")
  alldates   <- as.Date(alldates)
  windowName <- match.arg(windowName)
  switch(windowName,
         "all"   = data.frame(start=alldates[1], end=alldates[length(alldates)]),
         "3M"    = create.movingwindows(alldates, years=0.25),
         "1Y"    = create.movingwindows(alldates, years=1),
         "3Y"    = create.movingwindows(alldates, years=3),
         "5Y"    = create.movingwindows(alldates, years=5),
         "exp"   = create.movingwindows(alldates, years=Inf),
         "yearly"= create.yearlywindows(alldates),
         "YTD"   = create.yearlyYTDwindows(alldates)
  )
}

scaleByName <- function(alldates, windowName=c("plain", "yearly", "YTD")) {
  windowName <- match.arg(windowName)
  if (any(is.na(as.Date(alldates)))) stop("alldates not (convertible to) dates")
  alldates   <- as.Date(alldates)
  # Attention, "plain" uses scale function recursily!
  switch(windowName ,
         "plain"  = {sc <- mean(timestepsByName(alldates, "yearly") / 
                                  scaleByName(alldates, "yearly"), 
                                na.rm=TRUE) }, # na.rm removes zero obs per year in denominator
         "yearly" = {sc <- calculate.byWindow(as.matrix(alldates), alldates,
                                              windowByName(alldates, "yearly"), 
                                              FUN = function(x) as.numeric(x[length(x)]-x[1])/365.2425) 
         colnames(sc) <- "years"},
         "YTD"    = {sc <- pmin(1,1/yearsByName(alldates, "YTD"))} )
  return(sc)
}

yearsByName <- function(alldates, windowName=c("plain", "YTD")) {
  windowName <- match.arg(windowName)
  if (any(is.na(as.Date(alldates)))) stop("alldates not (convertible to) dates")
  alldates   <- as.Date(alldates)
  switch(windowName,
         "plain"  = as.numeric(alldates[length(alldates)] - alldates[1])/365.2425,
         "YTD"    = {calculate.byWindow(data     = as.matrix(alldates),
                                        alldates = alldates, 
                                        windows = windowByName(alldates, "YTD"), 
                                        FUN=function(x) as.numeric(x[length(x)]-x[1])/365.2425)}
  )
} 

timestepsByName <- function(alldates, windowName=c("yearly", "YTD")) {
  windowName <- match.arg(windowName)
  if (any(is.na(as.Date(alldates)))) stop("alldates not (convertible to) dates")
  alldates   <- as.Date(alldates)
  ts <- calculate.byWindow(data = as.matrix(1:length(alldates)), alldates,
                           windows = windowByName(alldates, windowName),
                           FUN=function(x) x[length(x)]-x[1])
  colnames(ts) <- "timesteps"
  return(ts)
}

#' wrapper around calculate.byWindow to allow post-processing (scaling, renaming)
statByWindow <- function(data, alldates = NULL, 
                         windowName=c("all", "3M", "1Y", "3Y", "5Y", "exp", "yearly", "YTD"),
                         FUNname = c("ret", "vola", "te", "tc", "turnover", "tickets", "mdd"),
                         ...) {
  if (is.vector(data)) data <- as.matrix(data)
    # if (!is.matrix(data)) stop("data needs to be a matrix")
  if (is.null(alldates)) {
    if (is.null(rownames(data)))             stop("rownames missing in data")
    if (any(is.na(as.Date(rownames(data))))) stop("rownames not convertible to dates")
    alldates <- as.Date(rownames(data))
  }
  # alldates <- as.Date(rownames(data))
  windowName <- match.arg(windowName)
  windows <- windowByName(alldates, windowName = windowName)
  FUNname <- match.arg(FUNname)
  switch(FUNname,
         "ret"      = {fun <- function(x) x[length(x)]/x[1]},
         "vola"     = {fun <- function(x) sd(x[-1])},
         "te"       = {fun <- function(x) sd(x[-1])},
         "tc"       = {fun <- function(x) sum(x,na.rm=TRUE)},
         "turnover" = {fun <- function(x) sum(x,na.rm=TRUE)},
         "tickets"  = {fun <- function(x) sum(x,na.rm=TRUE)},
         "mdd"      = {fun <- mdd}
  )
  
  preProc <- calculate.byWindow(data, alldates, windows, FUN = fun, ...)
  
  # RETURN post-processing
  if (FUNname == "ret") {
    if (windowName %in% c("3M", "1Y", "yearly")) {
      postProc <- preProc - 1
    } else if (windowName == "3Y" ) {
      postProc <- preProc^(1/3) - 1
    } else if (windowName == "5Y" ) {
      postProc <- preProc^(1/5) - 1
    } else if (windowName == "all") {
      postProc <- preProc^(1/yearsByName(alldates, "plain")) - 1
    } else if (windowName == "exp") {
      oneOverY <- 1 / apply(windows, MARGIN = 1, yearsByName, windowName = "plain")
      postProc <- sweep(preProc, MARGIN = 1, STATS = oneOverY, FUN = "^") - 1
    }
  } else if (FUNname %in% c("vola", "te")) {
    # VOLA & Tracking Error post-processing
    if (windowName %in% c("all", "3M", "1Y", "3Y", "5Y", "yearly")) {
      postProc <- preProc * sqrt(scaleByName(alldates, "plain"))
    } else if (windowName == "exp") {
      alldatesExp <- lapply(1:length(alldates), function(rw) alldates[1:rw])
      sqrtScale <- sqrt(sapply(alldatesExp, scaleByName, windowName = "plain"))
      postProc  <- sweep(preProc, MARGIN = 1, STATS = sqrtScale, FUN = "*")
    }
    else if (windowName == "YTD") {
      postProc <- matrix(apply(preProc, MARGIN=2, 
                               FUN = function(x) x * sqrt(timestepsByName(alldates, "YTD")/
                                                            yearsByName(alldates, "YTD"))),
                         nrow = nrow(windowByName(alldates, "YTD")), 
                         ncol = ncol(data))
      colnames(postProc) <- colnames(data)
      rownames(postProc) <- rownames(windowByName(alldates, "YTD"))      
    }
  } else if (FUNname %in% c("turnover", "tickets", "tc")) {
    # TURNOVER, TICKETS, TRANSACTION COSTS post-processing
    if (windowName %in% c("3M", "1Y", "yearly")) {
      postProc <- preProc 
    } else if (windowName == "all") {
      postProc <- preProc / yearsByName(alldates)
    } else if (windowName == "3Y" ) {
      postProc <- preProc / 3
    } else if (windowName == "5Y" ) {
      postProc <- preProc / 5
    } else if (windowName == "exp") {
      oneOverY <- 1 / apply(windows, MARGIN = 1, yearsByName, windowName = "plain")
      postProc <- sweep(preProc, MARGIN = 1, STATS = oneOverY, FUN = "*")
    }
  } else if (FUNname == "mdd") {
    postProc <- preProc
  }
  # Max Drawdown
  # SCALING of YTD for Returns, Turnover, Tickets, Transaction costs
  if (FUNname %in% c("ret", "turnover", "tickets", "tc") && windowName == "YTD") {
    postProc <- matrix(apply(preProc, MARGIN=2, 
                             FUN = function(x) x^scaleByName(alldates, "YTD")),
                       nrow = nrow(windowByName(alldates, "YTD")), 
                       ncol = ncol(data))
    colnames(postProc) <- colnames(data)
    rownames(postProc) <- rownames(windowByName(alldates, "YTD"))      
  }
  return(postProc)
}

routeToWindowAndOtherFunctions <- function(f, A, B, w, fp, envObj) {
  objA      <- get(A, envir = envObj)
  if (is.na(B)) {
    if (f == "statByWindow") {
        if (is.matrix(objA)) {
          return(do.call(match.fun(f), list(objA, windowName = w, FUNname = fp )))
        } else {
          return(lapply(objA, match.fun(f), windowName = w, FUNname = fp))
        }
      } else if (f == "calculate.performance") {
        if (is.matrix(objA)) {
          return(do.call(match.fun(f), list(objA)))
        } else {
          return(lapply(objA, match.fun(f)))
        }
      } else {
        stop("no valid function name provided")
      }
  } else {
    if (is.matrix(objA)) {
      return(do.call(match.fun(f), list(objA, get(B, envir=envObj))))
    } else { 
      #if (B=="rf.ret") browser()
      return(mapply(match.fun(f), objA,  get(B, envir=envObj), SIMPLIFY = FALSE))
    }
  }
  

}

# forRes$pf.tc      <-  mapply("-", forRes[["pf.ret"]], forRes[["pf.ret.tc"]])
# forRes$pf.perf    <-  lapply(forRes[["pf.ret"]], calculate.performance)
# forRes$pf.ret.all <-  lapply(forRes[["pf.perf"]], statByWindow, alldates = NULL, windowName = "all", FUNname = "ret")



#### Extract and Prepare from workspaces ________________________________________________ ####
#' Returns list of objects with given names/locations collected from saved workspaces
extractResults <- function(files, vars, params.nn = "fromParams") {
  if (params.nn != "fromParams") {
    stopifnot(is.character(params.nn) || length(params.nn)==length(files))
  } else {
    stopifnot("p$backtestParams$paramset_name" %in% vars)
  }
  outList <- lapply(vars, function(vs) {
    vector(mode="list", length=length(files))
  })
  for (ff in seq_along(files)) {
    extractEnv <<- new.env() # note double assignmetn operator
    load(files[ff], envir=extractEnv)
    for (vv in seq_along(vars)) {
      outList[[vv]][[ff]] <- eval(parse(text=paste0("extractEnv$",vars[vv])))
    }
    rm(extractEnv, envir=.GlobalEnv)
  }
  if (params.nn == "fromParams") {
    params.nn <- unlist(outList[[names(vars)[vars=="p$backtestParams$paramset_name"]]])
  }
  if (any(duplicated(params.nn))) stop("Names for param sets are not unique")
  outList <- lapply(outList, function(oL) {
    names(oL) <- params.nn
    return(oL)
  })
  return(outList)
}

#' formats given list of objects formatted as matrix with dates as row names (as character)  
formatAsDatedMats <- function(allObj, returnDatedMatricesOnly = TRUE, 
                              startDate = NULL, endDate = NULL) {
  # # get alldates and backtest for each param set, check for equality
  # # todo: wrap in try
  # alldates       <- allObj[["alldates"]]
  # backtestDates  <- allObj[["backtestDates"]]
  for (oo in seq_along(allObj)) {
    obj <- allObj[[oo]]
    # special case: data container: transform to named matrix
    if ("dataContainer" %in% unique(unlist(lapply(obj, class)))) {
      obj <- lapply(obj, dc2mat)
    }
    # numeric vector to matrix
    if (all(sapply(obj, is.vector)) && 
        all(sapply(obj, is.numeric))) {
      obj <- lapply(obj, as.matrix)
    } # if missing row names and nrow matches, set alldates or backtest as rownames
    if (all(sapply(obj, is.matrix))) {   
      if (all(sapply(obj, function(o) is.null(rownames(o))))) { 
        if (identical(sapply(obj, nrow), sapply(allObj[["backtestDates"]], length))) {
          for (i in seq_along(obj)) {
            rownames(obj[[i]]) <- as.character(allObj[["backtestDates"]][[i]])
          } 
        } else if (identical(sapply(obj, nrow), sapply(allObj[["alldates"]], length)))
          for (i in seq_along(obj)) {
            rownames(obj[[i]]) <- as.character(allObj[["alldates"]][[i]])
          } 
      } # end if no rownames
      # Impose provided Start end End dates 
      if (!is.null(startDate)){ 
        if (!is.Date(startDate)) stop("startDate must be provided in Date format")
        for (i in seq_along(obj)) {
          if (is.matrix(obj[[i]]) && 
              !is.null(rownames(obj[[i]])) &&
              !is(try(as.Date(rownames(obj[[i]])), silent = TRUE), 'try-error')) {
            obj[[i]] <- obj[[i]][as.Date(rownames(obj[[i]]))>=startDate, , drop=FALSE]
          }
        }
      } # end startDate restriction
      if (!is.null(endDate)){ 
        if (!is.Date(endDate)) stop("endDate must be provided in Date format")
        for (i in seq_along(obj)) {
          if (is.matrix(obj[[i]]) && 
              !is.null(rownames(obj[[i]])) &&
              !is(try(as.Date(rownames(obj[[i]])), silent = TRUE), 'try-error')) {
            obj[[i]] <- obj[[i]][as.Date(rownames(obj[[i]]))<=endDate, , drop=FALSE]
          }
        }
      } # end endDate restriction
      
      # For single col matrices use list name as column name
      if (all(sapply(obj, ncol)==1)) {
        for (o in seq_along(obj)) {
          colnames(obj[[o]]) <- names(obj)[o]
        }
      }
    }  # end if matrix
    allObj[[oo]] <- obj
  } # end loop over allObj
  if (returnDatedMatricesOnly == TRUE) {
    # keep only one col matrices with rowname convertible to date
    keepMatIndex <- rep(TRUE, times = length(allObj))
    for (oo in seq_along(allObj)) {
      for (mm in seq_along(allObj[[oo]])) {
        obj <- allObj[[oo]][[mm]]
        if (!is.matrix(obj)        ||
            ncol(obj)!=1L          ||
            is.null(rownames(obj)) ||
            is(try(as.Date(rownames(obj)), silent = TRUE), 'try-error')
        ) keepMatIndex[oo] <- FALSE
      } # end loop over objects in allObj
    } # end loop over allObj
    allObj <- allObj[keepMatIndex]
    # # Alternative to double loops, but harder to debug
    # allObj <- allObj[sapply(allObj, function(l) {
    #       (all(sapply(l, is.matrix)) &&
    #       (all(sapply(l, ncol)==1))) &&
    #       (all(unlist(sapply(l, function(m){
    #         !is.null(rownames(m))
    #       }))))                      &&
    #       (all(unlist(sapply(l, function(m) {
    #         res = try(as.Date(rownames(m)), silent = TRUE)
    #         if(is(res, 'try-error')) {
    #           return(FALSE)
    #         }
    #         !is.na(res)
    #       })))) # The last two conditions could be tested like this:
    #             # But the optional argument to as.Date is only implemented in R 3.5 !
    #       # (all(unlist(sapply(l, function(m) {
    #       #   !is.na(base::as.Date(base::rownames(m, do.NULL=FALSE), optional = TRUE))
    #       #   }))))
    # })]
  }
  return(allObj)
}

keepCommonDates <- function(mats, keepFirstNA = TRUE, startDate = NULL, endDate = NULL, 
                            stopIfHolesInCorePeriod = TRUE) {
  # ToDo: check that list is named, all matrices are named with standard dates
  # ToDo: introduce option with custom start and end date
  # Make sure that ordering fits in the end
  if (is.matrix(mats)) mats <- list(mats) 
  namats <- lapply(mats, is.na)
  if (keepFirstNA == TRUE) {
    for (mm in seq_along(namats)) {
      for (cc in seq_along(ncol(namats[[mm]]))) {
        namats[[mm]][min(which(namats[[mm]][ ,cc]==FALSE)[1] -1, 1), cc] <- FALSE
      }
    }
  }
  cmpldatelist <- lapply(namats, function(mm) {
    rownames(mm)[complete.cases(mm)]
  })
  maxFirst <- max(as.Date(sapply(cmpldatelist, head, n = 1L)))
  minLast  <- min(as.Date(sapply(cmpldatelist, tail, n = 1L)))
  if (maxFirst > minLast) stop("No common date overlap")
  cmpldates  <- Reduce(intersect, cmpldatelist)
  # truncate to start and end dates and sort
  cmpldatesD <- as.Date(cmpldates)
  names(cmpldatesD) <- cmpldates
  if (!is.null(startDate)) {
    cmpldatesD <- cmpldatesD[cmpldatesD >= startDate]
  }
  if (!is.null(endDate)) {
    cmpldatesD <- cmpldatesD[cmpldatesD <= endDate]
  }
  cmpldatesD <- sort(cmpldatesD)
  cmpldates  <- names(cmpldatesD)
  # check for holes
  if (stopIfHolesInCorePeriod == TRUE) {
    coreDates <- lapply(cmpldatelist, function(dl) {
      d <- as.Date(dl)
      ds <- d[d>=maxFirst & d<= minLast]
      if (!is.null(startDate)) ds <- ds[ds >= startDate]
      if (!is.null(endDate)  ) ds <- ds[ds <= endDate]
      return(as.character(ds))
    })
    if (length(cmpldates)!=length(Reduce(union, coreDates))) {
      stop('The core period of the date ranges has at least one "hole"')
    }
  }
  complmats <- lapply(mats, function(mm) {
    mmc <- mm[cmpldates, , drop=FALSE]
    return(mmc)
  })
  return(complmats)
}

#' returns list of matrices in which one vector matrices are bound together for commond Dates
keepCommonDatesList <- function(allObj, cbind2Mat = FALSE, startDate = NULL, endDate = NULL) {
  # sub list of one vector matrices
  matRes <- allObj[sapply(allObj, function(l) {
    (all(sapply(l, is.matrix)) &&
       (all(sapply(l, ncol)==1)))
  })]
  # keep dates common in the specific sub list
  matRes <- lapply(matRes, keepCommonDates, startDate = startDate, endDate = endDate)
  # ToDo: discuss whether it is sensible to have colnames with content, eg. bm.ret-1_re...
  # Without unique column names, some PerformanceAnalytics functions have problems
  if (cbind2Mat == TRUE) {
    matRes <- lapply(matRes, function(mR) {
      do.call(cbind, mR)
    })
  }
  return(matRes)
}

#' returns list of matrices in which one vector matrices are bound together for all dates
merge2UnionMat <- function(allObj) {
  # sub list of one vector matrices
  matRes <- allObj[sapply(allObj, function(l) {
    (all(sapply(l, is.matrix)) &&
       (all(sapply(l, ncol)==1)))
  })]
  matRes <- lapply(matRes, function(mR) {
    if (length(mR)==1) {
      return(mR[[1]]) # unlist does not work
    } else {
      df <- Reduce(merge, lapply(mR, function(x) data.frame(x, rn = row.names(x))))
      mat <- as.matrix(df[ ,-1, drop = FALSE])
      rownames(mat) <- as.character(df[ ,1, drop = TRUE])
      return(mat)
    }
  } )
  return(matRes)
}
#' Collect backtest results and calculate statitics
#' 
#' @param files A character vector of BacktestData.Rdata result paths and files
#' @param addtlDefVars A character vector of variables IN ADDITION TO STANDARD variables to be collected (not necessarily exported) from BacktestData
#' @param addtlWindows A character vector of windows to be collected and calculated IN ADDITON TO \code{"all"}.
#'    Currently available options are \code{"3M", "1Y", "3Y", "5Y", "exp", "YTD", "yearly"}.
#' @param commonDatesWithoutHolesOnly A Boolean. If \code{TRUE}, the backtest will be truncated to the 
#'    longest joint time interval and an error will be thrown if there is at least one missing date for at least 
#'    one of the parameter sets during this interval.
#' @param startDate All backtest dates before this date will be ignored.
#' @param endDate All backtest dates after this date will be ignored.
#' @param addtlListExport For series that are not one-column time series per parameter set. 
#'    Must be also specified in \code{addtlDefVars}
#' @param replaceBmBy Replacement of the benchmark contained in the parameter set(s). It can be 
#'    (1) a named numeric vector of rebalancing weights for new benchmark using the Datastream 
#'        Mnemomics as names. Currently this only queries total returns in Euro using \code{RI~E0}
#'    (2) [not implemented] a one column matrix of new benchmark returns for all relevant backtest dates 
#'        provided as row names. An attempt will be made to recycle this matrix for all parameter sets. 
#'        Otherwise a list of suitable matrices has to be provided.
#'    (3) [not implemented]. Some form of specifying the total return INDICES for all benchmark components 
#'        and the corresponding weights (column names?)
prepareResults <- function(files, 
                           # 
                           addtlDefVars = NULL, 
                           addtlWindows = NULL,
                           commonDatesWithoutHolesOnly = TRUE, 
                           startDate = NULL,
                           endDate   = NULL,
                           addtlListExport = NULL, # e.g. c("weights") 
                           replaceBmBy = NULL # Replacement BM to be specified in 
                           ) 
  {
  envTmp     <- new.env() # for temporary files
  envRes     <- new.env() # results will be store here
  defVarsStandard <- c("pf.ret"          = "envBacktest$pRetSimpleGrossBefTC",
                       "pf.ret.tc"       = "envBacktest$pRetSimpleGrossAftTC",
                       "bm.ret"          = "envBacktest$bm",
                       "bm.ret.tc"       = "envBacktest$bmAftTC",
                       "rf.ret"          = "envBacktest$rf",
                       "pf.weights"      = "envBacktest$wghtsAft",
                       "pf.turnover"     = "envBacktest$turnover",
                       "pf.tickets"      = "envBacktest$ticketFeeA",
                       # "as.ret"          = "envBacktest$dc_Return", # is not needed in standard comparison, but nice for individual calls
                       "backtestDates"   = "envBacktest$backtestDates",
                       "BTalldates"      = "envBacktest$alldates",
                       "MisinIndex"      = "envBacktest$MisinIndex",
                       "allnames"        = "envBacktest$allnames",
                       "params.nn"       = "p$backtestParams$paramset_name",
                       "active"          = "p$active",
                       "minWghtTrigger"  = "p$backtestParams$minWghtTrigger",
                       "BTstartDate"     = "p$backtestParams$startDate",
                       "BTendDate"       = "p$backtestParams$endDate")
                       
  defVars <- c(defVarsStandard, addtlDefVars)
  
  if (!is.null(addtlWindows)) {
    if (!all(addtlWindows %in% c("3M", "1Y", "3Y", "5Y", "exp", "yearly", "YTD"))) {
      stop('Only "3M", "1Y", "3Y", "5Y", "exp", "yearly", and "YTD" are permissible addtlWindows')
    }
  }
  envTmp$transDF <- createTransDF(windowNames = c("all", addtlWindows))
  # if (is.character(transDF)) {
  #   envTmp$transDF <- createTransDF(windowNames = transDF)
  # } else {
  #   envTmp$transDF <- transDF
  # }

  # #### Copy GlobalParams etc from global environment availaible when run in backtestTool
  # if (exists("envConfig"))    assign("envConfig"    , envConfig   , envir=envRes)
  # if (exists("globalParams")) assign("globalParams" , globalParams, envir=envRes)
  # if (exists("ParamSet"))     assign("ParamSet"     , ParamSet    , envir=envRes)	
  
  writeLines("... extracting results from files")
  
  extRes      <- extractResults(files = files, vars = defVars)
  # apply start and end date restrictions
  # ToDo: find more elegant way to avoid repeating "formatAsDatedMats" part below and maybe prevent 
  # change to matrix (from vector) for ´pf.turnover, pf.tickets, minWghtTrigger
  extRes      <- formatAsDatedMats(extRes, 
                           returnDatedMatricesOnly = FALSE,
                           startDate = startDate, 
                           endDate = endDate)
  extRes$pf.activepositions <- 
    mapply(function(x, y) rowSums(abs(x)>=y), 
                      extRes[["pf.weights"]],                        # x var
                      lapply(extRes[["minWghtTrigger"]], as.vector), # y var
                      SIMPLIFY=FALSE)
  extRes$bm.activepositions <- 
    lapply(extRes[["MisinIndex"]], rowSums, na.rm=TRUE)
  
  for (i in seq_len(length(addtlListExport))) {
    if (!exists(addtlListExport[1], where=extRes)) {
      stop("addtlListExport parameter contains object that is not in defVars parameter")  
    }
    assign(addtlListExport[i], extRes[[addtlListExport[i]]], envir = envRes)
  }

  ### Replace Benchmark #### 
  # replaceBmBy = c("MSWRLD$" = 0.4, "JPMGBU$" = 0.6)
  if (is.vector(replaceBmBy, mode = "numeric") && !is.null(names(replaceBmBy))) {
    TriRdsFile <- file.path(Sys.getenv("TEMP"), paste0(paste(names(replaceBmBy), collapse="-"),
                                                       "_", gsub(" ", "", as.character(zoo::as.yearmon(Sys.Date()))),
                                                       ".rds"), fsep = "\\")
    for (ps in seq_along(extRes[["BTalldates"]])) {
      extRes[["bm.ret"]][[ps]] <- replaceBenchmarkByDSquery(
        replaceBmBy = replaceBmBy,
        alldates    = extRes[["BTalldates"]][[ps]], 
        bmDates     = rownames(extRes[["bm.ret"]][[ps]]),
        TriRdsFile  = TriRdsFile,
        colname     = colnames(extRes[["bm.ret"]][[ps]]))
    }  
    extRes[["bm.ret.tc"]] <- extRes[["bm.ret"]]
  } 
  
  
  forRes           <- formatAsDatedMats(extRes, 
                                        returnDatedMatricesOnly = TRUE,
                                        startDate = startDate, 
                                        endDate = endDate)
  

  envRes$params.nn <- unlist(extRes[["params.nn"]])
  envRes$allnames  <- extRes[["allnames"]]
  
  #AW for continuity reasons, replicate prior calculations of alldates
  alldates  <- NULL
  for (ll in seq_along(extRes$params.nn)) {
    dd <- extRes$backtestDates[[ll]]
    alldates <- unique(as.numeric(c(alldates,
                       dd[dd >= extRes$BTstartDate[[ll]] & dd <= extRes$BTendDate[[ll]]]
                       ))) + as.Date("1970-01-01")
  }
  envRes$alldates <- alldates
  #### UNIVARIATE time series per param set merged into named matrix, put in result environment
  if (commonDatesWithoutHolesOnly) {
    list2env(keepCommonDatesList(forRes, cbind2Mat = TRUE, startDate = startDate, endDate = endDate),
             envir = envRes)
    # list2env(keepCommonDatesList(forRes, cbind2Mat = FALSE, startDate = startDate, endDate = endDate),
    #          envir = envRes)
  } else {
    list2env(forRes, envir = envRes)
  }
  

  # ### Replace Benchmark #### 
  # # replaceBmBy = c("MSWRLD$" = 0.4, "JPMGBU$" = 0.6)
  # if (is.vector(replaceBmBy, mode = "numeric") && !is.null(names(replaceBmBy))) {
  #   TriRdsFile <- file.path(Sys.getenv("TEMP"), paste0(paste(names(replaceBmBy), collapse="-"),
  #                                              "_", gsub(" ", "", as.character(zoo::as.yearmon(Sys.Date()))),
  #                                              ".rds"), fsep = "\\")
  #   if (commonDatesWithoutHolesOnly) {
  #     envRes[["bm.ret"]] <- replaceBenchmarkByDSquery(
  #       replaceBmBy = replaceBmBy,
  #       alldates    = extRes[["BTalldates"]], 
  #       bmDates     = rownames(envRes[["bm.ret"]]),
  #       TriRdsFile  = TriRdsFile,
  #       colname     = colnames(envRes[["bm.ret"]]))
  #   } else {
  #     for (ps in seq_along(extRes[["BTalldates"]])) {
  #       envRes[["bm.ret"]][[ps]] <- replaceBenchmarkByDSquery(
  #         replaceBmBy = replaceBmBy,
  #         alldates    = extRes[["BTalldates"]][[ps]], 
  #         bmDates     = rownames(envRes[["bm.ret"]][[ps]]),
  #         TriRdsFile  = TriRdsFile,
  #         colname     = colnames(envRes[["bm.ret"]][[ps]]))
  #     }  
  #   }
  #   envRes[["bm.ret.tc"]] <- envRes[["bm.ret"]]
  # } 

  # calculate derived statistics from matrices, instructions encoded in transDF
  with(envRes, {
    suppressWarnings(try(rm(.printdLast), silent=TRUE)) # removes possible .printLast from previous printd calls
    for (ds_i in seq_len(nrow(envTmp$transDF))) {
      printd(paste0("... calculating derived statistic ", ds_i, " out of ", nrow(envTmp$transDF), "\n"))  
      assign(envTmp$transDF$newName[[ds_i]], 
             do.call(routeToWindowAndOtherFunctions, list(
               f  = envTmp$transDF$fcts     [[ds_i]],
               A  = envTmp$transDF$mainSrs  [[ds_i]],
               B  = envTmp$transDF$auxSrs   [[ds_i]],
               w  = envTmp$transDF$windowPar[[ds_i]],
               fp = envTmp$transDF$funPar   [[ds_i]],
               envObj = envRes
              ))
             )
    }
    printd("\n")
    suppressWarnings(try(rm(.printdLast), silent=TRUE)) # removes .printLast from global environment
    rm(ds_i)
    
    writeLines("... generating betas and overview tables")
    
    ##AW Todo, discuss, whether and which betas and alphas should be systematically calculated (e.g., in transDF)
    if (commonDatesWithoutHolesOnly) {
      pf.beta.1y        <- calculate.beta(pf.ret       , bm.ret        , alldates = as.Date(rownames(pf.ret))        , window = windowByName(rownames(pf.ret)       , "1Y"    ), regressorNumber = 2, returnPValue = FALSE)
      # pf.beta.yearly    <- calculate.beta(pf.ret       , bm.ret        , alldates = as.Date(rownames(pf.ret))        , window = windowByName(rownames(pf.ret)       , "yearly"), regressorNumber = 2, returnPValue = FALSE) # does not seem to work for monthly data
      pf.beta.YTD       <- calculate.beta(pf.ret       , bm.ret        , alldates = as.Date(rownames(pf.ret))        , window = windowByName(rownames(pf.ret)       , "YTD"   ), regressorNumber = 2, returnPValue = FALSE)
      pf.beta.all       <- calculate.beta(pf.ret       , bm.ret        , alldates = as.Date(rownames(pf.ret))        , window = windowByName(rownames(pf.ret)       , "all"   ), regressorNumber = 2, returnPValue = FALSE)
      pf.alpha.all      <- calculate.beta(pf.exretrf   , bm.exretrf    , alldates = as.Date(rownames(pf.exretrf))    , window = windowByName(rownames(pf.exretrf)   , "all"   ), regressorNumber = 1, returnPValue = FALSE)
      pf.alpha.all      <- pf.alpha.all * scaleByName(as.Date(rownames(pf.exretrf)))
      pf.alphaP.all     <- calculate.beta(pf.exretrf   , bm.exretrf    , alldates = as.Date(rownames(pf.exretrf))    , window = windowByName(rownames(pf.exretrf)   , "all"   ), regressorNumber = 1, returnPValue = TRUE )
      pf.beta.tc.all    <- calculate.beta(pf.ret.tc    , bm.ret.tc     , alldates = as.Date(rownames(pf.ret.tc))     , window = windowByName(rownames(pf.ret.tc)    , "all"   ), regressorNumber = 2, returnPValue = FALSE)
      pf.alpha.tc.all   <- calculate.beta(pf.exretrf.tc, bm.exretrf.tc , alldates = as.Date(rownames(pf.exretrf.tc)) , window = windowByName(rownames(pf.exretrf.tc), "all"   ), regressorNumber = 1, returnPValue = FALSE)
      pf.alpha.tc.all   <- pf.alpha.tc.all * scaleByName(as.Date(rownames(pf.exretrf.tc)))
      pf.alphaP.tc.all  <- calculate.beta(pf.exretrf.tc, bm.exretrf.tc , alldates = as.Date(rownames(pf.exretrf.tc)) , window = windowByName(rownames(pf.exretrf.tc), "all"   ), regressorNumber = 1, returnPValue = TRUE )
    } else {
      pf.beta.1y        <- mapply(calculate.beta,pf.ret        , bm.ret       ,  lapply(pf.ret       , function(l) as.Date(rownames(l))),  lapply(pf.ret       , function(l) windowByName(rownames(l), "1Y"  )), regressorNumber = 2, returnPValue = FALSE, SIMPLIFY = FALSE)
      pf.beta.YTD       <- mapply(calculate.beta,pf.ret        , bm.ret       ,  lapply(pf.ret       , function(l) as.Date(rownames(l))),  lapply(pf.ret       , function(l) windowByName(rownames(l), "YTD" )), regressorNumber = 2, returnPValue = FALSE, SIMPLIFY = FALSE)
      pf.beta.all       <- mapply(calculate.beta,pf.ret        , bm.ret       ,  lapply(pf.ret       , function(l) as.Date(rownames(l))),  lapply(pf.ret       , function(l) windowByName(rownames(l), "all" )), regressorNumber = 2, returnPValue = FALSE, SIMPLIFY = FALSE)
      pf.alpha.all      <- mapply(calculate.beta,pf.exretrf    , bm.exretrf   ,  lapply(pf.exretrf   , function(l) as.Date(rownames(l))),  lapply(pf.exretrf   , function(l) windowByName(rownames(l), "all" )), regressorNumber = 1, returnPValue = FALSE, SIMPLIFY = FALSE)
      pf.alphaP.all     <- mapply(calculate.beta,pf.exretrf    , bm.exretrf   ,  lapply(pf.exretrf   , function(l) as.Date(rownames(l))),  lapply(pf.exretrf   , function(l) windowByName(rownames(l), "all" )), regressorNumber = 1, returnPValue = TRUE , SIMPLIFY = FALSE)
      pf.beta.tc.all    <- mapply(calculate.beta,pf.ret.tc     , bm.ret.tc    ,  lapply(pf.ret       , function(l) as.Date(rownames(l))),  lapply(pf.ret.tc    , function(l) windowByName(rownames(l), "all" )), regressorNumber = 2, returnPValue = FALSE, SIMPLIFY = FALSE)
      pf.alpha.tc.all   <- mapply(calculate.beta,pf.exretrf.tc , bm.exretrf.tc,  lapply(pf.exretrf.tc, function(l) as.Date(rownames(l))),  lapply(pf.exretrf.tc, function(l) windowByName(rownames(l), "all" )), regressorNumber = 1, returnPValue = FALSE, SIMPLIFY = FALSE)
      pf.alphaP.tc.all  <- mapply(calculate.beta,pf.exretrf.tc , bm.exretrf.tc,  lapply(pf.exretrf.tc, function(l) as.Date(rownames(l))),  lapply(pf.exretrf.tc, function(l) windowByName(rownames(l), "all" )), regressorNumber = 1, returnPValue = TRUE , SIMPLIFY = FALSE)
      
      pf.alpha.all      <- mapply("*", pf.alpha.all    , lapply(pf.exretrf   , function(l) scaleByName(rownames(l))), SIMPLIFY = FALSE)
      pf.alpha.tc.all   <- mapply("*", pf.alpha.tc.all , lapply(pf.exretrf.tc, function(l) scaleByName(rownames(l))), SIMPLIFY = FALSE)

      # Try to merge all 
      for (oN in ls()) {
        if (is.list(get(oN))) {    # is list
          if (all(sapply(get(oN), is.matrix)) && # contains one-column matrices
              (all(sapply(get(oN), ncol)==1)))  {
            assign(oN, merge2UnionMat(list(get(oN)))[[1]])
          }
        }
      }
      rm('oN')
    }
    
    backtest.overview<-
      rbind.data.frame(
        pf.ret      = pf.ret.all,
        bm.ret      = bm.ret.all,
        rf.ret      = rf.ret.all,
        pf.vola     = pf.vola.all,
        bm.vola     = bm.vola.all,
        pf.te       = pf.exvolabm.all,
        pf.mdd      = pf.mdd.all,
        bm.mdd      = bm.mdd.all,
        pf.turnover = pf.turnover.all,
        pf.tickets  = pf.tickets.all,
        pf.tc       = pf.tc.all,
        pf.exretbm  = pf.exretbm.all,
        pf.exretrf  = pf.exretrf.all,
        bm.exretrf  = bm.exretrf.all,
        pf.sharpe   = pf.sharpe.all,
        bm.sharpe   = bm.sharpe.all,
        pf.ir       = pf.ir.all,
        pf.beta     = pf.beta.all,
        pf.alpha    = pf.alpha.all,
        pf.alphaP   = pf.alphaP.all
      )
    format(backtest.overview, digits = 4, scientific = 4)
    
    backtest.overview.tc <-
      rbind.data.frame(
        pf.ret      = pf.ret.tc.all,
        bm.ret      = bm.ret.tc.all,
        rf.ret      = rf.ret.all,
        pf.vola     = pf.vola.tc.all,
        bm.vola     = bm.vola.tc.all,
        pf.te       = pf.exvolabm.tc.all,
        pf.mdd      = pf.mdd.tc.all,
        bm.mdd      = bm.mdd.tc.all,
        pf.turnover = pf.turnover.all,
        pf.tickets  = pf.tickets.all,
        pf.tc       = pf.tc.all,
        pf.exretbm  = pf.exretbm.tc.all,
        pf.exretrf  = pf.exretrf.tc.all,
        bm.exretrf  = bm.exretrf.tc.all,
        pf.sharpe   = pf.sharpe.tc.all,
        bm.sharpe   = bm.sharpe.tc.all,
        pf.ir       = pf.ir.tc.all,
        pf.beta     = pf.beta.tc.all,
        pf.alpha    = pf.alpha.tc.all,
        pf.alphaP   = pf.alphaP.tc.all
      )
    format(backtest.overview.tc, digits = 4, scientific = 4)
    
    # If all portfolios have same bm and rf (or are reduced to the intersection), 
    # generate tables with bm and pf as column
# all(bm.ret[ ,1]==bm.ret) && all(rf.ret[ ,1]==rf.ret )

  if (all(duplicated(t(round(bm.ret, 10)))[-1]) && all(duplicated(t(rf.ret))[-1])) {
      backtest.wide <- 
        rbind.data.frame(
          ret         = c(pf.ret.all     , firstColIfCommon(bm.ret.all    ), firstColIfCommon(rf.ret.all)),
          vola        = c(pf.vola.all    , firstColIfCommon(bm.vola.all   ), firstColIfCommon(rf.vola.all)),
          mdd         = c(pf.mdd.all     , firstColIfCommon(bm.mdd.all    ), firstColIfCommon(rf.mdd.all)),
          exretrf     = c(pf.exretrf.all , firstColIfCommon(bm.exretrf.all), NA),
          exretbm     = c(pf.exretbm.all , NA                              , NA),    
          sharpe      = c(pf.sharpe.all  , firstColIfCommon(bm.sharpe.all ), NA),
          ir          = c(pf.ir.all      , NA                                    , NA),
          turnover    = c(pf.turnover.all, NA                                    , NA),    
          tickets     = c(pf.tickets.all , NA                                    , NA),    
          tc          = c(pf.tc.all      , NA                                    , NA),
          beta        = c(pf.beta.all    , NA                                    , NA),
          alpha       = c(pf.alpha.all   , NA                                    , NA),
          alphaTstat  = c(pf.alphaP.all  , NA                                    , NA)
        )
      colnames(backtest.wide) <- c(colnames(pf.ret.all), "bm", "rf")
      rownames(backtest.wide) <- c("ret", "vola", "mdd", "exretrf", "exretbm", "sharpe", "ir", "turnover", "tickets", "tc", "beta", "alpha", "alphaP")
      format(backtest.wide, digits = 4, scientific = 4)
  
      backtest.wide.tc <- 
        rbind.data.frame(
          ret         = c(pf.ret.tc.all     , firstColIfCommon(bm.ret.tc.all    ), firstColIfCommon(rf.ret.all)),
          vola        = c(pf.vola.tc.all    , firstColIfCommon(bm.vola.tc.all   ), firstColIfCommon(rf.vola.all)),
          mdd         = c(pf.mdd.tc.all     , firstColIfCommon(bm.mdd.tc.all    ), firstColIfCommon(rf.mdd.all)),
          exretrf     = c(pf.exretrf.tc.all , firstColIfCommon(bm.exretrf.tc.all), NA),
          exretbm     = c(pf.exretbm.tc.all , NA                                 , NA),    
          sharpe      = c(pf.sharpe.tc.all  , firstColIfCommon(bm.sharpe.tc.all ), NA),
          ir          = c(pf.ir.tc.all      , NA                                 , NA),
          turnover    = c(pf.turnover.all   , NA                                 , NA),    
          tickets     = c(pf.tickets.all    , NA                                 , NA),    
          tc          = c(pf.tc.all         , NA                                 , NA),
          beta        = c(pf.beta.tc.all    , NA                                 , NA),
          alpha       = c(pf.alpha.tc.all   , NA                                 , NA),
          alphaTstat  = c(pf.alphaP.tc.all  , NA                                 , NA)
        )
      colnames(backtest.wide.tc) <- c(colnames(pf.ret.tc.all), "bm", "rf")
      rownames(backtest.wide.tc) <- c("ret", "vola", "mdd", "exretrf", "exretbm", "sharpe", "ir", "turnover", "tickets", "tc", "beta", "alpha", "alphaP")
      format(backtest.wide.tc, digits = 4, scientific = 4)
    } else {
      backtest.wide    <- NULL
      backtest.wide.tc <- NULL
    }
  }) # end with envRes

#clean return environment
envRet = new.env(parent = .GlobalEnv)
for(n in ls(envRes, all.names=TRUE)) {
  assign(n, get(n, envRes), envir = envRet)
}

return(envRet)
}

### Miscellaneous e.g., exports from Rclasses   _________________________________________________________________ ####

    envirInit = function(cls)
{
  ##* DESCRIPTION
  ##* Creates a new environment of the name \code{cls} if it does not exist and attaches it to 
  ##* the global environment. Furthermore the name of the environment is added to the .envirNames if necessary.
  ##*
  ##* ARGUMENTS
  ##* cls: name of the new environment.
  ##*
  
  if(substr(cls,1,1)!=".") {
    cls = paste(".",cls, sep = "")
  }
  
  if(!exists(".envirNames", envir = .GlobalEnv)) {
    assign(".envirNames", NULL, envir = .GlobalEnv)  
  }
  
  envirNames = get(".envirNames", envir = .GlobalEnv)
  if(! cls %in% envirNames) {
    envirNames = c(envirNames, cls)  
    assign(".envirNames", envirNames, envir = .GlobalEnv)
  }
  if(!exists(cls, envir = .GlobalEnv)) {
    
    assign(cls, new.env(parent = .GlobalEnv), envir = .GlobalEnv)
  } else {
    if(class(get(cls, envir = .GlobalEnv, inherits = FALSE))!="environment") {
      stop(paste("A ", cls ,"object exists in the .GlobalEnv, but its not an environment.",sep=""))  
    }
  }                       
}

envirInit("dbFsCon")
envirInit("dataContainer")

isNA = function(x)
{
  ##* returns TRUE if x is NA; doesn't work elementwise (i.e. isNA(c(NA,NA)) is
  ##* FALSE; isNA(list()) is FALSE; isNA(list(NA)) is FALSE )
  if(is.list(x)) {
    return(FALSE)
  }
  if(is.character(x) && identical(x, as.character(NA))) {
    return(TRUE)  
  }
  if(is.logical(x) && identical(x, as.logical(NA))) {
    return(TRUE)  
  }
  if(is.numeric(x) && identical(x, as.numeric(NA))) {
    return(TRUE)  
  }  
  
  return(FALSE)
}

dcInit = function( id  = character(), date = NA, data = NA, description = character(), dbCon = NA, hasTsData = FALSE)
{
  ##* initializes and returns a dataContainer object
  
  ##* id: unique id of the dataContainer
  ##* date: list with dates corresponding to the data in the matrix data
  ##* data: matrix with data (number of rows must correspond the length of date)
  ##* description: a decription of the data container; character() (of lenght 1)
  ##* dbCon: a database connection object 
  
  if(!is.character(id) || length(id)>1){
    stop("dcInit: Invalid ID.")
  }
  if(!is.character(description) || length(description)>1){
    stop("dcInit: Invalid description.")
  }
  if(!isNA(data) && class(data)!="matrix" && class(data)!="data.frame") {
    stop("dcInit: Invalid data object.")
  }
  if(!isNA(date) && class(try(as.Date(date),TRUE))[[1]]=="try-error") {
    stop("dcInit: Invalid date list.")
  }
  
  
  if (!isNA(data) || !isNA(date)) {
    
    data = as.matrix(data)
    
    if (length(date) != dim(data)[1] ) {
      stop("dcInit: Inconsistent dimension of date and data.")
    }
    if(mode(data)!="numeric") {
      #stop("dcInit: Non-numeric data.")
    }
  }
  
  
  
  dc=list(id=id, date=date, data=data, description=description, dbCon = dbCon, hasTsData = hasTsData)
  class(dc) = c("dataContainer","list")
  
  if(!isNA(dbCon)) {
    dc = setDbCon(dc, dbCon)
  }
  
  return(dc)
}

dbConRead = function(x,...)
{
  UseMethod("dbConRead")
}

dbConWrite = function (x,...) {
    UseMethod("dbConWrite")
}

searchPrefix = function(x,...)
{
  UseMethod("searchPrefix")
}

searchPrefix.default = function(obj)
{
  ##* returns the 2 character prefix string associated with given object; issues
  ##* an error if not found
  
  prefix = switch(class(obj)[1],  
                  equity = "eq",
                  interestRate = "ir",
                  benchmark = "bm",
                  bond = "bn",
                  timeSeries = "ts",
                  timeSeriesWithRev = "ts",
                  option = "op",
                  yieldCurve = "yc",
                  portfolio = "pf",
                  objectContainer = "oc",
                  dataContainer  = "dc",
                  economy = "ec",
                  option = "op",
                  exchangeRate = "er",
                  cash = "cs",
                  volaSurface = "vs",
                  forward = "fw",
                  indexFuture = "id",
                  interestFuture = "it",
                  deposit = "dp",
                  scoring = "sc",
                  characteristic = "ch",
                  relation = "re",                  
                  swap = "sw",
                  fxSwap = "fs",
                  timeSeriesControl = "tc",
                  fra = "fr",
                  transitionMatrix = "tm",
                  yieldCurveGroup = "yg",
                  region = "rg",
                  index = "ix",
                  certificate = 'cf',
                  subject="su",
                  task="ta",
                  scheduler="sh",
                  taskToDo="tt",
                  deposit="dp",
                  client="cl",
                  generic = "gn",
                  market = "ma",
                  stop("searchPrefix.default: Object is not one of portfolio classes objects.")
  ) 
  
  return(prefix)
}


getObject = function(x, ...)
{
  UseMethod("getObject")
}

getObject.default = function(o, envir = NULL)
{
  
  if(is.null(o$id)) {
    stop("getObject.default: Given object has no id attribute.")
  }
  if(is.null(envir)) {
    if(!exists(paste(".",class(o)[1],sep=""), envir = .GlobalEnv, inherits = FALSE)){
      stop(paste("getObject.default: Variable ",paste(".",class(o)[1],sep="")," not found in the .GlobalEnv. Class initialized?", sep =""))
    }
    envir = get(paste(".",class(o)[1],sep=""), envir = .GlobalEnv, inherits = FALSE)
  }
  
  if(!exists(o$id, where = envir, inherits = FALSE)) {
    return(o)
    #stop(paste("getObject.default: Object ",o$id," not found in the ",envir," environment.",sep=""))
  }
  
  return(get(o$id, envir = envir, inherits = FALSE))
  
}

prepareSave = function(x,...) {
##* generic function for preparing objects for saving to a db
  UseMethod("prepareSave")
}

prepareSave.default = function(x) {
  return(x)
}


dbConObjExists <- function(x,...)
{
  UseMethod("dbConObjExists")
}

dbConObjExists.dbFsCon <- function(conn, obj)
{
  ##* DESCRIPTION
  ##* Checks if in the given file system connection the given object exist (i.e. an object of the same type and with the same id).
  
  conn   <- getObject(conn)
  prefix <- searchPrefix(obj)
  
  # check object type
  if (is.null(prefix)) {
    stop("dbConIdExists.dbFsCon: Object is of unknown type.")
  }
  
  if (paste(prefix, getID(obj), ".Rraw", sep = "") %in% dir(path = conn$path) ) {
    return(TRUE)
  }
  
  return(FALSE) 
}

dbConRead.dbFsCon = function(conn, obj, ...)
{
  ##* in the given FileSystemConnection searches for and reads an object
  ##* corresponding to the class and id of the given object
  
  conn        <- getObject(conn)
  name_prefix <- searchPrefix(obj)
  
  if(is.null(name_prefix)) {
    stop("dbConRead.dbFsCon: The given object is of unknown type.")
  }
  
  if (!dbConObjExists(conn, obj)) {
    stop(paste("dbConRead.dbFsCon:",paste(name_prefix,getID(obj),".Rraw",sep=""),"cannot be found in directory",conn$path))
  }
  
  id = try(load(paste(conn$path,paste(name_prefix,getID(obj),".Rraw",sep=""),sep="/")),TRUE) 
  if (class(id)=="try-error") {
    stop(paste("dbConRead.dbFsCon: There was a problem loading the file:",paste(name_prefix,getID(obj),".Rraw",sep="")))
  }
  
  if(length(id)!=1 || id!=paste(name_prefix,getID(obj),sep="")){ 
    stop(paste("dbConRead.dbFsCon: File",paste(name_prefix,getID(obj),".Rraw",sep=""), "contains variable(s) named:",paste(id)))
  }
  
  read_obj = get(id)  
  
  if(sum(class(obj)!=class(read_obj))>0) {     # if the loaded object has different class than the given
    stop(paste("dbConReadEq.dbFsCon: The object saved in the file ",paste(name_prefix,getID(obj),".Rraw",sep="")," has a different class than the given object.",sep=""))
  }
  
  if(getID(read_obj)!=getID(obj)) { # if the id of the given object is not equal to the read one
    stop(paste("dbConReadEq.dbFsCon: The object saved in the file ",paste(name_prefix,getID(obj),".Rraw",sep="")," has a different ID than the given object.",sep=""))
  }
  
  read_obj = prepareLoad(read_obj)
  
  # set the connection of the read object to the conn of obj, if its a valid connection, otherwise to conn
  if(is.dbCon(getDbCon(obj))) {
    read_obj = setDbCon(read_obj, getDbCon(obj), ...)   
  } else {
    read_obj = setDbCon(read_obj, conn, ...)  
  }
  
  read_obj$db_time = Sys.time()
  
  return(read_obj)
}

dbConWrite.dbFsCon = function(conn, obj, filename = character(), objname = character(), recursive = FALSE) {
##* writes the object obj to the given FileSystemConnection
##* filename and objname override the default saving behavior (which is to name
##* the file and the saved with the obj$id with a prefix according to the
##* class of the object)
##* recursive, when TRUE, saves all objects related with the given object (e.g. for a yield curve it saves all interestRate objects and timeSeries objects
##* which are in the curve as well as the curve itself)
##* returns the saved object (as prepared for saving with the generic prepareSave function)
    
    conn <- getObject(conn)
    
    if (! conn$rwFlag %in% c("w","ow")) {
        stop("dbConWrite.dbFsCon: The connection is read only.")
    }
    
    if(recursive) {
        if(!hasTsData(obj)) {
            stop("dbConWrite.dbFsCon: Trying to recursively save an object without data.")
        }
        obj_list = prepareSave(obj, TRUE)
        if(length(obj_list)>0) {
            id_df = data.frame()
            for(i in length(obj_list):1) {
                if(nrow(id_df)>0 && searchPrefix(obj_list[[i]]) %in% id_df$type &&
                        getID(obj_list[[i]]) %in% id_df[id_df$type == searchPrefix(obj_list[[i]]),2]) {
                    
                    obj_list = obj_list[-i]
                }
                id_df = unique(rbind(id_df , data.frame(type = searchPrefix(obj_list[[i]]), id = getID(obj_list[[i]]))))
            }
            for(o in obj_list) {
                dbConWrite(conn, o)
            }
        }
    }
    
    obj = prepareSave(obj)
    
    if(!is.character(filename) || length(filename)>1) {
        stop("dbConWrite.dbFsCon: The filename must be a character string.")
    }  
    if(!is.character(objname) || length(objname)>1) {
        stop("dbConWrite.dbFsCon: Object name must be a character string.")
    }  
    if( (length(filename)==1 && length(objname)==0) || (length(filename)==0 && length(objname)==1) ) {
        stop("dbConWrite.dbFsCon: Filename and object name must be specified both or none.")    
    }
    if(length(filename) == 1 && filename =="" ){
        stop("dbConWrite.dbFsCon: The filename was not specified correctly.")  
    }
    if(length(objname) == 1 && objname =="" ){
        stop("dbConWrite.dbFsCon: Object name was not specified correctly.")  
    }    
    
    
    if(length(filename) == 1) { # if a file name and an object name are given, save using these
        if (class(try(assign(objname,obj))[1])=="try-error") {    # if the given object is not valid
            stop(paste("dbConWrite.dbFsCon: There was a problem creating object named ",objname,".", sep="" ))
        }
        if (paste(conn$path,paste(filename,".Rraw",sep=""),sep="/") %in% dir(path=conn$path) && conn$rwFlag!="ow") {
            stop(paste("dbConWrite.dbFsCon: File", paste(conn$path,paste(filename,".Rraw",sep=""),sep="/") ,"already exists, but the connection is not allowed to overwrite."))
        }
        
        if (class(try(save(list=objname,file=paste(conn$path,paste(filename,".Rraw",sep=""),sep="/"))))=="try-error") {
            stop(paste("dbConWrite.dbFsCon: There was a problem creating file ",paste(filename,".Rraw",sep="")," in directory ", conn$path,".",sep=""))    
        }
        obj$db_time = Sys.time()
        return(obj)
    }
    
    name_prefix = searchPrefix(obj)
    
    if(is.null(name_prefix)) { # if the object is unknown filename and objname must be set
        stop("dbConWrite.dbFsCon: The given object is of unknown type. Specify file and object names to save it.")  
    }
    
    if(dbConObjExists(conn, obj) && conn$rwFlag!="ow") {
        stop(paste("dbConWrite.dbFsCon: ", name_prefix, "-Object with id ", getID(obj) ," already exists in the given connection, but the connection is not allowed to overwrite.", sep=""))
    }
    
    if (class(try(assign(paste(name_prefix,getID(obj),sep=""),obj)))[1]=="try-error") {
        stop(paste("dbConWrite.dbFsCon: There was a problem creating object named ",paste(name_prefix,getID(obj),sep=""),".", sep="" ))
    }
    
    if ( class(try(save(list=paste(name_prefix,getID(obj),sep=""),file=paste(conn$path,paste(name_prefix,getID(obj),".Rraw",sep=""),sep="/")))) == "try-error" ) {
        stop(paste("dbConWrite.dbFsCon: ",geterrmessage()))  
    }
    obj$db_time = Sys.time()
    return(obj)
}


dbFsConInit <- function(path = getwd() , rwFlag = "r", id, overwrite = FALSE)
{
  ##* DESCRIPTION
  ##* creates a FileSystemConnection for reading or writing timeSeries etc.
  ##*
  ##* ARGUMENTS
  ##* path: path to the folder which the connection should use for reading or writing (if no path is given the current working directory is used)
  ##* rwFlag: maximum writing right: r - read only, w - write, ow - overwrite
  
  # set id if missing
  if(missing(id)) id <- paste("FsCon", length(ls(.dbFsCon)) + 1, sep = "")
  
  # initialize the dbFsCon object
  conn                <- list(id = id, path = path, rwFlag = rwFlag)
  
  # set class of the object
  attr(conn, "class") <- c("dbFsCon","list")
  
  # check if an object with the same id already exists
  alreadyExists <- ifelse(id %in% ls(.dbFsCon), TRUE, FALSE)
  
  # if so and overwrite is FALSE, do not overwirte the object in the dbFsCon environment
  if(!alreadyExists || overwrite) assign(conn$id, conn, envir = .dbFsCon)
  
  # set all arguments except id to NA and return conn
  for(i in names(conn)) {
    if(i != "id") {
      conn[[i]] <- NA
    }
  }
  
  return(conn)
}



is.dbFsCon <- function(conn)
{
  ##* DESCRIPTION
  ##* Check if connection is from class dbFsCon (file system connection).
  
  if (is(conn, "dbFsCon")) {
    return(TRUE)
  }
  
  return(FALSE)
}

is.dbCon <- function(conn)
{
  ##* DESCRIPTION
  ##* Check if connection is a known connection object, i.e. has class dbMySqlCon or dbFsCon at the moment.
  
  if (is.dbFsCon(conn)) {
    return(TRUE)
  }  
  return(FALSE)
}

setDbCon = function(x,...)
{
  UseMethod("setDbCon")
}

setDbCon.default = function(x, dbCon, recursive = TRUE, ...)
{
  ##* sets the connection of object x to dbCon; checks whether dbCon is
  ##* a valid dbConnection; also sets the dbCon of all sub objects of x if recursive = TRUE
  
  if(!isPortfolioClassObject(x) && !is.list(x)) {
    stop ("setDbCon.default: Unknown object type.")  
  }
  
  if(!is.dbCon(dbCon)) {
    stop ("setDbCon.default: Unknown connection type.")
  }
  
  if(isPortfolioClassObject(x)) {
    if(objectExists(x)) {
      dummy  = getObject(x)
      dummy$dbCon = dbCon
      putObject(dummy)
    }
    
    if(recursive) {
      for(i in 1:length(x)) {
        if(isPortfolioClassObject(x[[i]])) {
          x[[i]]$dbCon = dbCon
          setDbCon.default(x[[i]], dbCon = dbCon, recursive = TRUE)      
        } else if(is.list(x[[i]])) {
          x[[i]] = setDbCon.default(x[[i]], dbCon = dbCon, recursive = TRUE)                        
        }
      }
    }
    
    x$dbCon = dbCon  
    return(x)
    
  } else if(is.list(x)) {
    if(length(x)==0) {
      return(x)
    }      
    for(i in 1:length(x)) {    
      if(isPortfolioClassObject(x[[i]])) {
        x[[i]]$dbCon = dbCon
        setDbCon.default(x[[i]], dbCon = dbCon, recursive = TRUE)      
      } else if(is.list(x[[i]])) {
        x[[i]] = setDbCon.default(x[[i]], dbCon = dbCon, recursive = TRUE)                        
      }
    }
    return(x)
  }
  
  
}

putObject = function(x, ...)
{
  UseMethod("putObject")
}

putObject.default = function(o, envir = NULL)
{
  if(is.null(o$id) || length(o$id)!=1) {
    stop("putObject.default: Given object has an invalid id attribute.")
  }
  if(is.null(envir)) {
    if(!exists(paste(".",class(o)[1],sep=""), envir = .GlobalEnv, inherits = FALSE)){
      #stop(paste("putObject.default: Variable ",paste(".",class(o)[1],sep="")," not found in the .GlobalEnv. Class initialized?", sep =""))
      envirInit(paste(".",class(o)[1],sep=""))
    }
    envir = get(paste(".",class(o)[1],sep=""), envir = .GlobalEnv, inherits = FALSE)
  }
  
  assign(o$id, o, envir = envir)
  return(init(o))
}

isPortfolioClassObject <- function(obj)
{
  ##* DESCRIPTION
  ##* Check if object is a portfolio class object.
  ##*
  ##* AUTHOR
  ##* SK
  
  res <- try(searchPrefix(obj), silent = TRUE)
  
  if(is(res, "try-error")) {
    return(FALSE)
  }
  
  return(TRUE)
}

getDbCon = function(x,...)
{
  UseMethod("getDbCon")
}

getDbCon.default = function(x)
{
  return(x$dbCon)
}

getID = function(x,...)
{
  UseMethod("getID")
}

getID.default = function(x)
{
  ##* returns the id of the object x; if x$id doesn't exist returns NULL
  return(x$id)  
}

prepareLoad = function(x,...)
{
  ##* generic function for preparing objects for usage after loading from a db
  UseMethod("prepareLoad")
}

prepareLoad.default = function(obj)
{
  return(obj)
}

objectExists = function(x,...)
{
  UseMethod("objectExists")
}

objectExists.default = function(o, envir = NULL)
{
  if(is.null(o$id) || length(o$id)!=1) {
    stop("objectExists.default: Given object has an invalid id attribute.")
  }  
  if(is.null(envir)) {
    if(!exists(paste(".",class(o)[1],sep=""), envir = .GlobalEnv, inherits = FALSE)){
      stop(paste("objectExists.default: Variable ", paste(".",class(o)[1],sep="")," not found in the .GlobalEnv. Class initialized?", sep =""))
    }
    envir = get(paste(".",class(o)[1],sep=""), envir = .GlobalEnv, inherits = FALSE)
  }  
  if(!exists(o$id, where = envir, inherits = FALSE)) {
    return(FALSE)
  }
  
  return(TRUE)
}

"[.dataContainer" = function(dc, i = 1:nrow(dc$data), j = 1:ncol(dc$data)){
  
  dc_tmp <- dcInit(id = paste(dc$id, as.character(runif(1)), sep = "_"), date = dc$date[i], data = dc$data[i, j, drop = FALSE], description = dc$description,
                   dbCon = dc$dbCon)
  
  while(objectExists(dc_tmp)) {
    dc_tmp$id = paste(dc$id, as.character(runif(1)), sep = "_")
  }
  
  return(dc_tmp)
}

printd = function(s) {
  #dynamic printing to console
  s = as.character(s)
  
  if(exists('.printdLast', envir = .GlobalEnv)) {
    cat(gsub(', ','',toString(rep('\b',nchar(get('.printdLast', envir = .GlobalEnv))))))
  }
  
  if(nchar(s)!=0) {
    assign('.printdLast', s, envir = .GlobalEnv)
    
    cat(s)
  }
  
  flush.console()
  
}
