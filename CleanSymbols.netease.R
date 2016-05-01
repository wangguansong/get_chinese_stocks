CleanSymbols.netease <- function(x, savefile, dfname,
                         translate=FALSE, checktype=TRUE,
                         checkcode=TRUE, checkdate=TRUE,
                         removezero=TRUE) {
  # Clean the csv file of the daily returns of a stock of Shanghai or
  # Shenzhen market.
  # Args:
  #   x: a data frame or path to a csv file. 
  #   savefile: a string, the path to save the cleaned dadta. The
  #       function will recognize "csv" and "RData" extensions. If it is
  #       not given (missing), overwrite the original file if x is a
  #       path or no file will be created. If is is "NA", no file will
  #       be created.
  #   dfname: a string, the data frame name if saved to RData. If it is
  #       not given (missing) or it is "NA", it is created from the
  #       savefile. If the savefile opens with a number, it is set to
  #       "dailyprices".
  #   translate: a boolean. If TRUE, translate the column name to En.
  #   checktype: a boolean. If TRUE, check the type of each column.
  #   checkcode: a boolean. If TRUE, delete the ' character in code.
  #   checkdate: a boolean. If TRUE, convert DATE column into POSIT
  #       format and sort from old to new.

  if (is.character(x)) {
    tempdf <- read.csv(x, stringsAsFactors=FALSE,
                       na.strings=c("None", "NA", ""))
  } else {
    tempdf <- x
  }

  # Transalte column names
  if (translate) {
    colnames(tempdf)[colnames(tempdf)=="日期"] <- "DATE"
    colnames(tempdf)[colnames(tempdf)=="股票代码"] <- "CODE"
    colnames(tempdf)[colnames(tempdf)=="名称"] <- "NAME"
    colnames(tempdf)[colnames(tempdf)=="收盘价"] <- "TCLOSE"
    colnames(tempdf)[colnames(tempdf)=="最高价"] <- "HIGH"
    colnames(tempdf)[colnames(tempdf)=="最低价"] <- "LOW"
    colnames(tempdf)[colnames(tempdf)=="开盘价"] <- "TOPEN"
    colnames(tempdf)[colnames(tempdf)=="前收盘"] <- "LCLOSE"
    colnames(tempdf)[colnames(tempdf)=="涨跌额"] <- "CHG"
    colnames(tempdf)[colnames(tempdf)=="涨跌幅"] <- "PCHG"
    colnames(tempdf)[colnames(tempdf)=="换手率"] <- "TURNOVER"
    colnames(tempdf)[colnames(tempdf)=="成交量"] <- "VOTURNOVER"
    colnames(tempdf)[colnames(tempdf)=="成交金额"] <- "VATURNOVER"
    colnames(tempdf)[colnames(tempdf)=="总市值"] <- "TCAP"
    colnames(tempdf)[colnames(tempdf)=="流通市值"] <- "MCAP"
  }

  # Delete the "'" of the code.
  if (checkcode) {
    codecol <- which(colnames(tempdf) %in% c("股票代码", "CODE"))
    tempdf[, codecol] <- gsub("'", "", tempdf[, codecol])
  }

  # Check types of columns
  if (checktype) {
    numcol <- which(colnames(tempdf) %in%
      c("收盘价", "TCLOSE", "最高价", "HIGH", "最低价", "LOW",
        "开盘价", "TOPEN", "前收盘", "LCLOSE", "涨跌额", "CHG",
        "涨跌幅", "PCHG", "换手率", "TURNOVER", "成交量", "VOTURNOVER",
        "成交金额", "VATURNOVER", "总市值", "TCAP", "流通市值", "MCAP"))
    tempdf[, numcol] <- sapply(tempdf[, numcol], as.numeric)
    factorcol <- which(colnames(tempdf) %in%
      c("股票代码", "CODE", "名称", "NAME"))
    tempdf[, factorcol] <- sapply(tempdf[, factorcol], as.factor)
  }

  # Remove the rows with zero as TCLOSE.
  if (removezero) {
    tclosecol <- which(colnames(tempdf) %in% c("收盘价", "TCLOSE"))
    tempdf <- tempdf[tempdf[, tclosecol]>0, ]
    rownames(tempdf) <- 1:nrow(tempdf)
  }


  # Change DATE to POSIT format, and sort from old to new
  if (checkdate) {
    datecol <- which(colnames(tempdf) %in% c("日期", "DATE"))
    if (is.factor(tempdf[, datecol]) | is.character(tempdf[, datecol])){
        tempdf[, datecol] <- as.Date(as.character(tempdf[, datecol]),
                                     format="%Y-%m-%d")
        tempdf <- tempdf[order(tempdf[, datecol]), ]
        }
    rownames(tempdf) <- 1:nrow(tempdf)
  }

  # Save or Return
  if (missing("savefile")) {
    if (is.character(x)) {
      savefile <- x
    } else {
      savefile <- NA
    }
  }
  
  if (!is.na(savefile)) {
    savefilesplit <- strsplit(basename(savefile), "\\.")[[1]]
    if (length(savefilesplit)==1) {
      savebext <- "RData"
      savebase <- savefilesplit
    } else if (length(savefilesplit)>1) {
      saveext <- savefilesplit[length(savefilesplit)]
      savebase <- paste(savefilesplit[-length(savefilesplit)],
                        collapse=".")
    }
    
    if (missing(dfname) || is.na(dfname)) {
      if (substr(savebase, 1, 1) %in% as.character(0:9)) {
        dfname <- "dailyprices"
      } else {
        dfname <- savebase
      }
    }
    
    if (saveext %in% c("CSV", "csv")) {
      write.csv(tempdf, file=savefile, fileEncoding="utf-8")
    } else if (saveext %in% c("RData", "rdata", "RDATA")) {
      assign(dfname, tempdf)
      save(list=dfname, file=savefile)
    } else {
      stop(paste("Cannot save to", saveext, "format"))
    }
  }

  invisible(tempdf)

}
