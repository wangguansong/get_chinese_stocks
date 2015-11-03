CleanSymbols.netease <- function(filename, savefile, savedfname,
                         translate=FALSE, checktype=TRUE,
                         checkcode=TRUE, checkdate=TRUE) {
  # Clean the csv file of the daily returns of a stock of Shanghai or
  # Shenzhen market.
  # Args:
  #   filename: a dataframe or path to a csv file.
  #   savefile: a string. If given, save to a RData file or a csv file
  #       depending on extension. If no extension, save to RData.
  #       If missing, will not save.
  #   savedfname: a string. If given, use as the name of data frame in
  #       the RData. If missing, use the savefile basename starting with
  #       an "S".
  #   translate: a boolean. If TRUE, translate the column name to En.
  #   checktype: a boolean. If TRUE, check the type of each column.
  #   checkcode: a boolean. If TRUE, delete the ' character in code.
  #   checkdate: a boolean. If TRUE, convert DATE column into POSIT
  #       format and sort from old to new.

  if (is.character(filename)) {
    tempdf <- read.csv(filename, stringsAsFactors=FALSE,
                       na.strings=c("None", "NA", ""))
  } else {
    tempdf <- filename
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
    sapply(tempdf[, factorcol], as.factor)
  }


  # Change DATE to POSIT format, and sort from old to new
  if (checkdate) {
    datecol <- which(colnames(tempdf) %in% c("日期", "DATE"))
    if (is.factor(tempdf[, datecol]) | is.character(tempdf[, datecol])){
        tempdf[, datecol] <- as.Date(as.character(tempdf[, datecol]),
                                     format="%Y-%m-%d")
        tempdf <- tempdf[order(tempdf[, datecol]), ]
        }
  }

  # Save or Return
  if (!missing(savefile)) {
    namesplitted <- strsplit(basename(savefile), "\\.")[[1]]
    extname <- namesplitted[length(namesplitted)]
    if (missing(savedfname)) {
      savedfname <- paste(c("S", namesplitted[-length(namesplitted)]),
                          collapse="")
    }
    if (extname %in% c("CSV", "csv")) {
      write.csv(tempdf, file=savefile, fileEncoding="utf-8")
    } else if (extname %in% c("RData", "rdata", "RDATA")) {
      assign(savedfname, tempdf)
      save(list=savedfname, file=savefile)
    } else {
      assign(savefile, tempdf)
      save(list=savefile, file=paste(savefile, ".RData", sep=""))
    }
  }

  invisible(tempdf)

}
