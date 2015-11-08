GetFinancialInfo.sina <-
  function(code, savefile, dfname,
           translate=FALSE, clean=TRUE, type="lrb", quiet=TRUE) {
  # Download financial statement tables of a stock.
  # sina
  #
  #   code: a numeric code of the stock. It can be 6 digits, or it can
  #       be 7 digits where the first of which indicates the market:
  #         0   Shanghai
  #         1   Shenzhen
  #       If the code has less than 6 digits, it's prefixed with 0's.
  #   savefile: a string, the path to save the data in csv format. If it
  #       is not given (missing), use (SH/SZ)+(6 digits).csv. If it is
  #       set to "NA", no file will be created.
  #   dfname: a string, the name of the data frame to be loaded into
  #       the environment. If it is not given (missing), it is set to
  #       (SH/SZ)+(6 digits). If it is set to "NA", the data will not be
  #       loaded.
  #   type: a string, valued as "zcfzb", "lrb", or "xjllb". It controls
  #       which table to download.
  #   clean: a boolean. TRUE to clean the downloaded data.
  #   translate: a boolean. TRUE to translate column names to english.
  #   quiet: a boolean, passed to download.file(...).

  # Make code into 6 digits if it's not
  if (nchar(code) < 6) {
    code <- paste(c(rep(0, 6-nchar(code)), code), collapse="")
  }
  if (nchar(code) == 7) {
    code <- substr(code, 2, 7)
  }

  # codename for default savefile and dfname values
  # made to be consistent with GetSymbols functions,
  if (substr(code, 1, 1) %in% c("6", "9")) {
    codename <- paste("SH", code, sep="")
  } else if (substr(code, 1, 1) %in% c("0", "3", "2")) {
    codename <- paste("SZ", code, sep="")
  }

  # Construct link
  type <- tolower(type)
  if (type %in% c("zcfzb", "z")) {
    type <- "zcfzb"
    table <- "BalanceSheet"
  } else if (type %in% c("lrb", "l")) {
    type <- "lrb"
    table <- "ProfitStatement"
  } else if (type %in% c("xjllb", "x")) {
    type <- "xjllb"
    table <- "CashFlow"
  } else {
    stop(paste("Table:", type, " is not avaiable for ", codename,
               sep=""))
  }
  link <- paste("http://money.finance.sina.com.cn/corp/go.php/vDOWN_",
                table, "/displaytype/4/stockid/", code,
                "/ctrl/all.phtml", sep="")

  # Download link to a temporary file
  dltmp <- tempfile()
  on.exit(unlink(dltmp))
  dlflag <- try(download.file(url=link, destfile=dltmp, quiet=quiet),
                silent=TRUE)
  if (dlflag==0) {
    cat(paste("Got information of", codename))
  } else {
    file.remove(dltmp)
    cat(paste("Failed to get information of", codename))
    cat(".\n")
    return(invisible(codename))
  }

  # Save file or load data
  if (missing("savefile")) {
    savefile <- paste(codename, "_", toupper(type), ".csv", sep="")
  }
  if (missing("dfname")) {
    dfname <- paste(codename, toupper(type), sep="")
  }


  if (is.na(savefile) & is.na(dfname)) {
    cat(paste(", and nothing is done.\n"))
    return(invisible(codename))
  }

  linecount <- system(paste("wc -l", dltmp, sep=" "), intern=TRUE)
  linecount <- as.numeric(strsplit(linecount, split=" ")[[1]][1])
  if (linecount <=1) {
    cat(paste(", but it is empty.\n"))
    return(invisible(codename))
  }


  if (!clean) {
    file.copy(from=dltmp, to=savefile, overwrite=TRUE)
    cat(paste(", saved to", savefile))
  } else {
    tempdf <- read.csv(file=dltmp, header=FALSE, stringsAsFactors=FALSE,
                       sep="\t", fileEncoding="gb18030",
                       na.strings=c(""))
    tempdf <- t(tempdf)
    tempdf[grep("--", tempdf)] <- NA
    if (any(is.na(tempdf[, 1]))) {
      tempdf <- tempdf[-which(is.na(tempdf[, 1])), ]
    }
    header <- tempdf[1, ]
    header <- gsub("^[\t ]+", "", header)
    if (sum(header=="")>0) {
      tempdf <- tempdf[, -which(header=="")]
    }
    header <- tempdf[1, ]
    tempdf <- data.frame(tempdf[-1, ], stringsAsFactors=FALSE)
    colnames(tempdf) <- header
    rownames(tempdf) <- NULL
    tempdf[, -(1:2)] <- sapply(tempdf[, -(1:2)], as.numeric)
    tempdf[, 1] <- as.Date(as.character(tempdf[, 1]),
                           format="%Y%m%d")

    cat(", cleaned")
    if (!is.na(savefile)) {
      write.csv(tempdf, savefile, fileEncoding="utf-8", row.names=FALSE)
      cat(paste(", saved to", savefile))
    }
    if (!is.na(dfname)) {
      assign(dfname, tempdf, envir=parent.frame())
      cat(paste(", loaded as", dfname))
    }
  }

  cat(".\n")
  return(invisible(codename))

}
