GetSymbols.netease <- function(code, market, from, to, savefile, dfname,
                               quiet=TRUE, translate=TRUE,
                               fields=c("TCLOSE", "HIGH", "LOW", "TOPEN",
                                        "LCLOSE", "CHG", "PCHG",
                                        "TURNOVER", "VOTURNOVER",
                                        "VATURNOVER", "TCAP", "MCAP")) {
  # Download historical prices of a symbol, then load to the enviroment
  # or save to a file.
  #
  # Args:
  #   code: a numeric code of the stock. It can be 6 digits paired with
  #       the value of the market, or it can be 7 digits where the first
  #       of which indicates the market:
  #         0   Shanghai
  #         1   Shenzhen
  #       If the code has less than 6 digits, it's prefixed with 0's.
  #       If the code has 6 digits, it's assumed to be a stock instead of
  #       an index, and the market is deduced from the first digit.
  #   market: a character represents the market:
  #         Shanghai  "SH", "Shanghai", or "0"
  #         Shenzhen  "SZ", "Shenzhen", or "1"
  #   from: a numeric date in the format yyyymmdd. It is set to 19900101
  #       if not given, for 1990 is the year when the two markets were
  #       openned.
  #   to: a numeric date in the format yyyymmdd. It is set to system
  #       date if not given.
  #   savefile: a string, the path to save the data in csv format. If it
  #       is not given (missing), use "(SH/SZ)+(6 digits).csv". If it is
  #       set to "NA", no file will be created.
  #   dfname: a string, the name of the data frame to be loaded into
  #       the environment. If it is not given (missing), it is set to
  #       (SH/SZ)+(6 digits). If it is set to "NA", the data will not
  #       be loaded.
  #   quiet: a boolean, passed to download.file(...).
  #   fields: a vector of strings, the selected fields to download.
  #   translate: a boolean. TRUE to translate column names to english.
  # Returns:
  #   codename: (SH/SZ)+(6 digits), invisible.
  # Side Effects:
  #   A data frame loaded with name "dfname", (or) a csv file saved with
  #   name "savefile".

  # Make code into 7 digits if it's not
  if (nchar(code) < 6) {
    code <- paste(c(rep(0, 6-nchar(code)), code), collapse="")
  }
  if (nchar(code)==7) {
    market <- substr(code, 1, 1)
    code <- substr(code, 2, 7)
  }
  if (nchar(code) == 6) {
    if (missing("market")) {
      market <- ifelse(substr(code, 1, 1)<5, "SZ", "SH")
    } else if (market %in% c("0", "Shanghai", "SH", "sh")) {
      market <- "SH"
    } else if (market %in% c("1", "Shenzhen", "SZ", "sz")) {
      market <- "SZ"
    } else {
      stop("Cannot recognize market")
    }
  }
  if (missing("savefile")) {
    savefile <- paste(codename, ".csv", sep="")
  }
  if (missing("dfname")) {
    dfname <- codename
  }

  # codename is for the name of data frame and file to save.
  # codenum is for the link to netease.
  codename <- paste(market, code, sep="")
  codenum <- paste(ifelse(market=="SH", 0, 1), code, sep="")

  # Set default dates
  if (missing("from")) {
    from <- "19900101"
  }
  if (missing("to")) {
    to <- format(Sys.Date(), format="%Y%m%d")
  }
  
  # Check dfname
  if (substr(dfname, 1, 1) %in% as.character(0:9)) {
    stop("dfname cannot open with a number.")
  }

  link <- paste("http://quotes.money.163.com/service/chddata.html?",
                "code=", codenum, "&",
                "start=", from, "&",
                "end=", to, "&",
                "fields=", paste(fields, collapse=";"),
                sep="")

  dltmp <- tempfile()
  on.exit(unlink(dltmp))
  dlflag <- try(download.file(url=link, destfile=dltmp, quiet=quiet),
                silent=TRUE)
  if (dlflag==0) {
    cat(paste("Got historical prices of", codename))
  } else {
    file.remove(savefile)
    cat(paste("Failed to get historical prices of", codename))
  }
  
  if (missing("savefile")) {
    savefile <- paste(codename, ".csv", sep="")
  }
  if (missing("dfname")) {
    dfname <- codename
  }

  if (is.na(dfname) & is.na(savefile)) {
    cat(paste(", and nothing is done.\n"))
    invisible(codename)
  } else {
    temptable <- read.csv(dltmp, fileEncoding="gb18030",
                          na.strings=c("None", "NA", ""))
    if (translate) {
      colnames(temptable) <- c("DATE", "CODE", "NAME", fields)
    }
    if (!is.na(savefile)) {
      write.csv(temptable, savefile, fileEncoding="utf-8",
                row.names=FALSE)
      cat(paste(", saved to", savefile))
    }
    if (!is.na(dfname)) {
      assign(dfname, temptable, envir=parent.frame())
      cat(paste(", loaded as", dfname))
    }
    cat(".\n")
  }
  invisible(codename)
}
