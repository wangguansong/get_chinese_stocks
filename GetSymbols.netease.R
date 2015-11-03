GetSymbols.netease <- function(code, from, to, market,
                               savefile=NA, loadname,
                               quiet=TRUE,
                               translate=TRUE,
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
  #   from: a numeric date in the format yyyymmdd. It is set to 19900101
  #       if not given, for 1990 is the year when the two markets were
  #       openned.
  #   to: a numeric date in the format yyyymmdd. It is set to system
  #       date if not given.
  #   market: a character represents the market:
  #         Shanghai  "SH", "Shanghai", or "0"
  #         Shenzhen  "SZ", "Shenzhen", or "1"
  #   savefile: a string. If given, the path of the file to save.
  #       If it is NA, the data will not be saved in file.
  #   loadname: a string. If given, the name used to load the data.
  #       If missing, it is set to market code ("SH/SZ") and the 6
  #       digits code, e.g. SH000001. If it is set to NA, then the
  #       downloaded file will not be loaded.
  #   utf8encoding: a boolean.
  #   quiet: a boolean.
  #   fields: a vector of strings. The fields to download.
  #   translate: a boolean. TRUE to translate column names to english.
  # Returns:
  #   Null

  if (nchar(code) < 6) {
    code <- paste(c(rep(0, 6-nchar(code)), code), collapse="")
  }
  if (nchar(code) == 6) {
    if (missing("market")) {
      stop("DownlaodStock: need market code")
    } else if (market %in% c("0", "Shanghai", "SH")) {
      code <- paste("0", code, sep="")
    } else if (market %in% c("1", "Shenzhen", "SZ")) {
      code <- paste("1", code, sep="")
    } else {
      stop("Cannot recognize market")
    }
  }

  if (missing("from")) {
    from <- "19900101"
  }

  if (missing("to")) {
    to <- format(Sys.Date(), format="%Y%m%d")
  }


  link <- paste("http://quotes.money.163.com/service/chddata.html?",
                "code=", code, "&",
                "start=", from, "&",
                "end=", to, "&",
                "fields=", paste(fields, collapse=";"),
                sep="")

  dltmp <- tempfile()
  on.exit(unlink(dltmp))
  dlflag <- try(download.file(url=link, destfile=dltmp, quiet=quiet),
                silent=TRUE)
  if (dlflag==0) {
    cat(paste("Got historical prices of", code, ".\n"))
  } else {
    file.remove(savefile)
    cat(paste("Failed to get historical prices of", code, ".\n"))
  }

  if (missing("loadname")) {
    if (substr(code, 1, 1)=="0") {
      loadname <- paste("SH", substr(code, 2, 7), sep="")
    } else if (substr(code, 1, 1)=="1"){
      loadname <- paste("SZ", substr(code, 2, 7), sep="")
    }
  }

  if (is.na(loadname) & is.na(savefile)) {
    cat(paste(code, "is downloaded and nothing is done.\n"))
    return()
  } else {
    temptable <- read.csv(dltmp, fileEncoding="gb18030",
                          na.strings=c("None", "NA", ""))
    if (translate) {
      colnames(temptable) <- c("DATE", "CODE", "NAME", fields)
    }
    if (!is.na(savefile)) {
      write.csv(temptable, savefile, fileEncoding="utf-8",
                row.names=FALSE)
    }
    if (!is.na(loadname)) {
      assign(loadname, temptable, envir=parent.frame())
    }
  }

}


