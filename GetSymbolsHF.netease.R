GetSymbolsHF.netease <- function(code, date, market,
                                 savefile, quiet=TRUE) {
  # Download high frequence intra-daily prices of a symbol, then load
  # to the environment or save to a file.
  #
  # Args:
  #   code: a numeric code of the stock. It can be 6 digits paired with
  #       the value of the market, or it can be 7 digits where the first
  #       of which indicates the market:
  #         0   Shanghai
  #         1   Shenzhen
  #       If the code has less than 6 digits, it's prefixed with 0's.
  #   date: a numeric date in the format yyyymmdd. It is set to system
  #       date if not given.
  #   market: a character represents the market:
  #         Shanghai  "SH", "Shanghai", or "0"
  #         Shenzhen  "SZ", "Shenzhen", or "1"
  #   savefile: a string, the path to save the data in csv format. If it
  #       is not given (missing), use (SH/SZ)+(6 digits)+(_date).csv.
  #       If it is set to "NA", no file will be created.
  #   quiet: a boolean, passed to download.file(...).
  # Returns:
  #   codename: (SH/SZ)+(6 digits)+(_date), invisible.
  # Note:
  #   It is tricky to load an xls file into R so I leave that part out
  #   of this function. In CleanSymbolsHF.netease() I use "unoconv" to
  #   convert xls files into csv files. There's also "gdata" package.
  #   But as far as I know, both methods work only in Linux/Unix.
  
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

  if (missing(date)) {
    date <- format(Sys.Date(), format="%Y%m%d")
  }
  
  # create a name of the code (SH/SZ)+(6 digits)+(_date)
  if (substr(code, 1, 1)=="0") {
    codename <- paste("SH", substr(code, 2, 7), "_", date, sep="")
  } else if (substr(code, 1, 1)=="1"){
    codename <- paste("SZ", substr(code, 2, 7), "_", date, sep="")
  }

  link <- paste("http://quotes.money.163.com/cjmx/",
                substring(date, 1, 4), "/", date, "/",
                code, ".xls",
                sep="")
  
  dltmp <- tempfile()

  dlflag <- try(download.file(link, destfile=dltmp, quiet=quiet),
                silent=FALSE)
  if (dlflag==0) {
    cat(paste("Got intradaily prices of", codename))
  } else {
    cat(paste("Fail to get intradaily prices of", codename))
    cat(".\n")
    return(invisible())
  }
  
  if (missing("savefile")) {
    savefile <- paste(codename, ".xls", sep="")
  }
  
  if (is.na(savefile)) {
    cat(", and nothing is done")
  } else {
    file.copy(dltmp, savefile, overwrite=TRUE)
    cat(paste(", saved to", savefile))
  }
  cat(".\n")

  invisible(code)

}
