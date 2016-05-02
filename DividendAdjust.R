DividendAdjust <- function(stock, adjust, market, mode="backward") {

  # adjust columns:
  #   ExDate(除权除息日)
  #   Bonus(送股): per 10 shares
  #   Convert(转增): per 10 shares
  #   Dividend(派息): per 10 shares
  #   Allotment(配股方案): per 10 shares
  #   Price(配股价)

  if (is.character(stock)) {
    if (file.exists(stock)) {
      # when "stock" is a path to a file
      if (grepl("(csv|CSV)$", stock)) {
        stock <- read.csv(stock, stringsAsFactors=FALSE)
      } else if (grepl("RData$", stock)) {
        # TODO 
      }
    } else {
      # when "stock" is code of a stock
      code <- stock
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
      GetSymbols.netease(code, market, savefile=NA, dfname="stock") 
    }
  } else if (is.data.frame(stock)) {
    # when "stock" is a data frame
  }

  if (missing(adjust)) {
    adjust <- GetDividend.netease(code)$ADJUST
  }

  if (is.unsorted(stock$DATE)) {
    stock <- stock[order(stock$DATE), ]
  }
  adjclose <- stock$TCLOSE
  adjdates <- stock$DATE

  if (mode=="backward") {
    for (i in nrow(adjust):1) {
      adjclose[adjdates<adjust[i, 1]] <-
        (adjclose[adjdates<adjust[i, 1]] - adjust[i, 4]/10 +
         adjust[i, 5]/10 * adjust[i, 6]) /
      (1 + adjust[i, 2]/10 + adjust[i, 3]/10 + adjust[i, 5]/10)
    }
  } else if (mode=="forward") {
    for (i in 1:nrow(adjust)) {
      adjclose[adjdates>=adjust[i, 1]] <-
        adjclose[adjdates>=adjust[i, 1]] *
        (1 + adjust[i, 2]/10 + adjust[i, 3]/10 + adjust[i, 5]/10) +
        adjust[i, 4]/10 - adjust[i, 6] * adjust[i, 5]/10
    }
  }
  return(adjclose)
}
