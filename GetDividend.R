GetDividend.netease <- function(code, market) {
  # Get information on dividend, bonus, allotment, SEO of a stock.
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
  #         Shanghai  "sh", "SH", "Shanghai", or "0"
  #         Shenzhen  "sz", "SZ", "Shenzhen", or "1"
  # Returns:
  #   A list of original tables and a parsed one.
  # Dependent:
  #   rvest

  require("rvest")
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

  url <- paste("http://quotes.money.163.com/f10/fhpg_", code,
               ".html#01d05", sep="")
  infohtml <- read_html(url)

  # Bonus          Convert        Dividend
  # 送股（每10股） 转增（每10股） 派息（每10股） 
  divtable <- infohtml %>%
              html_nodes(xpath="/html/body/div[2]/div[4]/table") %>%
              html_table(fill=TRUE)
  divtable <- divtable[[1]]
  colnames(divtable)[3:5] <- paste(divtable[1, 1:3], "（每10股）",
                                   sep="")
  divtable <- divtable[-1, ]
  divtable <- divtable[!divtable[, 1]=="暂无数据", ]
  # Allotment
  # 配股方案
  pgutable <- infohtml %>%
              html_nodes(xpath="/html/body/div[2]/div[6]/table") %>%
              html_table(fill=TRUE)
  pgutable <- pgutable[[1]]
  pgutable <- pgutable[!pgutable[, 1]=="暂无数据", ]
  # SEO
  # 增发
  zfatable <- infohtml %>%
              html_nodes(xpath="/html/body/div[2]/div[8]/table") %>%
              html_table(fill=TRUE)
  zfatable <- zfatable[[1]]
  zfatable <- zfatable[!zfatable[, 1]=="暂无数据", ]

  histable <- infohtml %>%
              html_nodes(xpath="/html/body/div[2]/div[10]/table") %>%
              html_table(fill=TRUE)
  histable <- histable[[1]]
  histable <- histable[!histable[, 1]=="暂无数据", ]

  adjtable <- divtable[, c(7, 3:5)]
  adjtable <- merge(adjtable, pgutable[, c(8, 2, 3)], all=TRUE,
                    by.x=1, by.y=1)
  colnames(adjtable)[5] <- "配股方案（每10股）"
  for (i in 2:4) {
    adjtable[is.na(adjtable[, i]) | adjtable[, i]=="--", i] <- 0
    adjtable[, i] <- as.numeric(adjtable[, i])
  }
  adjtable[divtable[, 1]=="--", 1] <- NA
  adjtable[, 1] <- as.Date(adjtable[, 1], format="%Y-%m-%d")
  adjtable[, 5] <- as.numeric(sub("10配", "", adjtable[, 5]))
  adjtable[is.na(adjtable[, 5]), 5] <- 0
  adjtable[is.na(adjtable[, 6]), 6] <- 0

  return(list(DIVIDEND=divtable,
              ALLOTMENT=pgutable,
              SEO=zfatable,
              HISTORY=histable,
              ADJUST=adjtable[order(adjtable[, 1]), ]))
}
