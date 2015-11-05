# Get all symbols (as in 2015/09/08) of Shanghai and Shenzhen markets.
# Clean the historical prices:
#   - translate the column names to English.
#   - delete "'" in CODE column
#   - set CODE and NAME column as factors
#   - set price columns as numeric
#   - format DATE as POSIT date
#   - save to RData
# Codes:
#   ShangHai A Shares     0600000 ~ 0601999
#                         0603000 ~ 0603998
#   ShangHai B Shares     0900901 ~ 0900957
#   ShangHai Indices      0000001 ~ 0000162
#                         0000300
#                         0000801 ~ 0000999
#   ShenZhen A Shares     1000001 ~ 1002783
#                         1300001 ~ 1300498
#   ShenZhen B Shares     1200011 ~ 1200992
#   ShenZhen Indices      1399001 ~ 1399998


## Download all
source("GetSymbols.netease.R")
source("CleanSymbols.netease.R")
dir <- "stockfiles"
if (! dir.exists(dir)) dir.create(dir)

# list of the codes
code.list <-
  c(paste("0", as.character(600000L + 0:1999), sep=""),  # ShangHai A
    paste("0", as.character(603000L + 0:3998), sep=""),
    paste("0", as.character(900900L + 1:57), sep=""),    # ShangHai B
    substr(as.character(10000000L + c(1:162, 300, 801:999)), 2, 8),
                                                         # ShangHai Idx
    as.character(1000000L + 1:2783),     # ShenZhen A
    as.character(1300000L + 1:498),
    as.character(1200000L + 11:992),     # ShenZhen B
    as.character(1399000L + 1:998))      # ShenZhen Idx

for (i in code.list) {
  if (substr(i, 1, 1)=="0") {
    codename <- paste("SH", substr(i, 2, 7), sep="")
  } else if (substr(i, 1, 1)=="1"){
    codename <- paste("SZ", substr(i, 2, 7), sep="")
  }
  destfile <- paste(dir, "/", codename, ".csv", sep="")
  GetSymbols.netease(code=i, savefile=destfile, dfname=NA,
                     translate=TRUE, quiet=TRUE)

  linecount <- system(paste("wc -l", destfile, sep=" "), intern=TRUE)
  linecount <- as.numeric(strsplit(linecount, split=" ")[[1]][1])
  if (linecount <=1) {
    file.remove(destfile)
    next
  }

  CleanSymbols.netease(destfile, gsub("csv", "RData", destfile),
                       dfname=codename, translate=FALSE)
  file.remove(destfile)

}

remove(destfile, dir, i, linecount, codename)
