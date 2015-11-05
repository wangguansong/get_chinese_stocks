# Get all symbols (as in 2015/09/08) of Shanghai and Shenzhen markets.
# Intra-daily prices
# Cleaning:
#   - translate the column names to English.
#   - set TYPE column as factors
#   - set price columns as numeric
#   - round CHG
#   - convert TIME into seconds over midnight
#   - save to RData
#   - delete original xls files and skip empty files
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


## setup
source("GetSymbolsHF.netease.R")
source("CleanSymbolsHF.netease.R")
inputs <- commandArgs(trailingOnly=TRUE)
if (length(inputs)==1) {
  dates <- inputs
} else {
  dates <- "20151102"
}
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


for (d in dates) {
  datedir <- paste(dir, "/", d, sep="")
  if (! dir.exists(datedir)) dir.create(datedir)

  for (i in code.list[1:10]) {
    if (substr(i, 1, 1)=="0") {
      codename <- paste("SH", substr(i, 2, 7), "_", d, sep="")
    } else if (substr(i, 1, 1)=="1"){
      codename <- paste("SZ", substr(i, 2, 7), "_", d, sep="")
    }

    destfile <- paste(datedir, "/", codename, ".xls", sep="")
    GetSymbolsHF.netease(i, d, savefile=destfile, quiet=TRUE)

    if (file.exists(destfile)) {
      cleanfile <- gsub(".xls", ".RData", destfile)
      CleanSymbolsHF.netease(destfile, savefile=cleanfile,
                             dfname=codename)
      file.remove(destfile)
    }
  }
}

remove(cleanfile, codename, d, datedir, destfile, dir, i, inputs)
