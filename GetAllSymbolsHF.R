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


## setup

inputs <- commandArgs(trailingOnly=TRUE)
if (length(inputs)==1) {
  dates <- inputs
} else {
  dates <- 20160125:20160129
}

source("GetSymbolsHF.netease.R")
source("CleanSymbolsHF.netease.R")
source("GenCodeList.R")

dir <- "stockfiles"
if (! dir.exists(dir)) dir.create(dir)



for (d in dates) {
  datedir <- paste(dir, "/", d, sep="")
  if (! dir.exists(datedir)) dir.create(datedir)

  for (i in code.list) {
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
