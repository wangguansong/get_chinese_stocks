# Get all financial statements of Shanghai and Shenzhen markets.



## Download all
source("GetFinancialInfo.netease.R")
dir <- "financialinfo"
if (! dir.exists(dir)) dir.create(dir)

# list of the codes
source("GenCodeList.R")

# list of tables
type.list <- c("cwbbzy", "zcfzb", "lrb", "xjllb",
               "zycwzb", "ylnl", "chnl", "cznl", "yynl")

for (i in stock.list) {
  if (substr(i, 1, 1)=="0") {
    codename <- paste("SH", substr(i, 2, 7), sep="")
  } else if (substr(i, 1, 1)=="1"){
    codename <- paste("SZ", substr(i, 2, 7), sep="")
  }
  destfile <- paste(dir, "/", codename, "FI", ".RData", sep="")

  for (ti in type.list) {
    GetFinancialInfo.netease(i, savefile=NA, type=ti)
  }

  df.list <- ls(pattern=codename)

  if (length(df.list)>0) {
    save(list=df.list, file=destfile)
    remove(list=df.list)
  }
}

remove(destfile, dir, i, ti, codename, type.list, df.list)
