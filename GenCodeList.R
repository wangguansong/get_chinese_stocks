# Codes:
# Update: 2016-02-01
#   ShangHai A Shares     0600000 ~ 0601999
#                         0603000 ~ 0603999
#   ShangHai B Shares     0900901 ~ 0900957
#   ShangHai Indices      0000001 ~ 0000162
#                         0000300
#                         0000801 ~ 0000999
#   ShenZhen A Shares     1000001 ~ 1002783
#                         1300001 ~ 1300498
#   ShenZhen B Shares     1200011 ~ 1200992
#   ShenZhen Indices      1399001 ~ 1399998


# list of the codes
stock.list <-
  c(paste("0", as.character(600000L + 0:1999), sep=""),  # ShangHai A
    paste("0", as.character(603000L + 0:999), sep=""),
    paste("0", as.character(900900L + 1:57), sep=""),    # ShangHai B
    as.character(1000000L + 1:999),     # ShenZhen A
    "1001696", "1001896", "1001979",
    as.character(1002000L + 1:800),
    as.character(1300000L + 1:501),
    as.character(1200000L + 2:58),     # ShenZhen B
    "1200152", "1200160", "1200168",
    "1200413", "1200418", "1200429", "1200468", "1200488",
    "1200505", "1200512", "1200513", "1200521", "1200530", "1200539",
    "1200541", "1200550", "1200553", "1200570", "1200581", "1200596",
    "1200613", "1200625",
    "1200706", "1200725", "1200726", "1200761", "1200770", "1200771",
    "1200869", "1200986", "1200992")
    
index.list <-
  c(substr(as.character(10000000L + c(1:162, 300, 801:999)), 2, 8),
                                                         # ShangHai Idx
    as.character(1399000L + 1:998))      # ShenZhen Idx

code.list <- c(stock.list, index.list)
