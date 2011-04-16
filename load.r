listoffiles <- paste("R/", list.files("R", "^.+\\.r$"), sep = "")
for (i in listoffiles) source(i)

source("R/RweaveAscii_R2.13.r")
source("R/weaver_R2.13.r")
source("R/cacheSweaveAscii.r")
source("R/cacheSweaveDrivers.r")
