# all S3method
methods("ascii") -> meth
cat(paste("S3method(\"ascii\", \"", substr(meth, 7, 50), "\")", sep = ""), sep = "\n")

# all alias
cat(paste("\\alias{", c("ascii", meth), "}", sep = ""), sep = "\n")

