# all S3method
methods("ascii") -> meth
cat(paste("S3method(\"ascii\", \"", meth, "\")", sep = ""), sep = "\n")

# all alias
cat(paste("\\alias{", c("ascii", meth), "}", sep = ""), sep = "\n")

