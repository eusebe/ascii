setClass(
  "R2asciidocDataFrame",
  representation = representation(
    x = "data.frame",
    include.colnames = "logical",
    include.rownames = "logical",
    format           = "character", 
    digits           = "numeric", 
    decimal.mark     = "character", 
    na.print         = "character"
  )
)

#~ setClass(
#~   "R2asciidoc.list",
#~   representation = representation(
#~     x = "list"
#~   )
#~ )
