setClass(
  "R2asciidocDataFrame",
  representation = representation(
    x = "data.frame",
    include.colnames = "logical",
    include.rownames = "logical",
    format           = "character", 
    digits           = "numeric", 
    decimal.mark     = "character", 
    na.print         = "character",
    caption          = "character",
    width            = "numeric",
    frame            = "character",
    grid             = "character",
    valign           = "character",
    header           = "logical",
    footer           = "logical",
    align            = "character",
    col.width        = "numeric",
    style            = "character"
  )
)

setClass(
  "R2asciidocVector",
  representation = representation(
    x = "vector",
    include.colnames = "logical",
    include.rownames = "logical",
    format           = "character", 
    digits           = "numeric", 
    decimal.mark     = "character", 
    na.print         = "character",
    caption          = "character",
    width            = "numeric",
    frame            = "character",
    grid             = "character",
    valign           = "character",
    header           = "logical",
    footer           = "logical",
    align            = "character",
    col.width        = "numeric",
    style            = "character"
  )
)

setClass(
  "R2asciidocList",
  representation = representation(
    x = "list"
  )
)
