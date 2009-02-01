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
  "R2asciidocList",
  representation = representation(
    x = "list",
    caption = "character"
  ),
  validity = function(object) {
    for (i in 1:length(object@x)) {
      if (class(object@x[[i]]) != "character") stop("x must be a list of strings.")
    }
  }
)
