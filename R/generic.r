setGeneric(
  name = "ascii", 
  def = function(x, ...){standardGeneric("ascii")}
)

ascii <- function (x, ...) UseMethod("ascii")
