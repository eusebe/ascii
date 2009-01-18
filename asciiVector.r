setMethod(
  "ascii",
  "vector",
  function (x, include.rownames = FALSE, include.colnames = FALSE, format = "f", digits = 2, decimal.mark = ".", na.print = ""){
    new("R2asciidocVector", x = x, include.rownames = include.rownames,
         include.colnames = include.colnames, format = format,
         digits = digits, decimal.mark = decimal.mark, na.print = na.print)
  }
)
