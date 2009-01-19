setMethod(
  "ascii",
  "data.frame",
  function (x, include.rownames = TRUE, include.colnames = TRUE, format = "f", digits = 2, decimal.mark = ".", na.print = "", caption = "", width = 0, frame = "", grid = "", valign = "", header = TRUE, footer = FALSE, align = "", col.width = 1, style = ""){
    new("R2asciidocDataFrame", x = x, include.rownames = include.rownames,
         include.colnames = include.colnames, format = format,
         digits = digits, decimal.mark = decimal.mark, na.print = na.print,
         caption = caption, width = width, frame = frame, grid = grid,
         valign = valign, header = header, footer = footer, align = align,
         col.width = col.width, style = style)
  }
)

# from package reshape (Hadley Wickham : http://had.co.nz/reshape/)
#~ setMethod(
#~   "ascii",
#~   "cast_df",
#~   function (x, include.rownames = TRUE, include.colnames = TRUE, format = "f", digits = 2, decimal.mark = ".", na.print = ""){
#~     new("R2asciidocDataFrame", x = x, include.rownames = include.rownames,
#~          include.colnames = include.colnames, format = format,
#~          digits = digits, decimal.mark = decimal.mark, na.print = na.print)
#~   }
#~ )

## FIXME cast_df:
#~ Warning message:
#~ In matchSignature(signature, fdef, where) :
#~   in the method signature for function "ascii" no definition for class: “cast_df”
