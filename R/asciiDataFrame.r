ascii.data.frame <- function(x, include.rownames = TRUE, include.colnames = TRUE, rownames = NULL, colnames = NULL, format = "f", digits = 2, decimal.mark = ".", na.print = "", caption = NULL, caption.level = NULL, width = 0, frame = NULL, grid = NULL, valign = NULL, header = TRUE, footer = FALSE, align = "", col.width = 1, style = NULL, tgroup = NULL, n.tgroup = NULL, talign = "c", tvalign = "middle", tstyle = "h", bgroup = NULL, n.bgroup = NULL, balign = "c", bvalign = "middle", bstyle = "h", lgroup = NULL, n.lgroup = NULL, lalign = "c", lvalign = "middle", lstyle = "h", rgroup = NULL, n.rgroup = NULL, ralign = "c", rvalign = "middle", rstyle = "h", ...) {
  
  obj <- asciiTable$new(x, include.rownames, include.colnames, rownames, colnames, format, digits, decimal.mark, na.print, caption, caption.level, width, frame, grid, valign, header, footer, align, col.width, style, tgroup, n.tgroup, talign, tvalign, tstyle, bgroup, n.bgroup, balign, bvalign, bstyle, lgroup, n.lgroup, lalign, lvalign, lstyle, rgroup, n.rgroup, ralign, rvalign, rstyle)
  class(obj) <- c("ascii", "proto", "environment")
  return(obj)
}

# from package reshape (Hadley Wickham : http://had.co.nz/reshape/)
ascii.cast_df <-function (x, include.rownames = TRUE, include.colnames = TRUE, rownames = NULL, colnames = NULL, format = "f", digits = 2, decimal.mark = ".", na.print = "", caption = NULL, caption.level = NULL, width = 0, frame = NULL, grid = NULL, valign = NULL, header = TRUE, footer = FALSE, align = "", col.width = 1, style = NULL, tgroup = NULL, n.tgroup = NULL, talign = "c", tvalign = "middle", tstyle = "h", bgroup = NULL, n.bgroup = NULL, balign = "c", bvalign = "middle", bstyle = "h", lgroup = NULL, n.lgroup = NULL, lalign = "c", lvalign = "middle", lstyle = "h", rgroup = NULL, n.rgroup = NULL, ralign = "c", rvalign = "middle", rstyle = "h", ...) {
  
  x <- as.data.frame(x)
  obj <- asciiTable$new(x, include.rownames, include.colnames, rownames, colnames, format, digits, decimal.mark, na.print, caption, caption.level, width, frame, grid, valign, header, footer, align, col.width, style, tgroup, n.tgroup, talign, tvalign, tstyle, bgroup, n.bgroup, balign, bvalign, bstyle, lgroup, n.lgroup, lalign, lvalign, lstyle, rgroup, n.rgroup, ralign, rvalign, rstyle)
  class(obj) <- c("ascii", "proto", "environment")
  return(obj)
}
