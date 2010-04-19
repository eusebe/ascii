ascii.default <- function(x, include.rownames = TRUE, include.colnames = TRUE, rownames = NULL, colnames = NULL, format = "f", digits = 2, decimal.mark = ".", na.print = "", caption = NULL, caption.level = NULL, width = 0, frame = NULL, grid = NULL, valign = NULL, header = TRUE, footer = FALSE, align = NULL, col.width = 1, style = NULL, tgroup = NULL, n.tgroup = NULL, talign = "c", tvalign = "middle", tstyle = "h", bgroup = NULL, n.bgroup = NULL, balign = "c", bvalign = "middle", bstyle = "h", lgroup = NULL, n.lgroup = NULL, lalign = "c", lvalign = "middle", lstyle = "h", rgroup = NULL, n.rgroup = NULL, ralign = "c", rvalign = "middle", rstyle = "h", list.type = "bullet", ...) {
  if (is.list(x)) {
    x <- lapply(x, as.character)
    obj <- asciiList$new(x, caption = caption, caption.level = caption.level, list.type = list.type)
  }
  else {
    y <- as.data.frame(x)
    obj <- asciiTable$new(y, include.rownames, include.colnames, rownames, colnames, format, digits, decimal.mark, na.print, caption, caption.level, width, frame, grid, valign, header, footer, align, col.width, style, tgroup = tgroup, n.tgroup = n.tgroup, talign = talign, tvalign = tvalign, tstyle = tstyle, bgroup = bgroup, n.bgroup = n.bgroup, balign = balign, bvalign = bvalign, bstyle = bstyle, lgroup = lgroup, n.lgroup = n.lgroup, lalign = lalign, lvalign = lvalign, lstyle = lstyle, rgroup = rgroup, n.rgroup = n.rgroup, ralign = ralign, rvalign = rvalign, rstyle = rstyle)
  }
  class(obj) <- c("ascii", "proto", "environment")
  return(obj)
}
