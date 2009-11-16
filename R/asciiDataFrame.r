ascii.data.frame <- function(x, include.rownames = TRUE, include.colnames = TRUE, rownames = NULL, colnames = NULL, format = "f", digits = 2, decimal.mark = ".", na.print = "", caption = "", caption.level = "", width = 0, frame = "", grid = "", valign = "", header = TRUE, footer = FALSE, align = "", col.width = 1, style = "", tgroup = NULL, n.tgroup = NULL, talign = "", tvalign = "", tstyle = "", bgroup = NULL, n.bgroup = NULL, balign = "", bvalign = "", bstyle = "", lgroup = NULL, n.lgroup = NULL, lalign = "", lvalign = "", lstyle = "", ...) {
  obj <- asciiDataFrame$new(x, include.rownames, include.colnames, rownames, colnames, format, digits, decimal.mark, na.print, caption, caption.level, width, frame, grid, valign, header, footer, align, col.width, style, tgroup, n.tgroup, talign, tvalign, tstyle, bgroup, n.bgroup, balign, bvalign, bstyle, lgroup, n.lgroup, lalign, lvalign, lstyle)
	class(obj) <- c("ascii", "proto", "environment")
	return(obj)
}

#~ # from package reshape (Hadley Wickham : http://had.co.nz/reshape/)
ascii.cast_df <-function (x, include.rownames = FALSE, include.colnames = TRUE, rownames = NULL, colnames = NULL, format = "f", digits = 2, decimal.mark = ".", na.print = "", caption = "", caption.level = "", width = 0, frame = "", grid = "", valign = "", header = TRUE, footer = FALSE, align = "", col.width = 1, style = "", cgroup = NULL, n.cgroup = NULL, calign = "", cvalign = "", cstyle = "", rgroup = NULL, n.rgroup = NULL, ralign = "", rvalign = "", rstyle = "", ...){
    x <- as.data.frame(x)
  obj <- asciiDataFrame$new(x, include.rownames, include.colnames, rownames, colnames, format, digits, decimal.mark, na.print, caption, caption.level, width, frame, grid, valign, header, footer, align, col.width, style, cgroup, n.cgroup, calign, cvalign, cstyle, rgroup, n.rgroup, ralign, rvalign, rstyle)
	class(obj) <- c("ascii", "proto", "environment")
	return(obj)
  }
