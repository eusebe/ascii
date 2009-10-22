ascii.data.frame <- function(x, include.rownames = TRUE, include.colnames = TRUE, format = "f", digits = 2, decimal.mark = ".", na.print = "", caption = "", caption.level = "", width = 0, frame = "", grid = "", valign = "", header = TRUE, footer = FALSE, align = "", col.width = 1, style = "", cgroup = NULL, n.cgroup = NULL, calign = "", cvalign = "", cstyle = "", rgroup = NULL, n.rgroup = NULL, ralign = "", rvalign = "", rstyle = "", ...) {
  obj <- asciiDataFrame$new(x, include.rownames, include.colnames, format, digits, decimal.mark, na.print, caption, caption.level, width, frame, grid, valign, header, footer, align, col.width, style, cgroup, n.cgroup, calign, cvalign, cstyle, rgroup, n.rgroup, ralign, rvalign, rstyle)
	class(obj) <- c("ascii", "proto", "environment")
	return(obj)
}

#~ # from package reshape (Hadley Wickham : http://had.co.nz/reshape/)
ascii.cast_df <-function (x, include.rownames = FALSE, include.colnames = TRUE, format = "f", digits = 2, decimal.mark = ".", na.print = "", caption = "", caption.level = "", width = 0, frame = "", grid = "", valign = "", header = TRUE, footer = FALSE, align = "", col.width = 1, style = "", cgroup = NULL, n.cgroup = NULL, calign = "", cvalign = "", cstyle = "", rgroup = NULL, n.rgroup = NULL, ralign = "", rvalign = "", rstyle = "", ...){
    x <- as.data.frame(x)
  obj <- asciiDataFrame$new(x, include.rownames, include.colnames, format, digits, decimal.mark, na.print, caption, caption.level, width, frame, grid, valign, header, footer, align, col.width, style, cgroup, n.cgroup, calign, cvalign, cstyle, rgroup, n.rgroup, ralign, rvalign, rstyle)
	class(obj) <- c("ascii", "proto", "environment")
	return(obj)
  }
