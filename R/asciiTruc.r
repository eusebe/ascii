truc <- function(x, include.rownames = FALSE, include.colnames = FALSE, format = "f", digits = 2, decimal.mark = ".", na.print = "", caption = "", width = 0, frame = "", grid = "", valign = "", header = FALSE, footer = FALSE, align = "", col.width = 1, style = "", ...) {

  x <- ascii(x$expected, include.rownames, include.colnames, format, digits, decimal.mark, na.print, caption, width, frame, grid, valign, header, footer, align, col.width, style)
  y <- asciiList$new(list("Chi2"), caption = "")
  obj <- asciiDataFrameList$new(x, y)
	class(obj) <- c("ascii", "proto", "environment")
	return(obj)
}

