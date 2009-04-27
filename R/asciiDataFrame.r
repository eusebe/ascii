ascii.data.frame <- function(x, include.rownames = TRUE, include.colnames = TRUE, format = "f", digits = 2, decimal.mark = ".", na.print = "", caption = "", caption.level = "", width = 0, frame = "", grid = "", valign = "", header = TRUE, footer = FALSE, align = "", col.width = 1, style = "", ...) {
  obj <- asciiDataFrame$new(x, include.rownames, include.colnames, format, digits, decimal.mark, na.print, caption, caption.level, width, frame, grid, valign, header, footer, align, col.width, style)
	class(obj) <- c("ascii", "proto", "environment")
	return(obj)
}

#~ # from package reshape (Hadley Wickham : http://had.co.nz/reshape/)
ascii.cast_df <-function (x, include.rownames = FALSE, include.colnames = TRUE, format = "f", digits = 2, decimal.mark = ".", na.print = "", caption = "", caption.level = "", width = 0, frame = "", grid = "", valign = "", header = TRUE, footer = FALSE, align = "", col.width = 1, style = "", ...){
    x <- as.data.frame(x)
  obj <- asciiDataFrame$new(x, include.rownames, include.colnames, format, digits, decimal.mark, na.print, caption, caption.level, width, frame, grid, valign, header, footer, align, col.width, style)
	class(obj) <- c("ascii", "proto", "environment")
	return(obj)
  }
