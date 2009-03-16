ascii.matrix <- function (x, include.rownames = FALSE, include.colnames = FALSE, format = "f", digits = 2, decimal.mark = ".", na.print = "", caption = "", width = 0, frame = "", grid = "", valign = "", header = FALSE, footer = FALSE, align = "", col.width = 1, style = "", ...){
    x <- as.data.frame(x)
    obj <- asciiDataFrame$new(x, include.rownames,
         include.colnames, format,
         digits, decimal.mark, na.print,
         caption, width, frame, grid,
         valign, header, footer, align,
         col.width, style)
    class(obj) <- c("ascii", "proto", "environment")
    return(obj)
}

