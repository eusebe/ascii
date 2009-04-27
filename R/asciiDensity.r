ascii.density <- function (x, include.rownames = FALSE, include.colnames = TRUE, format = "f", digits = 2, decimal.mark = ".", na.print = "", caption = "", caption.level = ".", width = 0, frame = "", grid = "", valign = "", header = TRUE, footer = FALSE, align = "", col.width = 1, style = "", ...){
    y <- unclass(x)
    res <- summary(data.frame(x = y$x, y = y$y))
    obj <- ascii(x = res, include.rownames = include.rownames,
         include.colnames = include.colnames, format = format,
         digits = digits, decimal.mark = decimal.mark, na.print = na.print,
         caption = caption, caption.level = caption.level, width = width, frame = frame, grid = grid,
         valign = valign, header = header, footer = footer, align = align,
         col.width = col.width, style = style)
    class(obj) <- c("ascii", "proto", "environment")
    return(obj)
}
