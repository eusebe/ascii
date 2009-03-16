ascii.lm <- function (x, include.rownames = TRUE, include.colnames = TRUE, format = "f", digits = 2, decimal.mark = ".", na.print = "", caption = "", width = 0, frame = "", grid = "", valign = "", header = TRUE, footer = FALSE, align = "", col.width = 1, style = ""){
    x <- as.data.frame(summary(x)$coef)
    obj <- asciiDataFrame$new(x = x, include.rownames = include.rownames,
         include.colnames = include.colnames, format = format,
         digits = digits, decimal.mark = decimal.mark, na.print = na.print,
         caption = caption, width = width, frame = frame, grid = grid,
         valign = valign, header = header, footer = footer, align = align,
         col.width = col.width, style = style)
    class(obj) <- c("ascii", "proto", "environment")
    return(obj)
}

ascii.summary.lm <- function (x, include.rownames = TRUE, include.colnames = TRUE, format = "f", digits = 2, decimal.mark = ".", na.print = "", caption = "", width = 0, frame = "", grid = "", valign = "", header = TRUE, footer = FALSE, align = "", col.width = 1, style = ""){
    x <- as.data.frame(x$coef)
    obj <- asciiDataFrame(x = x, include.rownames = include.rownames,
         include.colnames = include.colnames, format = format,
         digits = digits, decimal.mark = decimal.mark, na.print = na.print,
         caption = caption, width = width, frame = frame, grid = grid,
         valign = valign, header = header, footer = footer, align = align,
         col.width = col.width, style = style)
    class(obj) <- c("ascii", "proto", "environment")
    return(obj)
}
