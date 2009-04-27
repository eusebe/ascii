ascii.tab1 <- function (x, include.rownames = TRUE, include.colnames = TRUE, format = "f", digits = 2, decimal.mark = ".", na.print = "", caption = "", caption.level = "", width = 0, frame = "", grid = "", valign = "", header = TRUE, footer = TRUE, align = "", col.width = 1, style = "", ...){
    y <- x$output
    row.names(y)[is.na(row.names(y))] <- "NA"
    colnames(y) <- sub("^ *", "", colnames(y))
    rownames(y) <- sub("^ *", "", rownames(y))
    rownames(y) <- sub("^NA$", "na", rownames(y))

    y <- as.data.frame(y)
    obj <- asciiDataFrame$new(y, include.rownames,
         include.colnames, format,
         digits, decimal.mark, na.print,
         caption, caption.level, width, frame, grid,
         valign, header, footer, align,
         col.width, style)
    class(obj) <- c("ascii", "proto", "environment")
    return(obj)
}


