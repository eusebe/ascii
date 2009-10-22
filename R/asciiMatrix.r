ascii.matrix <- function (x, include.rownames = FALSE, include.colnames = FALSE, format = "f", digits = 2, decimal.mark = ".", na.print = "", caption = "", caption.level = "", width = 0, frame = "", grid = "", valign = "", header = FALSE, footer = FALSE, align = "", col.width = 1, style = "", cgroup = NULL, n.cgroup = NULL, calign = "", cvalign = "", cstyle = "", rgroup = NULL, n.rgroup = NULL, ralign = "", rvalign = "", rstyle = "", ...){
    x <- as.data.frame(x)
    obj <- asciiDataFrame$new(x, include.rownames,
         include.colnames, format,
         digits, decimal.mark, na.print,
         caption, caption.level, width, frame, grid,
         valign, header, footer, align,
         col.width, style, cgroup = cgroup, n.cgroup = n.cgroup, calign = calign,
         cvalign = cvalign, cstyle = cstyle, rgroup = rgroup, n.rgroup = n.rgroup, ralign = ralign,
         rvalign = rvalign, rstyle = rstyle)
    class(obj) <- c("ascii", "proto", "environment")
    return(obj)
}

