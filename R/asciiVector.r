ascii.integer <- function (x, include.rownames = FALSE, include.colnames = FALSE, format = "f", digits = 2, decimal.mark = ".", na.print = "", caption = "", caption.level = "", width = 0, frame = "", grid = "", valign = "", header = FALSE, footer = FALSE, align = "", col.width = 1, style = "", cgroup = NULL, n.cgroup = NULL, calign = "", cvalign = "", cstyle = "", rgroup = NULL, n.rgroup = NULL, ralign = "", rvalign = "", rstyle = "", ...){
    x <- as.data.frame(t(x))
    obj <- asciiDataFrame$new(x = x, include.rownames = include.rownames,
         include.colnames = include.colnames, format = format,
         digits = digits, decimal.mark = decimal.mark, na.print = na.print,
         caption = caption, caption.level = caption.level, width = width, frame = frame, grid = grid,
         valign = valign, header = header, footer = footer, align = align,
         col.width = col.width, style = style, cgroup = cgroup, n.cgroup = n.cgroup, calign = calign,
         cvalign = cvalign, cstyle = cstyle, rgroup = rgroup, n.rgroup = n.rgroup, ralign = ralign,
         rvalign = rvalign, rstyle = rstyle)
    class(obj) <- c("ascii", "proto", "environment")
    return(obj)
}

ascii.numeric <- function (x, include.rownames = FALSE, include.colnames = FALSE, format = "f", digits = 2, decimal.mark = ".", na.print = "", caption = "", caption.level = "", width = 0, frame = "", grid = "", valign = "", header = FALSE, footer = FALSE, align = "", col.width = 1, style = "", cgroup = NULL, n.cgroup = NULL, calign = "", cvalign = "", cstyle = "", rgroup = NULL, n.rgroup = NULL, ralign = "", rvalign = "", rstyle = "", ...){
    x <- as.data.frame(t(x))
    obj <- asciiDataFrame$new(x = x, include.rownames = include.rownames,
         include.colnames = include.colnames, format = format,
         digits = digits, decimal.mark = decimal.mark, na.print = na.print,
         caption = caption, caption.level = caption.level, width = width, frame = frame, grid = grid,
         valign = valign, header = header, footer = footer, align = align,
         col.width = col.width, style = style, cgroup = cgroup, n.cgroup = n.cgroup, calign = calign,
         cvalign = cvalign, cstyle = cstyle, rgroup = rgroup, n.rgroup = n.rgroup, ralign = ralign,
         rvalign = rvalign, rstyle = rstyle)
    class(obj) <- c("ascii", "proto", "environment")
    return(obj)
}

ascii.character <- function (x, include.rownames = FALSE, include.colnames = FALSE, format = "f", digits = 2, decimal.mark = ".", na.print = "", caption = "", caption.level = "", width = 0, frame = "", grid = "", valign = "", header = FALSE, footer = FALSE, align = "", col.width = 1, style = "", cgroup = NULL, n.cgroup = NULL, calign = "", cvalign = "", cstyle = "", rgroup = NULL, n.rgroup = NULL, ralign = "", rvalign = "", rstyle = "", ...){
    x <- as.data.frame(t(x))
    obj <- asciiDataFrame$new(x = x, include.rownames = include.rownames,
         include.colnames = include.colnames, format = format,
         digits = digits, decimal.mark = decimal.mark, na.print = na.print,
         caption = caption, caption.level = caption.level, width = width, frame = frame, grid = grid,
         valign = valign, header = header, footer = footer, align = align,
         col.width = col.width, style = style, cgroup = cgroup, n.cgroup = n.cgroup, calign = calign,
         cvalign = cvalign, cstyle = cstyle, rgroup = rgroup, n.rgroup = n.rgroup, ralign = ralign,
         rvalign = rvalign, rstyle = rstyle)
    class(obj) <- c("ascii", "proto", "environment")
    return(obj)
}

ascii.factor <- function (x, include.rownames = FALSE, include.colnames = FALSE, format = "f", digits = 2, decimal.mark = ".", na.print = "", caption = "", caption.level = "", width = 0, frame = "", grid = "", valign = "", header = FALSE, footer = FALSE, align = "", col.width = 1, style = "", cgroup = NULL, n.cgroup = NULL, calign = "", cvalign = "", cstyle = "", rgroup = NULL, n.rgroup = NULL, ralign = "", rvalign = "", rstyle = "", ...){
    x <- as.data.frame(t(as.character(x)))
    obj <- asciiDataFrame$new(x = x, include.rownames = include.rownames,
         include.colnames = include.colnames, format = format,
         digits = digits, decimal.mark = decimal.mark, na.print = na.print,
         caption = caption, caption.level = caption.level, width = width, frame = frame, grid = grid,
         valign = valign, header = header, footer = footer, align = align,
         col.width = col.width, style = style, cgroup = cgroup, n.cgroup = n.cgroup, calign = calign,
         cvalign = cvalign, cstyle = cstyle, rgroup = rgroup, n.rgroup = n.rgroup, ralign = ralign,
         rvalign = rvalign, rstyle = rstyle)
    class(obj) <- c("ascii", "proto", "environment")
    return(obj)
}
