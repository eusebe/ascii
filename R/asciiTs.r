# from xtable package
ascii.ts <- function (x, include.rownames = TRUE, include.colnames = TRUE, format = "f", digits = 0, decimal.mark = ".", na.print = "", caption = "", width = 0, frame = "", grid = "", valign = "", header = TRUE, footer = FALSE, align = "", col.width = 1, style = "", ...){

  if (inherits(x, "ts") && !is.null(ncol(x))) {
    tp.1 <- trunc(time(x))
    tp.2 <- trunc(cycle(x))
    day.abb <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
    ROWNAMES <- switch(frequency(x),
        tp.1,
        "Arg2", "Arg3",              ## Dummy arguments
        paste(tp.1, c("Q1", "Q2", "Q3", "Q4")[tp.2], sep=" "),
        "Arg5", "Arg6",
        paste("Wk.", tp.1, " ", day.abb[tp.2], sep=""),
        "Arg8", "Arg9", "Arg10", "Arg11",
        paste(tp.1, month.abb[tp.2], sep=" "))
    tmp <- data.frame(x, row.names=ROWNAMES);
  }
  else if (inherits(x, "ts") && is.null(ncol(x))) {
    COLNAMES <- switch(frequency(x),
        "Value",
        "Arg2", "Arg3",              ## Dummy arguments
        c("Q1", "Q2", "Q3", "Q4"),
        "Arg5", "Arg6",
        day.abb,
        "Arg8", "Arg9", "Arg10", "Arg11",
        month.abb)
    ROWNAMES <- seq(from=start(x)[1], to=end(x)[1])
    tmp <- data.frame(matrix(c(rep(NA, start(x)[2] - 1), x,
              rep(NA, frequency(x) - end(x)[2])),
            ncol=frequency(x), byrow=TRUE), row.names=ROWNAMES)
    names(tmp) <- COLNAMES
  }
  obj <- asciiDataFrame$new(x = as.data.frame(tmp), include.rownames = include.rownames,
      include.colnames = include.colnames, format = format,
      digits = digits, decimal.mark = decimal.mark, na.print = na.print,
      caption = caption, width = width, frame = frame, grid = grid,
      valign = valign, header = header, footer = footer, align = align,
      col.width = col.width, style = style)
  class(obj) <- c("ascii", "proto", "environment")
  return(obj)
}

ascii.zoo <- function(x,...) {
    return(ascii(as.ts(x),...))
}
