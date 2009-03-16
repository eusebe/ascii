ascii.table <- function (x, include.rownames = TRUE, include.colnames = TRUE, format = "f", digits = 2, decimal.mark = ".", na.print = "", caption = "", width = 0, frame = "", grid = "", valign = "", header = TRUE, footer = FALSE, align = "", col.width = 1, style = ""){

  if (length(dim(x)) == 1 | is.null(dim(x))) {
    y <- as.data.frame(t(unclass(x)))
    names(y) <- names(x)
  }
  if (length(dim(x)) == 2) {
    y <- unclass(x)
    if (length(unique(rownames(y))) < nrow(y)) rownames(y) <- 1:nrow(y)
    y <- as.data.frame(y)
  }
  if (length(dim(x)) > 2)  y <- as.data.frame(x)

  obj <- ascii(x = y, include.rownames = include.rownames,
      include.colnames = include.colnames, format = format,
      digits = digits, decimal.mark = decimal.mark, na.print = na.print,
      caption = caption, width = width, frame = frame, grid = grid,
      valign = valign, header = header, footer = footer, align = align,
      col.width = col.width, style = style)
  class(obj) <- c("ascii", "proto", "environment")
  return(obj)
}
