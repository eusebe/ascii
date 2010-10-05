##' ascii method for class table
##'
##' @param x 
##' @param include.rownames 
##' @param include.colnames 
##' @param rownames 
##' @param colnames 
##' @param format 
##' @param digits 
##' @param decimal.mark 
##' @param na.print 
##' @param caption 
##' @param caption.level 
##' @param width 
##' @param frame 
##' @param grid 
##' @param valign 
##' @param header 
##' @param footer 
##' @param align 
##' @param col.width 
##' @param style 
##' @param tgroup 
##' @param n.tgroup 
##' @param talign 
##' @param tvalign 
##' @param tstyle 
##' @param bgroup 
##' @param n.bgroup 
##' @param balign 
##' @param bvalign 
##' @param bstyle 
##' @param lgroup 
##' @param n.lgroup 
##' @param lalign 
##' @param lvalign 
##' @param lstyle 
##' @param rgroup 
##' @param n.rgroup 
##' @param ralign 
##' @param rvalign 
##' @param rstyle 
##' @param ... 
##' @return An ascii object.
##' @author David Hajage
ascii.table <- function (x, include.rownames = TRUE, include.colnames = TRUE, rownames = NULL, colnames = NULL, format = "f", digits = 2, decimal.mark = ".", na.print = "", caption = NULL, caption.level = NULL, width = 0, frame = NULL, grid = NULL, valign = NULL, header = TRUE, footer = FALSE, align = NULL, col.width = 1, style = NULL, tgroup = NULL, n.tgroup = NULL, talign = "c", tvalign = "middle", tstyle = "h", bgroup = NULL, n.bgroup = NULL, balign = "c", bvalign = "middle", bstyle = "h", lgroup = NULL, n.lgroup = NULL, lalign = "c", lvalign = "middle", lstyle = "h", rgroup = NULL, n.rgroup = NULL, ralign = "c", rvalign = "middle", rstyle = "h", ...){

  dnames <- NULL
  
  if (length(dim(x)) == 1 | is.null(dim(x))) {
    y <- t(unclass(x))
  }
  if (length(dim(x)) == 2) {
    dnames <- names(dimnames(x))
    y <- unclass(x)
    if (length(unique(rownames(y))) < nrow(y))
      rownames(y) <- 1:nrow(y)
    y <- as.data.frame(y)
  }
  if (length(dim(x)) > 2)
    y <- as.data.frame(x)

  if (is.null(lgroup) & !is.null(dnames)) {
    if (dnames[1] != "") {
      lgroup <- dnames[1]
      if (is.null(n.lgroup))
        n.lgroup <- c(nrow(y))
    }
  }
  if (is.null(tgroup) & !is.null(dnames)) {
    if (dnames[2] != "") {
      tgroup <- dnames[2]
      if (is.null(n.tgroup))
        n.tgroup <- c(ncol(y))
    }
  }
  obj <- ascii(x = y, include.rownames = include.rownames,
      include.colnames = include.colnames, rownames = rownames, colnames = colnames,
      format = format, digits = digits, decimal.mark = decimal.mark, na.print = na.print,
      caption = caption, caption.level = caption.level, width = width, frame = frame,
      grid = grid, valign = valign, header = header, footer = footer, align = align,
      col.width = col.width, style = style,
      tgroup = tgroup, n.tgroup = n.tgroup, talign = talign,
      tvalign = tvalign, tstyle = tstyle,
      bgroup = bgroup, n.bgroup = n.bgroup, balign = balign,
      bvalign = bvalign, bstyle = bstyle,
      lgroup = lgroup, n.lgroup = n.lgroup, lalign = lalign,
      lvalign = lvalign, lstyle = lstyle,
      rgroup = rgroup, n.rgroup = n.rgroup, ralign = ralign,
      rvalign = rvalign, rstyle = rstyle)
  return(obj)
}
##' ascii method for class ftable
##'
##' @param x 
##' @param digits 
##' @param header 
##' @param ... 
##' @return An ascii object.
##' @author David Hajage
ascii.ftable <- function(x, digits = getOption("digits"), header = TRUE, ...) {
  ascii(format(x, quote = F, digits = digits), header = header)
}
