##' ascii method for class prcomp
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
ascii.prcomp <- function (x, include.rownames = TRUE, include.colnames = TRUE, rownames = NULL, colnames = NULL, format = "f", digits = 2, decimal.mark = ".", na.print = "", caption = NULL, caption.level = NULL, width = 0, frame = NULL, grid = NULL, valign = NULL, header = TRUE, footer = FALSE, align = NULL, col.width = 1, style = NULL, tgroup = NULL, n.tgroup = NULL, talign = "c", tvalign = "middle", tstyle = "h", bgroup = NULL, n.bgroup = NULL, balign = "c", bvalign = "middle", bstyle = "h", lgroup = NULL, n.lgroup = NULL, lalign = "c", lvalign = "middle", lstyle = "h", rgroup = NULL, n.rgroup = NULL, ralign = "c", rvalign = "middle", rstyle = "h", ...){
    y <- data.frame(x$rotation,check.names=FALSE)
    obj <- asciiTable$new(y, include.rownames,
         include.colnames, rownames, colnames, format,
         digits, decimal.mark, na.print,
         caption, caption.level = caption.level, width, frame, grid,
         valign, header, footer, align,
         col.width, style,
         tgroup = tgroup, n.tgroup = n.tgroup, talign = talign,
         tvalign = tvalign, tstyle = tstyle,
         bgroup = bgroup, n.bgroup = n.bgroup, balign = balign,
         bvalign = bvalign, bstyle = bstyle,
         lgroup = lgroup, n.lgroup = n.lgroup, lalign = lalign,
         lvalign = lvalign, lstyle = lstyle,
         rgroup = rgroup, n.rgroup = n.rgroup, ralign = ralign,
         rvalign = rvalign, rstyle = rstyle)
    class(obj) <- c("ascii", "proto", "environment")
    return(obj)
}

##' ascii method for class summary.prcomp
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
ascii.summary.prcomp <- function (x, include.rownames = TRUE, include.colnames = TRUE, rownames = NULL, colnames = NULL, format = "f", digits = 2, decimal.mark = ".", na.print = "", caption = NULL, caption.level = NULL, width = 0, frame = NULL, grid = NULL, valign = NULL, header = TRUE, footer = FALSE, align = NULL, col.width = 1, style = NULL, tgroup = NULL, n.tgroup = NULL, talign = "c", tvalign = "middle", tstyle = "h", bgroup = NULL, n.bgroup = NULL, balign = "c", bvalign = "middle", bstyle = "h", lgroup = NULL, n.lgroup = NULL, lalign = "c", lvalign = "middle", lstyle = "h", rgroup = NULL, n.rgroup = NULL, ralign = "c", rvalign = "middle", rstyle = "h", ...){
    x <- data.frame(x$importance,check.names=FALSE)
    obj <- asciiTable$new(x, include.rownames,
         include.colnames, rownames, colnames, format,
         digits, decimal.mark, na.print,
         caption, caption.level, width, frame, grid,
         valign, header, footer, align,
         col.width, style,
         tgroup = tgroup, n.tgroup = n.tgroup, talign = talign,
         tvalign = tvalign, tstyle = tstyle,
         bgroup = bgroup, n.bgroup = n.bgroup, balign = balign,
         bvalign = bvalign, bstyle = bstyle,
         lgroup = lgroup, n.lgroup = n.lgroup, lalign = lalign,
         lvalign = lvalign, lstyle = lstyle,
         rgroup = rgroup, n.rgroup = n.rgroup, ralign = ralign,
         rvalign = rvalign, rstyle = rstyle)
    class(obj) <- c("ascii", "proto", "environment")
    return(obj)
}
