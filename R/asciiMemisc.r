##' ascii method for class mtable
##'
##' \code{memisc} package
##' @param x An R object of class found among \code{methods(ascii)}.
##' @param lgroup Character vector or list of character vectors defining major
##'   left row headings.  The default is to have none (\code{NULL}).
##' @param n.lgroup A numeric vector containing the number of rows for which
##'   each element in lgroup is a heading. Column names count in the row
##'   numbers if \code{include.colnames = TRUE}.
##' @param include.rownames logical. If \code{TRUE} the rows names are printed.
##'   Default value depends of class of \code{x}.
##' @param include.colnames logical. If \code{TRUE} the columns names are
##'   printed. Default value depends of class of \code{x}.
##' @param ... Additional arguments.  (see \code{?ascii.default}).
##' @return An ascii object.
##' @export
##' @method ascii mtable
##' @author David Hajage
ascii.mtable <- function(x, lgroup = c(dimnames(x$coefficients)[[3]], rownames(x$summaries)), n.lgroup = c(rep(2, dim(x$coefficients)[x$as.row[2]]), rep(1, nrow(x$summaries))), include.rownames = FALSE, include.colnames = TRUE, ...) {
  coefs <- ftable(as.table(x$coefficients), row.vars = rev(x$as.row), col.vars = rev(x$as.col))
  coefs <- rbind(coefs, x$summaries)

  ascii.default(coefs, lgroup = lgroup, n.lgroup = n.lgroup, include.rownames = include.rownames, include.colnames = include.colnames, ...)
}
