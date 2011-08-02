##' @export
##' @method ascii mtable
ascii.mtable <- function(x, lgroup = c(dimnames(x$coefficients)[[3]], rownames(x$summaries)), n.lgroup = c(rep(2, dim(x$coefficients)[x$as.row[2]]), rep(1, nrow(x$summaries))), include.rownames = FALSE, include.colnames = TRUE, ...) {
  coefs <- ftable(as.table(x$coefficients), row.vars = rev(x$as.row), col.vars = rev(x$as.col))
  coefs <- rbind(coefs, x$summaries)

  ascii.default(coefs, lgroup = lgroup, n.lgroup = n.lgroup, include.rownames = include.rownames, include.colnames = include.colnames, ...)
}
