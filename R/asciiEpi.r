##' ascii method for class stat.table
##'
##' \code{Epi} package
##' @param x An R object of class \code{stat.table}.
##' @param tgroup Character vector or a list of character vectors defining
##'   major top column headings.  The default is to have none (\code{NULL}).
##' @param lgroup Character vector or list of character vectors defining major
##'   left row headings.  The default is to have none (\code{NULL}).
##' @param ... Additional arguments.  (see \code{?ascii.default}.)
##' @return An ascii object.
##' @export
##' @method ascii stat.table
##' @author David Hajage
ascii.stat.table <- function (x, tgroup = NULL, lgroup = NULL, ...){
  if (length(dim(x)) == 2) {
    if (is.null(lgroup)) {
      lgroup <- names(dimnames(t(x)))[1]
    } 
    results <- ascii.default(t(x), lgroup = lgroup, ...)
  }

  if (length(dim(x)) == 3) {
    xx <- list()
    for (i in 1:dim(x)[2]) {
      xx[[i]] <- x[, i, ]
    }
    xx <- do.call(rbind, xx)
    dn <- dimnames(x)

    if (is.null(lgroup)) {
      lgroup <- list(dn[[2]], names(dn)[2])
      tgroup <- names(dn)[3]
    } 
    results <- ascii.default(xx, lgroup = lgroup, tgroup = tgroup, ...)
  }
  results
}
