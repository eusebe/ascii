

##' Export R objects to several markup languages
##' Function converting an R object to an \code{ascii} object, which can then
##' be printed with asciidoc, txt2tags, reStructuredText, org or textile
##' syntax.
##' 
##' The nature of the output generated depends on the class of \code{x}.  For
##' example, \code{summary.table} objects produce a bulleted list while
##' \code{data.frame} objects produce a table of the entire data.frame.
##' 
##' Sometimes, arguments are not active, depending of the features implemented
##' in the markup language generated. All arguments are active when asciidoc
##' syntax is produced.
##' 
##' The available method functions for \code{ascii} are given by
##' \code{methods(ascii)}.  Users can extend the list of available classes by
##' writing methods for the generic function \code{ascii}.  All method
##' functions should return an object of class
##' \code{c("ascii","proto","environment")}.
##' 
##' @aliases ascii package-ascii
##' @param x An R object of class found among \code{methods(ascii)}.
##' @param ... Additional arguments.  (see \code{?ascii.default}).
##' @return This function returns an object of class
##'   \code{c("ascii","proto","environment")}.
##' @author David Hajage \email{dhajage@@gmail.com}
##' @seealso \code{\link{print.ascii}}
##' @keywords print
##' @export
##' @examples
##' data(esoph)
##' ascii(esoph[1:10,])
##' tab <- table(esoph$agegp, esoph$alcgp)
##' ascii(tab)
##' print(ascii(tab), type = "t2t")
##' print(ascii(tab), type = "rest")
##' print(ascii(tab), type = "org")
##' ascii(summary(tab))
##' 
ascii <- function (x, ...) {
  require(proto)
  UseMethod("ascii")
}
