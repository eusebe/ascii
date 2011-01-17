##' ascii method for class smooth.spline
##'
##' @param x An R object of class found among \code{methods(ascii)}.
##' @param caption Character vector of length 1 containing the table's caption
##'   or title.  Set to \code{""} to suppress the caption.  Default value is
##'   \code{NULL}.
##' @param caption.level Character or numeric vector of length 1 containing the
##'   caption's level.  Can take the following values: \code{0} to \code{5},
##'   \code{"."} (block titles in asciidoc markup), \code{"s"} (strong),
##'   \code{"e"} (emphasis), \code{"m"} (monospaced) or \code{""} (no markup).
##'   Default is NULL.
##' @param list.type Character vector of length one indicating the list type
##'   (\code{"bullet"}, \code{"number"}, \code{"label"} or \code{"none"}). If
##'   \code{"label"}, \code{names(list)} is used for labels. Default is
##'   \code{"bullet"}.
##' @param ... Additional arguments.  (Currently ignored.)
##' @return An ascii object.
##' @S3method ascii smooth.spline
##' @author David Hajage
ascii.smooth.spline <- function(x, caption = NULL, caption.level = NULL, list.type = "bullet", ...) {
    x <- as.list(capture.output(x)[-1:-3])
    obj <- asciiList$new(x = x, caption = caption, caption.level = caption.level, list.type = list.type)
    class(obj) <- c("ascii", "proto", "environment")
    return(obj)
}

