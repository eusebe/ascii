##' ascii method for class list
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
##' @export
##' @method ascii list
##' @author David Hajage
ascii.list <- function(x, caption = NULL, caption.level = NULL, list.type = "bullet", ...) {
    obj <- asciiList$new(x = x, caption = caption, caption.level = caption.level, list.type = list.type)
    class(obj) <- c("ascii", "proto", "environment")
    return(obj)
}

##' ascii method for class simple.list
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
##' @export
##' @method ascii simple.list
##' @author David Hajage
ascii.simple.list <- function(x, caption = NULL, caption.level = NULL, list.type = "label", ...) {
    x <- unlist(x)
    obj <- asciiList$new(x = x, caption = caption, caption.level = caption.level, list.type = list.type)
    class(obj) <- c("ascii", "proto", "environment")
    return(obj)
}

##' ascii method for class packageDescription
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
##' @export
##' @method ascii packageDescription
##' @author David Hajage
ascii.packageDescription <- function(x, caption = NULL, caption.level = NULL, list.type = "label", ...) {
  x <- unclass(x)
  x <- lapply(x, function(x) gsub("\n", " ", x))
  obj <- asciiList$new(x = x, caption = caption, caption.level = caption.level, list.type = list.type)
  class(obj) <- c("ascii", "proto", "environment")
  return(obj)  
}

##' ascii metho for class sessionInfo
##'
##' @param x An R object of class found among \code{methods(ascii)}.
##' @param locale default is TRUE to show locale information
##'   (\code{sessionInfo()}).
##' @param ... Additional arguments.  (Currently ignored.)
##' @return An ascii object.
##' @export
##' @method ascii sessionInfo
##' @author David Hajage
ascii.sessionInfo <- function (x, locale = TRUE, ...) {
  mkLabel <- function(L, n) {
    vers <- sapply(L[[n]], function(x) x[["Version"]])
    pkg <- sapply(L[[n]], function(x) x[["Package"]])
    paste(pkg, vers, sep = "_")
  }
  res <- NULL
  res$"R version" <- paste(x$R.version$version.string, x$R.version$platform, sep = ", ")
  if (locale)
    res$locale <- paste(strsplit(x$locale, ";", fixed = TRUE)[[1]], collapse = ", ")
  res$"attached base packages" <- paste(x$basePkgs, collapse = ", ")
  if (!is.null(x$otherPkgs))
    res$"other attached packages" <- paste(mkLabel(x, "otherPkgs"), collapse = ", ")
  if (!is.null(x$loadedOnly))
  res$"loaded via a namespace (and not attached)" <- paste(mkLabel(x, "loadedOnly"), collapse = ", ")

  res <- asciiList$new(x = res, caption = NULL, caption.level = NULL, list.type = "label")
  class(res) <- c("ascii", "proto", "environment")
  return(res)
}
