##' ascii method for class list
##'
##' @param x 
##' @param caption 
##' @param caption.level 
##' @param list.type 
##' @param ... 
##' @return An ascii object.
##' @author David Hajage
ascii.list <- function(x, caption = NULL, caption.level = NULL, list.type = "bullet", ...) {
    obj <- asciiList$new(x = x, caption = caption, caption.level = caption.level, list.type = list.type)
    class(obj) <- c("ascii", "proto", "environment")
    return(obj)
}

##' ascii method for class simple.list
##'
##' <details>
##' @title 
##' @param x 
##' @param caption 
##' @param caption.level 
##' @param list.type 
##' @param ... 
##' @return An ascii object.
##' @author David Hajage
ascii.simple.list <- function(x, caption = NULL, caption.level = NULL, list.type = "label", ...) {
    x <- unlist(x)
    obj <- asciiList$new(x = x, caption = caption, caption.level = caption.level, list.type = list.type)
    class(obj) <- c("ascii", "proto", "environment")
    return(obj)
}

##' ascii method for class packageDescription
##'
##' @param x 
##' @param caption 
##' @param caption.level 
##' @param list.type 
##' @param ... 
##' @return An ascii object.
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
##' @param x 
##' @param locale 
##' @param ... 
##' @return An ascii object.
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
