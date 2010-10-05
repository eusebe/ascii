##' ascii method for class summary.table
##'
##' @param x 
##' @param caption 
##' @param caption.level 
##' @param list.type 
##' @param ... 
##' @return An ascii object.
##' @author David Hajage
ascii.summary.table <- function(x, caption = NULL, caption.level = NULL, list.type = "bullet", ...) {
    x <- as.list(capture.output(x))
    obj <- asciiList$new(x = x, caption = caption, caption.level = caption.level, list.type = list.type)
    class(obj) <- c("ascii", "proto", "environment")
    return(obj)
}
