ascii.list <- function(x, caption = "", caption.level = "", list.type = "bullet", ...) {
    obj <- asciiList$new(x = x, caption = caption, caption.level = caption.level, list.type = list.type)
    class(obj) <- c("ascii", "proto", "environment")
    return(obj)
}

ascii.simple.list <- function(x, caption = "", caption.level = "", list.type = "label", ...) {
    x <- unlist(x)
    obj <- asciiList$new(x = x, caption = caption, caption.level = caption.level, list.type = list.type)
    class(obj) <- c("ascii", "proto", "environment")
    return(obj)
}

ascii.packageDescription <- function(x, caption = "", caption.level = "", list.type = "label", ...) {
  x <- unclass(x)
#  x$File <- attributes(x)$file
  x <- lapply(x, function(x) gsub("\n", " ", x))
  obj <- asciiList$new(x = x, caption = caption, caption.level = caption.level, list.type = list.type)
    class(obj) <- c("ascii", "proto", "environment")
    return(obj)  
}
