ascii.list <- function(x, caption = "", list.type = "bullet", ...) {
    obj <- asciiList$new(x = x, caption = caption, list.type = list.type)
    class(obj) <- c("ascii", "proto", "environment")
    return(obj)
}

