ascii.list <- function(x, caption = "", ...) {
    obj <- asciiList$new(x = x, caption = caption)
    class(obj) <- c("ascii", "proto", "environment")
    return(obj)
}

