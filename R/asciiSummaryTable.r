ascii.summary.table <- function(x, caption = "", list.type = "bullet", ...) {
    x <- as.list(capture.output(x))
    obj <- asciiList$new(x = x, caption = caption, list.type = list.type)
    class(obj) <- c("ascii", "proto", "environment")
    return(obj)
}
