ascii.summary.table <- function(x, caption = "", caption.level = ".", list.type = "bullet", ...) {
    x <- as.list(capture.output(x))
    obj <- asciiList$new(x = x, caption = caption, caption.level = caption.level, list.type = list.type)
    class(obj) <- c("ascii", "proto", "environment")
    return(obj)
}
