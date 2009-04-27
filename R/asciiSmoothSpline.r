ascii.smooth.spline <- function(x, caption = "", caption.level = "", list.type = "bullet", ...) {
    x <- as.list(capture.output(x)[-1:-3])
    obj <- asciiList$new(x = x, caption = caption, caption.level = caption.level, list.type = list.type)
    class(obj) <- c("ascii", "proto", "environment")
    return(obj)
}

