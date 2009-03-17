ascii.smooth.spline <- function(x, caption = "", ...) {
    x <- as.list(capture.output(x)[-1:-3])
    obj <- asciiList$new(x = x, caption = caption)
    class(obj) <- c("ascii", "proto", "environment")
    return(obj)
}

