#~ setMethod(
#~   "ascii",
#~   "summary.table",
#~   function (x, include.rownames = TRUE, include.colnames = TRUE, format = "f", digits = 2, decimal.mark = ".", na.print = "", caption = "", width = 0, frame = "", grid = "", valign = "", header = TRUE, footer = FALSE, align = "", col.width = 1, style = ""){
#~     x <- as.list(capture.output(x))
#~     new("R2asciidocList", x = x)
#~   }
#~ )
ascii.summary.table <- function(x, caption = "") {
    x <- as.list(capture.output(x))
    obj <- asciiList$new(x = x, caption = caption)
    class(obj) <- c("ascii", "proto", "environment")
    return(obj)
}
