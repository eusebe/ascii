print.Ascii <- function(x, type = "asciidoc") {
 if (type == "asciidoc") x$show.asciidoc()
 if (type == "t2t") x$show.t2t()
}
