options(asciiType = "asciidoc")

print.ascii <- function(x, type = getOption("asciiType"), ...) {
 if (type == "asciidoc") x$show.asciidoc()
 if (type == "t2t") x$show.t2t()
 if (type == "textile") x$show.textile()
}
