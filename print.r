options(asciiType = "asciidoc")

print.Ascii <- function(x, type = getOption("asciiType")) {
 if (type == "asciidoc") x$show.asciidoc()
 if (type == "t2t") x$show.t2t()
}
