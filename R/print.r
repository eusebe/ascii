options(asciiType = "asciidoc")

print.ascii <- function(x, type = getOption("asciiType"), file = NULL, append = FALSE, ...) {
  if (is.null(file)) {
    if (type == "asciidoc") x$show.asciidoc()
    if (type == "sphinx") x$show.sphinx()
    if (type == "org") x$show.org()
    if (type == "t2t") x$show.t2t()
  }
  else {
    if (type == "asciidoc") res <- capture.output(x$show.asciidoc())
    if (type == "sphinx") res <- capture.output(x$show.sphinx())
    if (type == "org") res <- capture.output(x$show.org())
    if (type == "t2t") res <- capture.output(x$show.t2t())
    if (append) op <- "a" else op <- "w"
    f <- file(file, op)
    writeLines(res, f)
    close(f)
  }
  invisible(x)
}

