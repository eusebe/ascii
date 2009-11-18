options(asciiType = "asciidoc")

print.ascii <- function(x, type = getOption("asciiType"), file = NULL, append = FALSE, escape = FALSE, ...) {
  if (type == "asciidoc") res <- capture.output(x$show.asciidoc())
  if (type == "rest") res <- capture.output(x$show.rest())
  if (type == "org") res <- capture.output(x$show.org())
  if (type == "t2t") res <- capture.output(x$show.t2t())

  if (escape) {
    res <- gsub("\\_", "\\\\_", res)
    res <- gsub("\\^", "\\\\^", res)
  }
  
  if (is.null(file)) {
    cat(res, sep = "\n")
  }
  else {
    if (append) op <- "a" else op <- "w"
    f <- file(file, op)
    writeLines(res, f)
    close(f)
  }
  invisible(x)
}

