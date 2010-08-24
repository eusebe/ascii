section <- function(caption, caption.level = 2) {
  results <- list(caption = caption, caption.level = caption.level)
  class(results) <- "section"
  results
}

print.section <- function(x, type = "asciidoc", ...) {
  caption <- x$caption
  caption.level <- x$caption.level
  
  results <- NULL
  if (!is.null(caption)) {
    if (is.null(caption.level)) {
      results <- paste(".", caption, "\n", results, sep = "")
    } else if (caption.level == ".") {
      results <- paste(".", caption, "\n", results, sep = "")
    } else if (is.numeric(caption.level) & caption.level > 0) {
      lev <- paste(rep("=", caption.level), collapse = "")
      results <- paste(lev, " ", caption, " ", "\n\n", results, sep = "")
    } else if (caption.level == "s") {
      results <- paste(beauty.asciidoc(caption, "s"), "\n\n", results, sep = "")
    }
    else if (caption.level == "e") {
      results <- paste(beauty.asciidoc(caption, "e"), "\n\n", results, sep = "")
    }
    else if (caption.level == "m") {
      results <- paste(beauty.asciidoc(caption, "m"), "\n\n", results, sep = "")
    }
    else if (caption.level == "none" | caption.level == "") {
      results <- paste(caption, "\n\n", results, sep = "")
    }
  }
  cat(results, sep = "")
}

paragraph <- function(text, new = TRUE) {
  results <- list(text = text, new = new)
  class(results) <- "paragraph"
  results
}

print.paragraph <- function(x, type = "asciidoc", ...) {
  newline <- "\n"
  text <- x$text
  new <- x$new
  
  if (new)
    cat(newline)
  cat(text, "\n")
}

sexpr <- function(x) {
  results <- x
  class(results) <- "sexpr"
  results
}

print.sexpr <- function(x, ...) {
  text <- paste("\\Sexpr{", x, "}", sep = "")
  cat(text)
}

## footnote <- function(text) {

##   class(results) <- "footnote"
##   results  
## }

convert <- function(input, destination = NULL, format = "xhtml", encoding = NULL, cmd = NULL) {

  windows <- grepl("mingw", version$os)
  
  opencmd <- ""
  closecmd <- ""
  # for cygwin users...
  if (windows) {
    opencmd <- "bash -c \""
    closecmd <- "\""
  }
  
  if (is.null(cmd)) {
    if (is.null(encoding)) {
      encoding <- "UTF-8"
      if (windows) {
        encoding <- "ISO-8859-1"
      }
    }
    if (is.null(destination)) {
      destination <- "."
    }
    if (format != "xhtml") {
      cmd <- paste("a2x -a encoding=", encoding, " -D ", destination, " -f ", format, sep = "")
    } else {
      destfile <- paste(paste(destination, sub("^(.+)(.txt)$", "\\1", basename(input)), sep = "/"), "html", sep = ".")
      cmd <- paste("asciidoc -a encoding=", encoding, " -o ", destfile, sep = "")
    }
    
  }
  cmd <- paste(cmd, input, closecmd)
  system(cmd, wait = TRUE)
}

export <- function(..., file = NULL, destination = NULL, format = "xhtml", open = TRUE, main = NULL, author = NULL, email = NULL, revdate = NULL, revnumber = NULL, cmd = NULL, encoding = NULL) {
  if (is.null(file)) {
    file <- tempfile("asciidoc-report")
  }
  if (is.null(main)) {
    main <- file
  }

  if (is.null(destination)) {
    wd <- dirname(file)
  } else {
    wd <- destination
  }
  
  args <- list(...)
  lines <- capture.output({
                          print(section(main, 1))
                          if (!is.null(author)) {
                            cat(":author:", author, "\n")
                          }
                          if (!is.null(email)) {
                            cat(":email:", email, "\n")
                          }
                          if (!is.null(revdate)) {
                            cat(":revate:", revdate, "\n")
                          }
                          if (!is.null(revnumber)) {
                            cat(":revnumber:", revnumber, "\n")
                          }
                          for (i in seq_along(args)) {
                            arg <- args[[i]]
                            if ("ascii" %in% class(arg)) {
                              arg$show.asciidoc()
                              cat("\n")
                            } else {
                              print(arg)
                            }
                          }})
  textfile <- paste(file, "txt", sep = ".")
  f <- file(textfile, "w")
  writeLines(lines, f)
  close(f)

  if (format == "xhtml") {
    htmlfile <- paste(basename(file), "html", sep = ".")
    convert(textfile, destination = wd, format = "xhtml", cmd = cmd, encoding = encoding)
    if (open) {
      browseURL(paste(wd, htmlfile, sep = "/"))
    }
  }
}

file = NULL; format = "html"; open = TRUE; main = NULL; author = NULL; email = NULL; revdate = NULL; revnumber = NULL; cmd = NULL; encoding = NULL
