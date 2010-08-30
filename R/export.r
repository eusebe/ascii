section <- function(caption, caption.level = 2) {
  results <- list(caption = caption, caption.level = caption.level)
  class(results) <- "section"
  results
}

print.section <- function(x, type = "asciidoc", ...) {
  caption <- x$caption
  caption.level <- x$caption.level
  results <- header.asciidoc(caption, caption.level)
  cat("\n", results, sep = "")
}

paragraph <- function(..., new = TRUE) {
  results <- list(text = list(...), new = new)
  class(results) <- "paragraph"
  results
}

print.paragraph <- function(x, type = "asciidoc", ...) {
  newline <- "\n"
  text <- x$text
  new <- x$new

  if (new) {
    cat(newline)
  }
  for (i in 1:length(text)) {
    if (class(text[[i]]) == "sexpr" | class(text[[i]]) == "math") {
      print(text[[i]])
    }
    else {
      cat(text[[i]], " ", sep = "")
    }
  }
  cat("\n")
}

sexpr <- function(x) {
  results <- x
  class(results) <- "sexpr"
  results
}

print.sexpr <- function(x, ...) {
  cat(x, " ", sep = "")
}

out <- function(x, results = "verbatim") {
  results <- list(x, results)
  class(results) <- "out"
  results
}

print.out <- function(x, ...) {
  results <- x[[2]]
  cat("\n")
  if (results == "latex") {
    cat("[latex]\n")
  }
  if (results == "verbatim") {
    cat("----\n")
  }
  print(x[[1]], ...)
  if (results == "verbatim") {
    cat("----\n")
  }
}

math <- function(x, notation = "latexmath") {
  results <- list(x, notation)
  class(results) <- "math"
  results
}

print.math <- function(x, ...) {
  notation <- x[[2]]
  if (notation == "latexmath") {
    cat("latexmath:[$", x[[1]], "$]\n", sep = "")
  }
  if (notation == "asciimath") {
    cat("asciimath:[", x[[1]], "]\n", sep = "")
  }  
}

## faire un figure (à partir du package evaluate plot_snapshot)?

convert <- function(input, destination = NULL, format = "xhtml", encoding = NULL, latexmath = FALSE, asciimath = FALSE, cmd = NULL, cygwin = FALSE) {

  windows <- grepl("mingw", version$os)
  
  opencmd <- ""
  closecmd <- ""
  # for cygwin users...
  if (windows & cygwin) {
    opencmd <- "bash -c \""
    closecmd <- "\""
  }
  a2x <- "a2x"
  asciidoc <- "asciidoc"
  if (windows & !cygwin) {
    a2x <- paste(Sys.getenv("COMSPEC"), "/c", "a2x.py")
    asciidoc <- paste(Sys.getenv("COMSPEC"), "/c", "asciidoc.py")
  }

  amath <- lmath <- ""
  if (asciimath) {
    amath <- " -a asciimath "
  }
  if (latexmath) {
    lmath <- " -a latexmath "
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
      cmd <- paste(a2x, amath, lmath, " -a encoding=", encoding, " -D ", destination, " -f ", format, sep = "")
    } else {
      destfile <- paste(paste(destination, sub("^(.+)(.txt)$", "\\1", basename(input)), sep = "/"), "html", sep = ".")
      cmd <- paste(asciidoc, amath, lmath, " -a encoding=", encoding, " -o ", destfile, sep = "")
    }
    
  }
  finalcmd <- paste(opencmd, cmd, input, closecmd)
  err <- system(finalcmd, wait = TRUE)
  invisible(err)
}

export <- function(..., file = NULL, format = "xhtml", open = NULL, main = NULL, author = NULL, email = NULL, revdate = NULL, revnumber = NULL, encoding = NULL, cmd = NULL, cygwin = FALSE) {

  windows <- grepl("mingw", version$os)
  
  format <- format[1]
  available <- c("chunked", "epub", "htmlhelp", "pdf", "text", "xhtml", "dvi", "ps", "tex", "docbook", "asciidoc")
  if (!(format %in% available)) {
    stop(paste("Please choose an available format:", paste(available, collapse = ", ")))
  }

  asciimath <- FALSE
  latexmath <- FALSE
  
  if (is.null(file)) {
    file <- tempfile("R-report")
    if (is.null(open)) {
      open <- TRUE
    }
  } else {
    if (is.null(open)) {
      open <- FALSE
    }
  }

  if (windows & cygwin) {
    cygfile <- gsub("\\\\", "/", file)
    if (grepl("^[A-Z]:", cygfile)) {
      cygfile <- sub("^[A-Z]", tolower(substr(cygfile, 1, 1)), cygfile)
    }
    cygfile <- sub("^([a-z])(:)", "/cygdrive/\\1", cygfile)
  }
  
  wd <- dirname(file)
  
  if (is.null(main)) {
    main <- paste("+", sub("\\~", "\\\\~", paste(wd, basename(file), sep = "/")), "+", sep = "")
  }

  args <- list(...)
  lines <- capture.output({
    cat(paste("= ", main, "\n", sep = ""))
    if (!is.null(author)) {
      cat(":author:", author, "\n")
    }
    if (!is.null(email)) {
      cat(":email:", email, "\n")
    }
    if (!is.null(revdate)) {
      cat(":revdate:", revdate, "\n")
    }
    if (!is.null(revnumber)) {
      cat(":revnumber:", revnumber, "\n")
    }
    cat("\n")
    for (i in seq_along(args)) {
      arg <- args[[i]]
      if (!is.null(names(args))) {
        if (names(args)[i] != "") {
          cat(".", names(args)[i], "\n", sep = "")
        }
      }
      if ("ascii" %in% class(arg)) {
        arg$show.asciidoc()
        cat("\n")
      } else if ("out" %in% class(arg)) {
        print(arg)
      } else if ("paragraph" %in% class(arg)) {
        print(arg)
        parclass <- sapply(arg[[1]], class)
        if ("math" %in% parclass) {
          asciimath <- TRUE
        }
      } else if ("math" %in% class(arg)) {
        print(arg)
        asciimath <- TRUE
      }
      else {
        print(out(arg, "verbatim"))
      }
    }})
  textfile <- paste(file, "txt", sep = ".")
  f <- file(textfile, "w")
  writeLines(lines, f)
  close(f)
  
  if (format == "asciidoc") {
    asciidocfile <- paste(basename(file), "txt", sep = ".")
    finalfile <- paste(wd, asciidocfile, sep = "/")
  }
  if (format == "xhtml") {
    htmlfile <- paste(basename(file), "html", sep = ".")
    finalfile <- paste(wd, htmlfile, sep = "/")
  }
  if (format == "chunked") {
    finalfile <- paste(wd, paste(basename(file), "chunked", sep = "."), "index.html", sep = "/")
  }
  if (format == "htmlhelp") {
    finalfile <- paste(wd, paste(basename(file), "htmlhelp", sep = "."), "index.html", sep = "/")
  }
  if (format == "pdf") {
    pdffile <- paste(basename(file), "pdf", sep = ".")
    finalfile <- paste(wd, pdffile, sep = "/")
  }
  if (format == "text") {
    txtfile <- paste(basename(file), "text", sep = ".")
    finalfile <- paste(wd, txtfile, sep = "/")
  }
  if (format == "dvi") {
    dvifile <- paste(basename(file), "dvi", sep = ".")
    finalfile <- paste(wd, dvifile, sep = "/")
  }
  if (format == "ps") {
    psfile <- paste(basename(file), "ps", sep = ".")
    finalfile <- paste(wd, psfile, sep = "/")
  }
  if (format == "tex") {
    texfile <- paste(basename(file), "tex", sep = ".")
    finalfile <- paste(wd, texfile, sep = "/")
  }
  if (format == "epub") {
    epubfile <- paste(basename(file), "epub", sep = ".")
    finalfile <- paste(wd, epubfile, sep = "/")
  }
  if (format == "docbook") {
    xmlfile <- paste(basename(file), "xml", sep = ".")
    finalfile <- paste(wd, xmlfile, sep = "/")
  }
  
  cat("Writing ", finalfile, "...\n", sep = "")
  if (format != "asciidoc") {
    if (windows & cygwin) {
      err <- convert(paste(cygfile, "txt", sep = "."), destination = dirname(cygfile), format = format, latexmath = latexmath, asciimath = asciimath, cmd = cmd, encoding = encoding, cygwin = cygwin)
    } else {
      err <- convert(textfile, destination = wd, format = format, latexmath = latexmath, asciimath = asciimath, cmd = cmd, encoding = encoding, cygwin = cygwin)
    }
  } else {
    err <- 0
  }
  if (err != 0 ) {
    stop("Error during conversion.")
  }
  cat("Done\n")

  if (open) {
    cat("Trying to open ", finalfile, sep = "")
    if (windows) {
      cat(" with shell.exec...\n")
      if (!grepl(":", finalfile)) {
        finalfile <- paste("\"", sub("(^.+)(/$)", "\\", getwd()), "/", finalfile, "\"", sep = "")
      }
      shell.exec(finalfile)
    } else {
      cat(" with xdg-open...\n")
      system(paste(shQuote("/usr/bin/xdg-open"), shQuote(finalfile)), wait = FALSE, ignore.stderr = TRUE)
    }
  }
  invisible(args)
}
