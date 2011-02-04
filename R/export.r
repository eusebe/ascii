##' Create a section
##'
##' \code{section} can be used with \code{export} function to add\dots a section
##' @param caption a string
##' @param caption.level caption level
##' @return A section object.
##' @export
##' @author David Hajage
section <- function(caption, caption.level = 1) {
  results <- list(caption = caption, caption.level = caption.level + 1)
  class(results) <- "section"
  results
}

##' Print a section object
##'
##' Print a section object
##' @param x a section object
##' @param ... not used
##' @export
##' @author David Hajage
print.section <- function(x, ...) {
  caption <- x$caption
  caption.level <- x$caption.level
  results <- header.asciidoc(caption, caption.level)
  cat("\n", results, sep = "")
}

##' Create a paragraph
##'
##' \code{paragraph} can be used with \code{export} function to add\dots a paragraph
##' @param ... strings composing the paragraph
##' @param new whether to create a new paragraph or to continue a preceding one
##' @return A paragraph object.
##' @export
##' @author David Hajage
paragraph <- function(..., new = TRUE) {
  results <- list(text = list(...), new = new)
  class(results) <- "paragraph"
  results
}

##' Print a paragraph object
##'
##' Print a paragraph object
##' @param x a paragraph object
##' @param ... not used
##' @export
##' @author David Hajage
print.paragraph <- function(x, ...) {
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

##' Insert an inline R result
##'
##' \code{sexpr} can be used with \code{export} function to insert an inline R results
##' @param x an R results (of length one)
##' @return A sexpr object.
##' @export
##' @author David Hajage
sexpr <- function(x) {
  results <- x
  class(results) <- "sexpr"
  results
}

##' Print a sexpr object
##'
##' Print a sexpr object
##' @param x a sexpr object
##' @param ... not used
##' @export
##' @author David Hajage
print.sexpr <- function(x, ...) {
  cat(x, sep = "")
}

##' Export R objects
##'
##' \code{out} can be used with \code{export} function to insert an R results
##' @param x an R object
##' @param results if \code{'verbatim'}, the output is included in a verbatim environment. If \code{'latex'}, the output is taken to be already proper latex markup and included as is.
##' @return An out object
##' @export
##' @author David Hajage
out <- function(x, results = "verbatim") {
  results <- list(x, results)
  class(results) <- "out"
  results
}

##' Print an out object
##'
##' Print an out object
##' @param x an out object
##' @param ... not used
##' @export
##' @author David Hajage
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

##' Insert an equation
##'
##' \code{math} can be used with \code{export} function to insert some maths
##' @param x some maths
##' @param notation latexmath or asciimath
##' @export
##' @author David Hajage
math <- function(x, notation = "latexmath") {
  results <- list(x, notation)
  class(results) <- "math"
  results
}

##' Print a math object
##'
##' Print a math object
##' @param x a math object
##' @param ... not used
##' @export
##' @author David Hajage
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

##' convert a file to several output using asciidoc
##'
##' @param input input file
##' @param destination output file (no extension)
##' @param format format of the output (chunked, epub, htmlhelp, pdf, text, xhtml, slidy, odt, dvi, ps, tex, docbook, asciidoc)
##' @param encoding encoding format of input file
##' @param latexmath use latexmath attribute
##' @param asciimath use asciimath attribute
##' @param cmd eventually define the asciidoc command
##' @param cygwin do you use asciidoc through cygwin
##' @export
##' @author David Hajage
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
    if (format == "slidy") {
      htmldest <- paste(paste(destination, sub("^(.+)(.txt)$", "\\1", basename(input)), sep = "/"), "html", sep = ".")
      cmd <- paste(asciidoc, " -b slidy", amath, lmath, " -a encoding=", encoding, " -o ", htmldest, sep = "")      
    } else if (format != "xhtml" & format != "odt") {
      cmd <- paste(a2x, amath, lmath, " -a encoding=", encoding, " -D ", destination, " -f ", format, sep = "")
    } else {
      htmldest <- paste(paste(destination, sub("^(.+)(.txt)$", "\\1", basename(input)), sep = "/"), "html", sep = ".")
      cmd <- paste(asciidoc, amath, lmath, " -a encoding=", encoding, " -o ", htmldest, sep = "")
      if (format == "odt") {
        odtdest <- paste(paste(destination, sub("^(.+)(.txt)$", "\\1", basename(input)), sep = "/"), "odt", sep = ".")        
        odtcmd <- paste("xhtml2odt -i", htmldest, "-o", odtdest)
      }
    }
  }
  finalcmd <- paste(opencmd, cmd, input, closecmd)
  if (format == "odt") {
    finalcmd <- paste(finalcmd, odtcmd, sep = " && ")
  }
  err <- system(finalcmd, wait = TRUE)
  invisible(err)
}

##' Use asciidoc from R
##' These functions allow to use asciidoc and a2x toolchains directly from R.
##' 
##' @param ... \code{section}, \code{paragraph}, \code{sexpr}, \code{out}, \code{math}, \code{ascii}, or what-you-want (exported as verbatim) objects
##' @param list list of objects
##' @param file output file (no extension)
##' @param format format of the output (chunked, epub, htmlhelp, pdf, text, xhtml, slidy, odt, dvi, ps, tex, docbook, asciidoc)
##' @param open open or not resulting file
##' @param main main title of the document
##' @param author author of the document
##' @param email email of the document
##' @param revdate revision date of the document
##' @param revnumber revision number of the document
##' @param encoding encoding format of input file
##' @param cmd eventually define the asciidoc command
##' @param cygwin do you use asciidoc through cygwin
##' @keywords print
##'
##' @examples
##' \dontrun{
##'   export(ascii(head(esoph)))}
##' 
##' @export
##' @return a list with all \code{...} arguments.
##' @author David Hajage \email{dhajage@@gmail.com}
export <- function(..., list = NULL, file = NULL, format = "xhtml", open = NULL, main = NULL, author = NULL, email = NULL, revdate = NULL, revnumber = NULL, encoding = NULL, cmd = NULL, cygwin = FALSE) {

  windows <- grepl("mingw", version$os)
  
  format <- format[1]
  available <- c("chunked", "epub", "htmlhelp", "pdf", "text", "xhtml", "slidy", "odt", "dvi", "ps", "tex", "docbook", "asciidoc")
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

  if (is.null(list))
    args <- list(...)
  else
    args <- list
  
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
      } else if ("out" %in% class(arg) | "section" %in% class(arg)) {
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
  if (format == "xhtml" | format == "slidy") {
    htmlfile <- paste(basename(file), "html", sep = ".")
    finalfile <- paste(wd, htmlfile, sep = "/")
  }
  if (format == "odt") {
    odtfile <- paste(basename(file), "odt", sep = ".")
    finalfile <- paste(wd, odtfile, sep = "/")
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
