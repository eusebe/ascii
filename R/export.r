.backends <- c("asciidoc", "a2x", "txt2tags", "pandoc")
.outputs <- list(asciidoc = c("html", "docbook", "slidy"),
                 a2x = c("xhtml", "chunked", "htmlhelp", "epub", "pdf", "text", "dvi", "ps", "tex", "asciidoc"),
                 txt2tags = c("aap", "aas", "aat", "adoc", "bbcode", "creole", "csv", "dbk", "doku", "gwiki", "html", "html5", "lout", "man", "md", "mgp", "moin", "ods", "pm6", "pmw", "red", "rtf", "sgml", "spip", "tex", "txt", "txt2t", "wiki", "xhtml", "xhtmls"),
                 pandoc = c("native", "json", "html", "html+lhs", "s5", "slidy", "docbook", "opendocument", "latex", "latex+lhs", "context", "texinfo", "man", "markdown", "markdown+lhs", "plain", "rst", "rst+lhs", "mediawiki", "textile", "rtf", "org", "odt", "epub"),
                 markdown2pdf = "")

.extensions <- list(docbook = "xml",
                    slidy = "html",
                    xhtml = "html",
                    chunked = "html",
                    htmlhelp = "html",
                    asciidoc = "adoc",
                    html5 = "html",
                    txt2t = "t2t",
                    xhtml = "html",
                    xhtmls = "html",
                    "html+lhs" = "html",
                    s5 = "html",
                    slidy = "html",
                    opendocument = "odt",
                    latex = "tex",
                    "latex+lhs" = "tex",
                    markdown = "md",
                    "markdown+lhs" = "md",
                    "rst+lhs" = "rst")

.cli <- list(asciidoc = c("asciidoc %options", paste(Sys.getenv("COMSPEC"), "/c", "asciidoc.py %options"), "bash -c \"asciidoc %options \""),
             a2x = c("a2x %options", paste(Sys.getenv("COMSPEC"), "/c", "a2x.py %options"), "bash -c \"a2x %options \""),
             txt2tags = c("txt2tags %options", paste(Sys.getenv("COMSPEC"), "/c", "txt2tags.py %options"), "bash -c \"txt2tags %options \""),
             pandoc = c("pandoc %options", paste(Sys.getenv("COMSPEC"), "/c", "pandoc %options"), "bash -c \"pandoc %options \""),
             markdown2pdf = c("markdown2pdf %options", paste(Sys.getenv("COMSPEC"), "/c", "markdown2pdf %options"), "bash -c \"markdown2pdf %options \""))

# %i input
# %o output
# %f format
# %d directory
# %e encoding
# %O other options
.args <- list(asciidoc = "-a encoding=%e -b %f %O -o %d/%o %i",
              a2x = "-a encoding=%e -D %d -f %f %O %i",
              txt2tags = "--encoding=%e -t %f %O -o %d/%o %i",
              pandoc = "-t %f -o %d/%o %O %i",
              markdown2pdf = "-o %d/%o %O %i")

.O <- list(asciidoc = "-a toc",
           a2x = "-a toc",
           txt2tags = "",
           pandoc = "-s",
           markdown2pdf = "")

.f <- list(asciidoc = "html",
           a2x = "xhtml",
           txt2tags = "html",
           pandoc = "html",
           markdown2pdf = "pdf")

.d <- list(asciidoc = ".",
           a2x = ".",
           txt2tags = ".",
           pandoc = ".",
           markdown2pdf = ".")

.e <- list(asciidoc = "UTF-8",
           a2x = "UTF-8",
           txt2tags = "UTF-8",
           pandoc = "",
           markdown2pdf = "")

.preambule <- list(asciidoc =
"= %title
:author:    %author
:email:     %email
:revdate:   %date

",
                   txt2tags =
"%title
%author
%date

",
                   pandoc =
"% %title
% %author %email
% %date

")

##' replace
##'
##' @param backend backend
##' @param plateform plateform
##' @param cygwin cygwin
##' @param i i
##' @param f f
##' @param d d
##' @param e e
##' @param O O
##' @keywords internal
##' @author David Hajage
replace <- function(backend = "asciidoc", plateform = version$os, cygwin = FALSE, i, f = NULL, d = NULL, e = NULL, O = NULL) {
  if (is.null(f))
    f <- .f[[backend]]
  if (is.null(d))
    d <- dirname(i)
  if (is.null(e))
    e <- .e[[backend]]
  if (is.null(O))
    O <- .O[[backend]]

  extension <- ifelse(f %in% names(.extensions), .extensions[[f]], f)
  basefile <- sub("(.+)(\\..+$)", "\\1", basename(i))
  file <- paste(basefile, extension, sep = ".")

  windows <- grepl("w|W", plateform)
  if (windows) {
    if (cygwin) {
      cli <- .cli[[backend]][3]
    } else {
      cli <- .cli[[backend]][2]
    }
  } else {
    cli <- .cli[[backend]][1]
  }

  args <- .args[[backend]]
  args <- sub("%i", i, args)
  args <- sub("%f", f, args)
  args <- sub("%d", d, args)
  args <- sub("%o", file, args)
  args <- sub("%e", e, args)
  args <- sub("%O", O, args)

  results <- sub("%options", args, cli)
  attr(results, "file") <- file
  attr(results, "directory") <- d
  attr(results, "f") <- f
  attr(results, "windows") <- windows
  attr(results, "cygwin") <- cygwin
  results
}

##' Concert a file with specified backend
##'
##' This function convert a file with asciidoc, txt2tags or pandoc backend
##' @title Convert a file with specified backend
##' @param i input file
##' @param d output directory
##' @param f format
##' @param e encoding
##' @param O other options
##' @param backend backend (\code{"asciidoc"}, \code{"txt2tags"} or \code{"pandoc"})
##' @param cygwin use cygwin?
##' @param open open resulting file?
##' @return Nothing
##' @export
##' @author David Hajage
convert <- function(i, d = NULL, f = NULL, e = NULL, O = NULL, backend = "asciidoc", cygwin = FALSE, open = FALSE) {
  cmd <- replace(backend, cygwin = cygwin, i = i, d = d, f = f, e = e, O = O)
  err <- system(cmd, wait = TRUE)
  invisible(err)

  file <- attr(cmd, "file")
  f <- attr(cmd, "f")
  directory <- attr(cmd, "d")
  windows <- attr(cmd, "windows")

  if (f == "chunked") {
    dfile <- paste(directory, paste(sub("(.+)(\\..+$)", "\\1", file), "chunked", sep = "."), "index.html", sep = "/")
  } else {
    dfile <- paste(directory, file, sep = "/")
  }

  if (open) {
    cat("Trying to open ", dfile, sep = "")
    if (windows) {
      cat(" with shell.exec...\n")
      if (!grepl(":", dfile)) {
        dfile <- paste("\"", sub("(^.+)(/$)", "\\", getwd()), "/", dfile, "\"", sep = "")
      }
      shell.exec(dfile)
    } else {
      cat(" with xdg-open...\n")
      system(paste(shQuote("/usr/bin/xdg-open"), shQuote(dfile)), wait = FALSE, ignore.stderr = TRUE)
    }
  }
}

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
##' @param type ascii type
##' @param ... not used
##' @export
##' @author David Hajage
print.section <- function(x, type = getOption("asciiType"), ...) {
  caption <- x$caption
  caption.level <- x$caption.level
  if (type == "asciidoc")
    results <- header.asciidoc(caption, caption.level)
  if (type == "t2t")
    results <- header.t2t(caption, caption.level)
  if (type == "pandoc")
    results <- header.pandoc(caption, caption.level)
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
    if (class(text[[i]]) == "sexpr") {
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
##' @param type ascii type
##' @param ... not used
##' @export
##' @author David Hajage
print.out <- function(x, type = getOption("asciiType"), ...) {
  results <- x[[2]]
  cat("\n")
  if (results == "verbatim") {
    if (type == "asciidoc")
      cat("----\n")
    if (type == "t2t")
      cat("```\n")
    if (type == "pandoc")
      cat("\n~~~~~~~{.R}\n")
  }
  print(x[[1]], ...)
  if (results == "verbatim") {
    if (type == "asciidoc")
      cat("----\n")
    if (type == "t2t")
      cat("```\n")
    if (type == "pandoc")
      cat("~~~~~~~~~~~\n\n")
  }
}

##' Produce a report
##'
##' Produce a report from a list of R objects
##' @title Report creation
##' @param ... R objects (not used if \code{"list"} is not NULL)
##' @param list list of R objects
##' @param file name of the output file (without extension)
##' @param format format of the output file
##' @param open open resulting file?
##' @param backend backend
##' @param encoding encoding
##' @param options other options
##' @param cygwin use cygwin?
##' @param title title of the report
##' @param author author of the report
##' @param email email of the author
##' @param date date
##' @return
##' @export
##' @author David Hajage
export <- function(..., list = NULL, file = NULL, format = NULL, open = NULL, backend = "asciidoc", encoding = NULL, options = NULL, cygwin = FALSE, title = NULL, author = NULL, email = NULL, date = NULL) {
  
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

  wd <- dirname(file)
  if (is.null(title)) {
    title <- sub("\\~", "\\\\~", paste(wd, basename(file), sep = "/"))
    if (backend == "asciidoc")
      title <- beauty.asciidoc(title, "m")
    if (backend == "text2tags")
      title <- beauty.t2t(title, "m")
    if (backend == "pandoc")
      title <- beauty.pandoc(title, "m")
  }

  if (backend == "a2x") {
    preambule <- .preambule[["asciidoc"]]
  } else {
    preambule <- .preambule[[backend]]
  }

  if (is.null(author)) {
    author <- paste(version$language, " ", paste(version$major, version$minor, sep = "."), ", ascii ", packageDescription("ascii")$Version, sep = "")
  }

  if (is.null(email)) {
    email <- "cran@r-project.org"
  }

  if (is.null(date)) {
    date <- format(Sys.time(), "%Y/%m/%d %X")
  }

  preambule <- sub("%title", title, preambule)
  preambule <- sub("%author", author, preambule)
  preambule <- sub("%email", email, preambule)
  preambule <- sub("%date", date, preambule)

  if (is.null(list)) {
    args <- list(...)
  } else {
    args <- list
  }

  lines <- capture.output({
    cat(preambule)
    
    for (i in seq_along(args)) {
      arg <- args[[i]]
      if (!is.null(names(args))) {
        if (names(args)[i] != "") {
          cat(".", names(args)[i], "\n", sep = "")
        }
      }
      if ("ascii" %in% class(arg)) {
        print(arg)
        cat("\n")
      } else if ("out" %in% class(arg) | "section" %in% class(arg) | "paragraph" %in% class(arg)) {
        print(arg)
      } else {
        print(out(arg, "verbatim"))
      }
    }})
  textfile <- paste(file, "txt", sep = ".")
  f <- file(textfile, "w")
  writeLines(lines, f)
  close(f)

  convert(i = textfile, d = NULL, f = format, e = encoding, O = options, backend = backend, cygwin = cygwin, open = open)  
}

Report <- proto(expr = {
  new <- function(., file = NULL, format = "html", open = NULL, backend = "asciidoc", encoding = NULL, options = NULL, cygwin = FALSE, title = NULL, author = NULL, email = NULL, date = NULL)
    proto(., list, file = NULL, format = "html", open = NULL, backend = "asciidoc", encoding = NULL, options = NULL, cygwin = FALSE, title = NULL, author = NULL, email = NULL, date = NULL)

  objects <- list()
  
  add <- function(., ...) {
    obj <- list(...)
    .$objects <- c(.$objects, obj)
  }

  export <- function(.) {
    .super$export(list = .$objects, file = .$file, format = .$format, open = .$open, backend = .$backend, encoding = .$encoding, options = .$options, cygwin = .$cygwin, title = .$title, author = .$author, email = .$email, date = .$date)
  }
})
