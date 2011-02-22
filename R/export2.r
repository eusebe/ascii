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
# %O options
.args <- list(asciidoc = "-a encoding=%e -b %f %O -o %d/%o %i",
              a2x = "-a encoding=%e -D %d -f %f %O %i",
              txt2tags = "--encoding=%e -t %f %O -o %d/%o %i",
              pandoc = "-t %f -o %d/%o %O %i",
              markdown2pdf = "-o %d/%o %O %i")

.O <- list(asciidoc = "-a toc",
           a2x = "-a toc",
           txt2tags = "",
           pandoc = "",
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
  attr(results, "windows") <- windows
  attr(results, "cygwin") <- cygwin
  results
}

convert <- function(i, d = NULL, f = NULL, e = NULL, O = NULL, backend = "asciidoc", cygwin = FALSE, open = FALSE) {
  cmd <- replace(backend, cygwin = cygwin, i = i, d = d, f = f, e = e, O = O)
  err <- system(cmd, wait = TRUE)
  invisible(err)

  file <- attr(cmd, "file")
  directory <- attr(cmd, "d")
  windows <- attr(cmd, "windows")
  
  if (f != "chunked") {
    dfile <- paste(directory, file, sep = "/")
  } else {
    dfile <- paste(directory, paste(sub("(.+)(\\..+$)", "\\1", file), "chunked", sep = "."), "index.html", sep = "/")
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


export <- function(..., list = NULL, file = NULL, format = "html", open = NULL, backend = "asciidoc", encoding = NULL) {

}
