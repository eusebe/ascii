##' RweaveAsciiSetup
##'
##' @param file file
##' @param syntax syntax
##' @param output output
##' @param quiet quite
##' @param debug debug
##' @param stylepath stylepath
##' @param ... ...
##' @keywords internal
RweaveAsciiSetup <-
    function(file, syntax, output = NULL, quiet = FALSE, debug = FALSE,
             extension = "txt", backend = "asciidoc", openSchunk = "----",
             closeSchunk = "\n----\n", openSinput = "", closeSinput = "", openSoutput = "\n",
             closeSoutput = "", indent = "", openInclude = "include::", closeInclude = ".txt[]",
             openFig = "image::", closeFig = "[]", ...)
{
    dots <- list(...)
    if (is.null(output)) {
        prefix.string <- basename(sub(syntax$extension, "", file))
        output <- paste(prefix.string, extension, sep=".")
    } else prefix.string <- basename(sub(paste("\\.", extension, "$", sep = ""), "", output))

    if (!quiet) cat("Writing to file ", output, "\n",
                   "Processing code chunks with options ...\n", sep = "")
    ## Did we re-encode the input?  In which case we need to do
    ## something about the output.
    output <- if (identical(attr(file, "encoding"), "latin1")) {
        file(output, open = "w+", encoding = "latin1")
    } else file(output, open = "w+")

    ## if (missing(stylepath)) {
    ##     p <- Sys.getenv("SWEAVE_STYLEPATH_DEFAULT")
    ##     stylepath <-
    ##         if (length(p) >= 1L && nzchar(p[1L])) identical(p, "TRUE") else FALSE
    ## }
    ## if (stylepath) {
    ##     styfile <- file.path(R.home("share"), "texmf", "tex", "latex", "Sweave")
    ##     if (.Platform$OS.type == "windows") styfile <- chartr("\\", "/", styfile)
    ##     if (length(grep(" ", styfile)))
    ##         warning(gettextf("path to '%s' contains spaces,\n", styfile),
    ##                 gettext("this may cause problems when running LaTeX"),
    ##                 domain = NA)
    ## } else styfile <- "Sweave"

    options <- list(prefix = TRUE, prefix.string = prefix.string,
                    engine = "R", print = FALSE, eval = TRUE, fig = FALSE,
                    pdf = FALSE, eps = FALSE, png = FALSE, jpeg = TRUE, format = "jpg",
                    width = 6, height = 6, resolution = 100, term = TRUE,
                    echo = TRUE, keep.source = FALSE, results = "verbatim",
                    split = FALSE, strip.white = "true", include = TRUE,
                    pdf.version = grDevices::pdf.options()$version,
                    pdf.encoding = grDevices::pdf.options()$encoding,
                    expand = TRUE, openSchunk = openSchunk, closeSchunk = closeSchunk,
                    openSinput = openSinput, closeSinput = closeSinput,
                    openSoutput = openSoutput, closeSoutput = closeSoutput, indent = indent,
                    openInclude = openInclude, closeInclude = closeInclude,
                    openFig = openFig, closeFig = closeFig)
    options[names(dots)] <- dots

    ## to be on the safe side: see if defaults pass the check
    options <- RweaveAsciiOptions(options)

    list(output = output, extension = extension, backend = backend,
         debug = debug, quiet = quiet, syntax = syntax,
         options = options, chunkout = list(), # a list of open connections
         srclines = integer()) }

##' makeRweaveAsciiCodeRunner
##'
##' @param evalFunc evalFunc
##' @keywords internal
makeRweaveAsciiCodeRunner <- function(evalFunc = RweaveEvalWithOpt)
{
    ## Return a function suitable as the 'runcode' element
    ## of an Sweave driver.  evalFunc will be used for the
    ## actual evaluation of chunk code.
    ## FIXME: well, actually not for the figures.
    ## If there were just one figure option set, we could eval the chunk
    ## only once.
    function(object, chunk, options) {
        pdf.Swd <- function(name, width, height, ...)
            grDevices::pdf(file = paste(chunkprefix, "pdf", sep = "."),
                           width = width, height = height,
                           version = options$pdf.version,
                           encoding = options$pdf.encoding)
        eps.Swd <- function(name, width, height, ...)
            grDevices::postscript(file = paste(name, "eps", sep = "."),
                                  width = width, height = height,
                                  paper = "special", horizontal = FALSE)
        png.Swd <- function(name, width, height, options, ...)
            grDevices::png(filename = paste(chunkprefix, "png", sep = "."),
                           width = width, height = height,
                           res = options$resolution, units = "in")
        jpeg.Swd <- function(name, width, height, options, ...)
            grDevices::jpeg(filename = paste(chunkprefix, "jpg", sep = "."),
                            width = width, height = height,
                            res = options$resolution, units = "in")

        if (!(options$engine %in% c("R", "S"))) return(object)

        if (!object$quiet) {
            cat(formatC(options$chunknr, width = 2), ":")
            if (options$echo) cat(" echo")
            if (options$keep.source) cat(" keep.source")
            if (options$eval) {
                if (options$print) cat(" print")
                if (options$term) cat(" term")
                cat("", options$results)
                if (options$fig) {
                    if (options$eps) cat(" eps")
                    if (options$pdf) cat(" pdf")
                    if (options$png) cat(" png")
                    if (options$jpeg) cat(" jpeg")
                    if (!is.null(options$grdevice)) cat("", options$grdevice)
                }
            }
            if (!is.null(options$label))
                cat(" (label=", options$label, ")", sep = "")
            cat("\n")
        }

        chunkprefix <- RweaveChunkPrefix(options)

        if (options$split) {
            ## [x][[1L]] avoids partial matching of x
            chunkout <- object$chunkout[chunkprefix][[1L]]
            if (is.null(chunkout)) {
                chunkout <- file(paste(chunkprefix, object$extension, sep = "."), "w")
                if (!is.null(options$label))
                    object$chunkout[[chunkprefix]] <- chunkout
            }
        } else chunkout <- object$output

        srcfile <- srcfilecopy(object$filename, chunk)
        utils:::SweaveHooks(options, run = TRUE)

        ## Note that we edit the error message below, so change both
        ## if you change this line:
        chunkexps <- try(parse(text = chunk, srcfile = srcfile), silent = TRUE)

        if (inherits(chunkexps, "try-error"))
            chunkexps[1L] <- sub(" parse(text = chunk, srcfile = srcfile) : \n ",
                                 "", chunkexps[1L], fixed = TRUE)

        RweaveTryStop(chunkexps, options)

        ## Some worker functions used below...
        putSinput <- function(dce) {
            if (!openSinput) {
                if (!openSchunk) {
                    cat(options$openSchunk, file = chunkout)
                    linesout[thisline + 1L] <<- srcline
                    thisline <<- thisline + 1L
                    openSchunk <<- TRUE
                }
                cat(options$openSinput, file = chunkout)
                openSinput <<- TRUE
            }
            cat("\n", paste(paste(options$indent, getOption("prompt"), sep = ""), dce[1L:leading],
                            sep = "", collapse = "\n"),
                file = chunkout, sep = "")
            if (length(dce) > leading)
                cat("\n", paste(paste(options$indent, getOption("continue"), sep = ""), dce[-(1L:leading)],
                                sep = "", collapse = "\n"),
                    file = chunkout, sep = "")
            linesout[thisline + seq_along(dce)] <<- srcline
            thisline <<- thisline + length(dce)
        }

        trySrcLines <- function(srcfile, showfrom, showto, ce) {
            lines <- try(suppressWarnings(getSrcLines(srcfile, showfrom, showto)),
                         silent = TRUE)
            if (inherits(lines, "try-error")) {
                if (is.null(ce)) lines <- character()
                else lines <- deparse(ce, width.cutoff = 0.75*getOption("width"))
            }
            lines
        }

        echoComments <- function(showto) {
            if (options$echo && !is.na(lastshown) && lastshown < showto) {
                dce <- trySrcLines(srcfile, lastshown + 1L, showto, NULL)
                linedirs <- grepl("^#line ", dce)
		dce <- dce[!linedirs]
                leading <<- length(dce) # These are all trailing comments
                putSinput(dce)
                lastshown <<- showto
            }
        }

        openSinput <- FALSE
        openSchunk <- FALSE

        srclines <- attr(chunk, "srclines")
        linesout <- integer()      # maintains concordance
        srcline <- srclines[1L]    # current input line
        thisline <- 0L             # current output line
        lastshown <- 0L            # last line already displayed;

        refline <- NA    # line containing the current named chunk ref
        leading <- 1L    # How many lines get the user prompt

        srcrefs <- attr(chunkexps, "srcref")

        for (nce in seq_along(chunkexps)) {
            ce <- chunkexps[[nce]]
            if (options$keep.source && nce <= length(srcrefs) &&
                !is.null(srcref <- srcrefs[[nce]])) {
                showfrom <- srcref[7L]
                showto <- srcref[8L]

                dce <- trySrcLines(srcfile, lastshown+1L, showto, ce)
                leading <- showfrom - lastshown

                lastshown <- showto
                srcline <- srcref[3L]

                linedirs <- grepl("^#line ", dce)
                dce <- dce[!linedirs]
                # Need to reduce leading lines if some were just removed
                leading <- leading - sum(linedirs[seq_len(leading)])

                while (length(dce) && length(grep("^[[:blank:]]*$", dce[1L]))) {
                    dce <- dce[-1L]
                    leading <- leading - 1L
                }
            } else {
                dce <- deparse(ce, width.cutoff = 0.75*getOption("width"))
                leading <- 1L
            }
            if (object$debug)
                cat("\nRnw> ", paste(dce, collapse = "\n+  "),"\n")

            if (options$echo && length(dce)) putSinput(dce)

            ## avoid the limitations (and overhead) of output text connections
            if (options$eval) {
                tmpcon <- file()
                sink(file = tmpcon)
                err <- evalFunc(ce, options)
                cat("\n")           # make sure final line is complete
                sink()
                output <- readLines(tmpcon)
                close(tmpcon)
                ## delete empty output
                if (length(output) == 1L && !nzchar(output[1L])) output <- NULL
                RweaveTryStop(err, options)
            } else output <- NULL

            ## or writeLines(output)
            if (length(output) && object$debug)
                cat(paste(output, collapse = "\n"))

            if (length(output) && (options$results != "hide")) {
                if (openSinput) {
                    cat(options$closeSinput, file = chunkout)
                    linesout[thisline + 1L:2L] <- srcline
                    thisline <- thisline + 2L
                    openSinput <- FALSE
                }
                if (options$results == "verbatim") {
                    if (!openSchunk) {
                        cat(options$openSchunk, file = chunkout)
                        linesout[thisline + 1L] <- srcline
                        thisline <- thisline + 1L
                        openSchunk <- TRUE
                    }
                    cat(options$openSoutput, file = chunkout)
                    linesout[thisline + 1L] <- srcline
                    thisline <- thisline + 1L
                }
                if (options$results=="ascii"){
                    if (openSinput){
                        cat(options$closeSinput,
                            file=chunkout, append=TRUE)
                        linesout[thisline + 1L] <- srcline
                        thisline <- thisline + 1L
                        openSchunk <- TRUE
                    }
                    if (openSchunk) {
                        cat(options$closeSchunk,
                            file=chunkout, append=TRUE)
                        linesout[thisline + 1L] <- srcline
                        thisline <- thisline + 1L
                        openSchunk <- FALSE
                    }
                }

                if (options$results == "verbatim")
                    output <- paste("", output,collapse="\n", sep = options$indent)
                else
                    output <- paste(output, collapse="\n")
                if (options$strip.white %in% c("all", "true")) {
                    output <- sub("^[[:space:]]*\n", "", output)
                    output <- sub("\n[[:space:]]*$", "", output)
                    if (options$strip.white == "all")
                        output <- sub("\n[[:space:]]*\n", "\n", output)
                }
                cat(output, file = chunkout)
                if(options$results == "ascii")
                  cat("\n", file = chunkout)
                count <- sum(strsplit(output, NULL)[[1L]] == "\n")
                if (count > 0L) {
                    linesout[thisline + 1L:count] <- srcline
                    thisline <- thisline + count
                }

                remove(output)

                if (options$results == "verbatim") {
                    cat(options$closeSoutput, file = chunkout)
                    linesout[thisline + 1L:2L] <- srcline
                    thisline <- thisline + 2L
                }
            }
        } # end of loop over chunkexps.

        ## Echo remaining comments if necessary
        if (options$keep.source) echoComments(length(srcfile$lines))

        if (openSinput) {
            cat(options$closeSinput, file = chunkout)
            linesout[thisline + 1L:2L] <- srcline
            thisline <- thisline + 2L
        }

        if (openSchunk) {
            cat(options$closeSchunk, file = chunkout)
            linesout[thisline + 1L] <- srcline
            thisline <- thisline + 1L
        }

        if (is.null(options$label) && options$split) close(chunkout)

        if (options$split && options$include) {
            cat(options$openInclude, chunkprefix, options$closeInclude, "\n\n", sep = "",
                file = object$output)
            linesout[thisline + 1L] <- srcline
            thisline <- thisline + 1L
        }

        if (options$fig && options$eval) {
            devs <- list()
            if (options$pdf) devs <- c(devs, list(pdf.Swd))
            if (options$eps) devs <- c(devs, list(eps.Swd))
            if (options$png) devs <- c(devs, list(png.Swd))
            if (options$jpeg) devs <- c(devs, list(jpeg.Swd))
            if (!is.null(grd <- options$grdevice))
                devs <- c(devs, list(get(grd, envir = .GlobalEnv)))
            for (dev in devs) {
                dev(name = chunkprefix, width = options$width,
                    height = options$height, options)
                err <- tryCatch({
                    utils:::SweaveHooks(options, run = TRUE)
                    eval(chunkexps, envir = .GlobalEnv)
                }, error = function(e) {
                    grDevices::dev.off()
                    stop(conditionMessage(e), call. = FALSE, domain = NA)
                })
                grDevices::dev.off()
            }

            if (options$include) {
                cat(options$openFig, chunkprefix, ".", options$format, options$closeFig, "\n\n", sep = "",
                    file = object$output)
                linesout[thisline + 1L] <- srcline
                thisline <- thisline + 1L
            }
        }
        object$linesout <- c(object$linesout, linesout)
        object
    }
}

RweaveAsciiRuncode <- makeRweaveAsciiCodeRunner()

##' RweaveAsciiWritedoc
##'
##' @param object object
##' @param chunk chunk
##' @keywords internal
RweaveAsciiWritedoc <- function(object, chunk)
{
    linesout <- attr(chunk, "srclines")

  ##   if (length(grep("\\usepackage[^\\}]*Sweave.*\\}", chunk)))
  ##       object$havesty <- TRUE

  ##   if (!object$havesty) {
 	## begindoc <- "^[[:space:]]*\\\\begin\\{document\\}"
 	## which <- grep(begindoc, chunk)
 	## if (length(which)) {
  ##           chunk[which] <- sub(begindoc,
  ##                               paste("\\\\usepackage{",
  ##                                     object$styfile,
  ##                                     "}\n\\\\begin{document}", sep = ""),
  ##                               chunk[which])
  ##           linesout <- linesout[c(1L:which, which,
  ##                                  seq(from = which+1L,
  ##                                      length.out = length(linesout)-which))]
  ##           object$havesty <- TRUE
  ##       }
  ##   }

    while(length(pos <- grep(object$syntax$docexpr, chunk)))
    {
        cmdloc <- regexpr(object$syntax$docexpr, chunk[pos[1L]])
        cmd <- substr(chunk[pos[1L]], cmdloc,
                      cmdloc + attr(cmdloc, "match.length") - 1L)
        cmd <- sub(object$syntax$docexpr, "\\1", cmd)
        if (object$options$eval) {
            val <- as.character(eval(parse(text = cmd), envir = .GlobalEnv))
            ## protect against character(), because sub() will fail
            if (length(val) == 0L) val <- ""
        } else val <- paste("\\\\verb{<<", cmd, ">>{", sep = "")

        chunk[pos[1L]] <- sub(object$syntax$docexpr, val, chunk[pos[1L]])
    }

    ## Process \SweaveOpts{} or similar
    while(length(pos <- grep(object$syntax$docopt, chunk)))
    {
        opts <- sub(paste(".*", object$syntax$docopt, ".*", sep = ""),
                    "\\1", chunk[pos[1L]])
        object$options <- utils:::SweaveParseOptions(opts, object$options,
                                             RweaveAsciiOptions)
        ## if (isTRUE(object$options$concordance)
        ##     && !object$haveconcordance) {
        ##     if (isTRUE(object$hasSweaveInput))
        ##     	warning("\\SweaveInput is not compatible with concordances.",
        ##     	        immediate. = TRUE)
        ##     savelabel <- object$options$label
        ##     object$options$label <- "concordance"
        ##     prefix <- RweaveChunkPrefix(object$options)
        ##     object$options$label <- savelabel
        ##     object$concordfile <- paste(prefix, "tex", sep = ".")
        ##     chunk[pos[1L]] <- sub(object$syntax$docopt,
        ##                           paste("\\\\input{", prefix, "}", sep = ""),
        ##                           chunk[pos[1L]])
        ##     object$haveconcordance <- TRUE
        ## } else
            chunk[pos[1L]] <- sub(object$syntax$docopt, "", chunk[pos[1L]])
    }

    cat(chunk, sep = "\n", file = object$output)
    object$linesout <- c(object$linesout, linesout)

    object
}

##' RweaveAsciiFinish
##'
##' @param object object
##' @param error error
##' @keywords internal
RweaveAsciiFinish <- function(object, error = FALSE)
{
    outputname <- summary(object$output)$description
    inputname <- object$filename
    if (!object$quiet && !error)
        cat("\n",
            sprintf(paste("You can now run", object$backend, "on '%s'"), outputname),
            "\n", sep = "")
    close(object$output)
    if (length(object$chunkout))
        for (con in object$chunkout) close(con)
    ## if (object$haveconcordance) {
    ## 	## This output format is subject to change.  Currently it contains
    ## 	## three parts, separated by colons:
    ## 	## 1.  The output .tex filename
    ## 	## 2.  The input .Rnw filename
    ## 	## 3.  The input line numbers corresponding to each output line.
    ## 	##     This are compressed using the following simple scheme:
    ## 	##     The first line number, followed by
    ## 	##     a run-length encoded diff of the rest of the line numbers.
    ##     linesout <- object$linesout
    ##     vals <- rle(diff(linesout))
    ##     vals <- c(linesout[1L], as.numeric(rbind(vals$lengths, vals$values)))
    ## 	concordance <- paste(strwrap(paste(vals, collapse = " ")), collapse = " %\n")
    ## 	special <- paste("\\Sconcordance{concordance:", outputname, ":",
    ##                      inputname, ":%\n", concordance,"}\n", sep = "")
    ## 	cat(special, file = object$concordfile)
    ## }
    invisible(outputname)
}

## This is the check function for both RweaveAscii and Rtangle drivers.

##' RweaveAsciiOptions
##'
##' @param options options
##' @keywords internal
RweaveAsciiOptions <- function(options)
{
    ## ATTENTION: Changes in this function have to be reflected in the
    ## defaults in the initialization in RweaveAsciiSetup

    ## convert a character string to logical
    c2l <- function(x)
        if (is.null(x)) FALSE else as.logical(toupper(as.character(x)))

    ## numeric
    NUMOPTS <- c("width", "height", "resolution")

    ## not logical
    NOLOGOPTS <- c(NUMOPTS, "results", "prefix.string", "engine",
                   "label", "strip.white", "pdf.version", "pdf.encoding",
                   "grdevice", "format", "openSchunk", "closeSchunk","openSinput",
                   "closeSinput", "openSoutput", "closeSoutput", "indent", "openInclude",
                   "closeInclude", "openFig", "closeFig")

    for (opt in names(options)) {
        if (! (opt %in% NOLOGOPTS)) {
            oldval <- options[[opt]]
            if (!is.logical(options[[opt]]))
                options[[opt]] <- c2l(options[[opt]])
            if (is.na(options[[opt]]))
                stop(gettextf("invalid value for '%s' : %s", opt, oldval),
                     domain = NA)
        } else if (opt %in% NUMOPTS)
            options[[opt]] <- as.numeric(options[[opt]])
    }

    if (!is.null(options$results))
        options$results <- tolower(as.character(options$results))
    options$results <- match.arg(options$results, c("verbatim", "ascii", "hide"))

    if (!is.null(options$strip.white))
        options$strip.white <- tolower(as.character(options$strip.white))
    options$strip.white <-
        match.arg(options$strip.white, c("true", "false", "all"))
    options
}

##' RtangleAscii
##'
##' @keywords internal
RtangleAscii <-  function()
{
    list(setup = utils:::RtangleSetup,
         runcode = utils:::RtangleRuncode,
         writedoc = RtangleAsciiWritedoc,
         finish = utils:::RtangleFinish,
         checkopts = RweaveAsciiOptions)
}

##' RtangleAsciidocWritedoc
##' @param object object
##' @param chunk chunk
##' @keywords internal
RtangleAsciiWritedoc <- function(object, chunk)
{
    while(length(pos <- grep(object$syntax$docopt, chunk))) {
        opts <- sub(paste(".*", object$syntax$docopt, ".*", sep = ""),
                    "\\1", chunk[pos[1L]])
        object$options <- utils:::SweaveParseOptions(opts, object$options,
                                             RweaveAsciiOptions)
        chunk[pos[1L]] <- sub(object$syntax$docopt, "", chunk[pos[1L]])
    }
    object
}

#########################################################

##' A driver to parse asciidoc noweb files with Sweave tool
##' This driver parses asciidoc files containing R code and replace pieces of
##' code with their output.
##' 
##' 
##' @aliases RweaveAsciidoc RtangleAsciidoc RweaveAsciidocOptions
##'   RweaveAsciidocFinish RweaveAsciidocWritedoc RweaveAsciidocSetup
##'   RweaveAsciidocRuncode cacheSweaveAsciidoc weaverAsciidoc
##' @return None value is returned. From a .Rnw noweb file, the corresponding
##'   .txt is produced (as eventuals files for graphs).
##' @note In order to work properly, noweb codes have to be located at the
##'   beginning of a line (no indentation).
##' 
##' Compare with RweaveLatex driver, RweaveAsciidoc provides four new options :
##'   \code{res} for the resolution of jpg or png figure (if produced),
##'   \code{ext} (extension) for the format of figure that will be inserted,
##'   and \code{png} and \code{jpg} (from \code{R2HTML} package) to produce png
##'   and jpg figures.
##' 
##' In addition, \code{cache} option from \code{cacheSweave} or \code{weaver}
##'   package is also available with \code{cacheSweaveAsciidoc} driver and
##'   \code{weaverAsciidoc} driver.
##' 
##' A wrapper for \code{Sweave} can be used, named \code{Asciidoc}.
##' @author David Hajage \email{dhajage@@gmail.com}
##' @seealso \code{\link[utils]{Sweave}}, \code{\link[ascii]{Asciidoc}}
##' @keywords IO file
##' @export
##' @examples
##'   \dontrun{
##' library(ascii)
##' Asciidoc("file.Rnw")
##'   }
##' 
RweaveAsciidoc <- function()
{
    list(setup = RweaveAsciiSetup,
         runcode = RweaveAsciiRuncode,
         writedoc = RweaveAsciiWritedoc,
         finish = RweaveAsciiFinish,
         checkopts = RweaveAsciiOptions)
}

##' A driver to parse txt2tags noweb files with Sweave tool
##' This driver parses txt2tags files containing R code and replace pieces of
##' code with their output.
##' 
##' 
##' @aliases RweaveT2t RtangleT2t RweaveT2tOptions RweaveT2tFinish
##'   RweaveT2tWritedoc RweaveT2tSetup RweaveT2tRuncode cacheSweaveT2t
##'   weaverT2t
##' @return None value is returned. From a .Rnw noweb file, the corresponding
##'   .t2t is produced (as eventuals files for graphs).
##' @note In order to work properly, noweb codes have to be located at the
##'   beginning of a line (no indentation).
##' 
##' Compare with RweaveLatex driver, RweaveT2t provides four new options :
##'   \code{res} for the resolution of jpg or png figure (if produced),
##'   \code{ext} (extension) for the format of figure that will be inserted,
##'   and \code{png} and \code{jpg} (from \code{R2HTML} package) to produce png
##'   and jpg figures.
##' 
##' In addition, \code{cache} option from \code{cacheSweave} or \code{weaver}
##'   package is also available with \code{cacheSweaveT2t} driver and
##'   \code{weaverT2t} driver.
##' 
##' A wrapper for \code{Sweave} can be used, named \code{T2t}.
##' @author David Hajage \email{dhajage@@gmail.com}
##' @seealso \code{\link[utils]{Sweave}}, \code{\link[ascii]{T2t}}
##' @keywords IO file
##' @export
##' @examples
##'   \dontrun{
##' library(ascii)
##' T2t("file.Rnw")
##'   }
##' 
RweaveT2t <- function()
{
    list(setup = RweaveT2tSetup,
         runcode = RweaveAsciiRuncode,
         writedoc = RweaveAsciiWritedoc,
         finish = RweaveAsciiFinish,
         checkopts = RweaveAsciiOptions)
}

##' RweaveT2tSetup
##'
##' @param file file
##' @param syntax syntax
##' @param output output
##' @param quiet quite
##' @param debug debug
##' @param stylepath stylepath
##' @param ... ...
##' @keywords internal
RweaveT2tSetup <- RweaveAsciiSetup
formals(RweaveT2tSetup) <-alist(file=, syntax=, output=NULL, quiet=FALSE, debug=FALSE,
                                extension="t2t", backend="txt2tags", openSchunk="```",
                                closeSchunk="\n```\n", openSinput="", closeSinput="",
                                openSoutput="\n", closeSoutput="", indent="", openInclude ="%!include: ",
                                closeInclude=".t2t", openFig="[", closeFig="]", ...=)

##' A driver to parse org noweb files with Sweave tool
##' This driver parses org files containing R code and replace pieces of code
##' with their output.
##' 
##' 
##' @aliases RweaveOrg RtangleOrg RweaveOrgOptions RweaveOrgFinish
##'   RweaveOrgWritedoc RweaveOrgSetup RweaveOrgRuncode cacheSweaveOrg
##'   weaverOrg
##' @return None value is returned. From a .Rnw noweb file, the corresponding
##'   .org is produced (as eventuals files for graphs).
##' @note In order to work properly, noweb codes have to be located at the
##'   beginning of a line (no indentation).
##' 
##' Compare with RweaveLatex driver, RweaveOrg provides four new options :
##'   \code{res} for the resolution of jpg or png figure (if produced),
##'   \code{ext} (extension) for the format of figure that will be inserted,
##'   and \code{png} and \code{jpg} (from \code{R2HTML} package) to produce png
##'   and jpg figures.
##' 
##' In addition, \code{cache} option from \code{cacheSweave} or \code{weaver}
##'   package is also available with \code{cacheSweaveOrg} driver and
##'   \code{weaverOrg} driver.
##' 
##' A wrapper for \code{Sweave} can be used, named \code{Org}.
##' @author David Hajage \email{dhajage@@gmail.com}
##' @seealso \code{\link[utils]{Sweave}}, \code{\link[ascii]{Org}}
##' @keywords IO file
##' @export
##' @examples
##'   \dontrun{
##' library(ascii)
##' Org("file.Rnw")
##'   }
##' 
RweaveOrg <- function()
{
    list(setup = RweaveOrgSetup,
         runcode = RweaveAsciiRuncode,
         writedoc = RweaveAsciiWritedoc,
         finish = RweaveAsciiFinish,
         checkopts = RweaveAsciiOptions)
}

##' RweaveOrgSetup
##'
##' @param file file
##' @param syntax syntax
##' @param output output
##' @param quiet quite
##' @param debug debug
##' @param stylepath stylepath
##' @param ... ...
##' @keywords internal
RweaveOrgSetup <- RweaveAsciiSetup
formals(RweaveOrgSetup) <-alist(file=, syntax=, output=NULL, quiet=FALSE, debug=FALSE,
                                extension="org", backend="org-mode", openSchunk="#+BEGIN_SRC R-transcript",
                                closeSchunk="\n#+END_SRC\n", openSinput="", closeSinput="",
                                openSoutput="\n", closeSoutput="", indent="", openInclude ="#+INCLUDE: \"",
                                closeInclude=".org\"", openFig="[[file:", closeFig="]]", ...=)

##' A driver to parse Pandoc noweb files with Sweave tool
##' This driver parses Pandoc files containing R code and replace pieces of code
##' with their output.
##' 
##' 
##' @aliases RweavePandoc RtanglePandoc RweavePandocOptions RweavePandocFinish
##'   RweavePandocWritedoc RweavePandocSetup RweavePandocRuncode cacheSweavePandoc
##'   weaverPandoc
##' @return None value is returned. From a .Rnw noweb file, the corresponding
##'   .md is produced (as eventuals files for graphs).
##' @note In order to work properly, noweb codes have to be located at the
##'   beginning of a line (no indentation).
##' 
##' Compare with RweaveLatex driver, RweavePandoc provides four new options :
##'   \code{res} for the resolution of jpg or png figure (if produced),
##'   \code{ext} (extension) for the format of figure that will be inserted,
##'   and \code{png} and \code{jpg} (from \code{R2HTML} package) to produce png
##'   and jpg figures.
##' 
##' In addition, \code{cache} option from \code{cacheSweave} or \code{weaver}
##'   package is also available with \code{cacheSweavePandoc} driver and
##'   \code{weaverPandoc} driver.
##' 
##' A wrapper for \code{Sweave} can be used, named \code{Pandoc}.
##' @author David Hajage \email{dhajage@@gmail.com} Matti Pastell \email{matti.pastell@@helsinki.fi}
##' @seealso \code{\link[utils]{Sweave}}, \code{\link[ascii]{Pandoc}}
##' @keywords IO file
##' @export
##' @examples
##'   \dontrun{
##' library(ascii)
##' Pandoc("file.Rnw")
##'   }
##' 
RweavePandoc <- function()
{
    list(setup = RweavePandocSetup,
         runcode = RweaveAsciiRuncode,
         writedoc = RweaveAsciiWritedoc,
         finish = RweaveAsciiFinish,
         checkopts = RweaveAsciiOptions)
}

##' RweavePandocSetup
##'
##' @param file file
##' @param syntax syntax
##' @param output output
##' @param quiet quite
##' @param debug debug
##' @param stylepath stylepath
##' @param ... ...
##' @keywords internal
RweavePandocSetup <- RweaveAsciiSetup
formals(RweavePandocSetup) <-alist(file=, syntax=, output=NULL, quiet=FALSE, debug=FALSE,
                                   extension="md", backend="pandoc", openSchunk="~~~~~~{.R}",
                                   closeSchunk="\n~~~~~~~~~~~\n\n", openSinput="", closeSinput="",
                                   openSoutput="\n", closeSoutput="", indent="", openInclude ="",
                                   closeInclude="", openFig="![](", closeFig=")", ...=)

##' A driver to parse textile noweb files with Sweave tool
##' This driver parses textile files containing R code and replace pieces of
##' code with their output.
##' 
##' 
##' @aliases RweaveTextile RtangleTextile RweaveTextileOptions
##'   RweaveTextileFinish RweaveTextileWritedoc RweaveTextileSetup
##'   RweaveTextileRuncode cacheSweaveTextile weaverTextile
##' @return None value is returned. From a .Rnw noweb file, the corresponding
##'   .txt is produced (as eventuals files for graphs).
##' @note In order to work properly, noweb codes have to be located at the
##'   beginning of a line (no indentation).
##' 
##' Compare with RweaveLatex driver, RweaveTextile provides four new options :
##'   \code{res} for the resolution of jpg or png figure (if produced),
##'   \code{ext} (extension) for the format of figure that will be inserted,
##'   and \code{png} and \code{jpg} (from \code{R2HTML} package) to produce png
##'   and jpg figures.
##' 
##' In addition, \code{cache} option from \code{cacheSweave} or \code{weaver}
##'   package is also available with \code{cacheSweaveTextile} driver and
##'   \code{weaverTextile} driver.
##' 
##' A wrapper for \code{Sweave} can be used, named \code{Textile}.
##' @author David Hajage \email{dhajage@@gmail.com}
##' @seealso \code{\link[utils]{Sweave}}, \code{\link[ascii]{Textile}}
##' @keywords IO file
##' @export
##' @examples
##'   \dontrun{
##' library(ascii)
##' Textile("file.Rnw")
##'   }
##' 
RweaveTextile <- function()
{
    list(setup = RweaveTextileSetup,
         runcode = RweaveAsciiRuncode,
         writedoc = RweaveAsciiWritedoc,
         finish = RweaveAsciiFinish,
         checkopts = RweaveAsciiOptions)
}

##' RweaveTextileSetup
##'
##' @param file file
##' @param syntax syntax
##' @param output output
##' @param quiet quite
##' @param debug debug
##' @param stylepath stylepath
##' @param ... ...
##' @keywords internal
RweaveTextileSetup <- RweaveAsciiSetup
formals(RweaveTextileSetup) <-alist(file=, syntax=, output=NULL, quiet=FALSE, debug=FALSE,
                                    extension="txt", backend="textile", openSchunk="\nbc.. ",
                                    closeSchunk="\n\np. \n\n", openSinput="", closeSinput="",
                                    openSoutput="\n", closeSoutput="", indent="", openInclude ="",
                                    closeInclude="", openFig="!", closeFig="!", ...=)

##' A driver to parse sphinx noweb files with Sweave tool
##' This driver parses sphinx files containing R code and replace pieces of
##' code with their output.
##' 
##' 
##' @aliases RweaveReST RtangleReST RweaveReSTOptions RweaveReSTFinish
##'   RweaveReSTWritedoc RweaveReSTSetup RweaveReSTRuncode cacheSweaveReST
##'   weaverReST
##' @return None value is returned. From a .Rnw noweb file, the corresponding
##'   .rst is produced (as eventuals files for graphs).
##' @note In order to work properly, noweb codes have to be located at the
##'   beginning of a line (no indentation).
##' 
##' Compare with RweaveLatex driver, RweaveReST provides four new options :
##'   \code{res} for the resolution of jpg or png figure (if produced),
##'   \code{ext} (extension) for the format of figure that will be inserted,
##'   and \code{png} and \code{jpg} (from \code{R2HTML} package) to produce png
##'   and jpg figures.
##' 
##' In addition, \code{cache} option from \code{cacheSweave} or \code{weaver}
##'   package is also available with \code{cacheSweaveReST} driver and
##'   \code{weaverReST} driver.
##' 
##' A wrapper for \code{Sweave} can be used, named \code{ReST}.
##' @author David Hajage \email{dhajage@@gmail.com}
##' @seealso \code{\link[utils]{Sweave}}, \code{\link[ascii]{ReST}}
##' @keywords IO file
##' @export
##' @examples
##'   \dontrun{
##' library(ascii)
##' ReST("file.Rnw")
##'   }
##' 
RweaveReST <- function()
{
    list(setup = RweaveReSTSetup,
         runcode = RweaveAsciiRuncode,
         writedoc = RweaveAsciiWritedoc,
         finish = RweaveAsciiFinish,
         checkopts = RweaveAsciiOptions)
}

##' RweaveReSTSetup
##'
##' @param file file
##' @param syntax syntax
##' @param output output
##' @param quiet quite
##' @param debug debug
##' @param stylepath stylepath
##' @param ... ...
##' @keywords internal
RweaveReSTSetup <- RweaveAsciiSetup
formals(RweaveReSTSetup) <-alist(file=, syntax=, output=NULL, quiet=FALSE, debug=FALSE,
                                 extension="rst", backend="docutils, sphinx, ...", openSchunk=".. code-block:: r\n",
                                 closeSchunk="\n\n", openSinput="", closeSinput="",
                                 openSoutput="\n", closeSoutput="", indent="  ", openInclude =".. include::",
                                 closeInclude=".rst", openFig=".. image:: ", closeFig="", ...=)
