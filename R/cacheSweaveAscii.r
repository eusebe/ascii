##' A driver to parse ascii noweb files with Sweave tool - cacheSweave based
##'
##' @keywords internal 
##' @author David Hajage
cacheSweaveAsciiSetup <- function(..., cache = FALSE) {
        out <- RweaveAsciiSetup(...)
        out$options[["cache"]] <- cache
        out
}

##' A driver to parse ascii noweb files with Sweave tool - cacheSweave based
##'
##' @keywords internal 
##' @author David Hajage
makeCacheSweaveAsciiCodeRunner <- function(evalFunc = cacheSweave:::cacheSweaveEvalWithOpt)
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

        ## Additions here [RDP]
        options$chunkDigest <- cacheSweave:::hashExpr(parse(text = chunk, srcfile = NULL))
        ## [RDP]

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
            if (options$results == "ascii")
              cat("\n", file = chunkout)
            else
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
                cat(options$openFig, chunkprefix, ".", options$ext, options$closeFig, "\n\n", sep = "",
                    file = object$output)
                linesout[thisline + 1L] <- srcline
                thisline <- thisline + 1L
            }
        }
        object$linesout <- c(object$linesout, linesout)
        object
    }
}

##' A driver to parse ascii noweb files with Sweave tool - cacheSweave based
##'
##' @author David Hajage
cacheSweaveAsciiRuncode <- makeCacheSweaveAsciiCodeRunner()

###############################################

##' A driver to parse asciidoc noweb files with Sweave tool - cacheSweave based
##'
##' @export
##' @author David Hajage
cacheSweaveAsciidoc <- function()
{
    require(cacheSweave)
    list(setup = cacheSweaveAsciiSetup,
         runcode = cacheSweaveAsciiRuncode,
         writedoc = RweaveAsciiWritedoc,
         finish = RweaveAsciiFinish,
         checkopts = RweaveAsciiOptions)
}

##' A driver to parse txt2tags noweb files with Sweave tool - cacheSweave based
##'
##' @export
##' @author David Hajage
cacheSweaveT2t <- function()
{
    list(setup = cacheSweaveT2tSetup,
         runcode = cacheSweaveAsciiRuncode,
         writedoc = RweaveAsciiWritedoc,
         finish = RweaveAsciiFinish,
         checkopts = RweaveAsciiOptions)
}

##' A driver to parse txt2tags noweb files with Sweave tool - cacheSweave based
##'
##' @keywords internal
##' @author David Hajage
cacheSweaveT2tSetup <- function(..., cache = FALSE) {
        out <- RweaveT2tSetup(...)
        out$options[["cache"]] <- cache
        out
}

##' A driver to parse org noweb files with Sweave tool - cacheSweave based
##'
##' @export
##' @author David Hajage
cacheSweaveOrg <- function()
{
    list(setup = cacheSweaveOrgSetup,
         runcode = cacheSweaveAsciiRuncode,
         writedoc = RweaveAsciiWritedoc,
         finish = RweaveAsciiFinish,
         checkopts = RweaveAsciiOptions)
}

##' A driver to parse org noweb files with Sweave tool - cacheSweave based
##'
##' @keywords internal
##' @author David Hajage
cacheSweaveOrgSetup <- function(..., cache = FALSE) {
        out <- RweaveOrgSetup(...)
        out$options[["cache"]] <- cache
        out
}

##' A driver to parse pandoc noweb files with Sweave tool - cacheSweave based
##'
##' @export
##' @author David Hajage
cacheSweavePandoc <- function()
{
    list(setup = cacheSweavePandocSetup,
         runcode = cacheSweaveAsciiRuncode,
         writedoc = RweaveAsciiWritedoc,
         finish = RweaveAsciiFinish,
         checkopts = RweaveAsciiOptions)
}

##' A driver to parse pandoc noweb files with Sweave tool - cacheSweave based
##'
##' @keywords internal
##' @author David Hajage
cacheSweavePandocSetup <- function(..., cache = FALSE) {
        out <- RweavePandocSetup(...)
        out$options[["cache"]] <- cache
        out
}

##' A driver to parse textile noweb files with Sweave tool - cacheSweave based
##'
##' @export
##' @author David Hajage
cacheSweaveTextile <- function()
{
    list(setup = cacheSweaveTextileSetup,
         runcode = cacheSweaveAsciiRuncode,
         writedoc = RweaveAsciiWritedoc,
         finish = RweaveAsciiFinish,
         checkopts = RweaveAsciiOptions)
}

##' A driver to parse textile noweb files with Sweave tool - cacheSweave based
##'
##' @keywords internal
##' @author David Hajage
cacheSweaveTextileSetup <- function(..., cache = FALSE) {
        out <- RweaveTextileSetup(...)
        out$options[["cache"]] <- cache
        out
}

##' A driver to parse rest noweb files with Sweave tool - cacheSweave based
##'
##' @export
##' @author David Hajage
cacheSweaveReST <- function()
{
    list(setup = cacheSweaveReSTSetup,
         runcode = cacheSweaveAsciiRuncode,
         writedoc = RweaveAsciiWritedoc,
         finish = RweaveAsciiFinish,
         checkopts = RweaveAsciiOptions)
}

##' A driver to parse rest noweb files with Sweave tool - cacheSweave based
##'
##' @keywords internal
##' @author David Hajage
cacheSweaveReSTSetup <- function(..., cache = FALSE) {
        out <- RweaveReSTSetup(...)
        out$options[["cache"]] <- cache
        out
}
