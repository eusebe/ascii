cacheRweaveReST <- function()
{
    require(cacheSweave)
    list(setup = cacheRweaveReSTSetup,
         runcode = cacheRweaveReSTRuncode,
         writedoc = RweaveReSTWritedoc,
         finish = RweaveReSTFinish,
         checkopts = RweaveReSTOptions)
}

cacheRweaveReSTSetup <-
    function(file, syntax, output=NULL, quiet=FALSE, debug=FALSE,
             stylepath, ...)
{
    dots <- list(...)
    if(is.null(output)) {
        prefix.string <- basename(sub(syntax$extension, "", file))
        output <- paste(prefix.string, "rst", sep=".")
    } else prefix.string <- basename(sub("\\.rst$", "", output))

    if(!quiet) cat("Writing to file ", output, "\n",
                   "Processing code chunks ...\n", sep="")
    output <- file(output, open="w+")

    options <- list(prefix=TRUE, prefix.string=prefix.string,
                    engine="R", print=FALSE, eval=TRUE,
                    fig=FALSE, ext = "jpg", png=FALSE, jpg=TRUE, pdf=TRUE, eps=FALSE,
                    width=6, height=6, res=100, term=TRUE,
                    echo=TRUE, keep.source=FALSE, results="verbatim",
                    split=FALSE, strip.white="true", include=TRUE,
                    pdf.version=grDevices::pdf.options()$version,
                    pdf.encoding=grDevices::pdf.options()$encoding,
                    concordance=FALSE, expand=TRUE, cache = FALSE)
    options[names(dots)] <- dots

    ## to be on the safe side: see if defaults pass the check
    options <- RweaveReSTOptions(options)

    list(output=output, 
         debug=debug, quiet=quiet, syntax = syntax,
         options=options, chunkout=list(), srclines=integer(0L),
         srcfile=srcfile(file))
}

makeCacheRweaveReSTCodeRunner <- function(evalFunc=RweaveEvalWithOpt)
{
    ## Return a function suitable as the 'runcode' element
    ## of an Sweave driver.  evalFunc will be used for the
    ## actual evaluation of chunk code.
    RweaveReSTRuncode <- function(object, chunk, options)
      {
          if(!(options$engine %in% c("R", "S"))){
              return(object)
          }

          if(!object$quiet){
              cat(formatC(options$chunknr, width=2), ":")
              if(options$echo) cat(" echo")
              if(options$keep.source) cat(" keep.source")
              if(options$eval){
                  if(options$print) cat(" print")
                  if(options$term) cat(" term")
                  cat("", options$results)
                  if(options$fig){
                      if(options$png) cat(" png")
                      if(options$jpg) cat(" jpg")
                      if(options$eps) cat(" eps")
                      if(options$pdf) cat(" pdf")
                  }
              }
              if(!is.null(options$label))
                cat(" (label=", options$label, ")", sep="")
              cat("\n")
          }

          chunkprefix <- RweaveChunkPrefix(options)

          if(options$split){
              ## [x][[1L]] avoids partial matching of x
              chunkout <- object$chunkout[chunkprefix][[1L]]
              if(is.null(chunkout)){
                  chunkout <- file(paste(chunkprefix, "rst", sep="."), "w")
                  if(!is.null(options$label))
                    object$chunkout[[chunkprefix]] <- chunkout
              }
          }
          else
            chunkout <- object$output

      saveopts <- options(keep.source=options$keep.source)
      on.exit(options(saveopts))

          SweaveHooks(options, run=TRUE)

          chunkexps <- try(parse(text=chunk), silent=TRUE)
          RweaveTryStop(chunkexps, options)
          ## Additions here [RDP] (from cacheSweave package)
          options$chunkDigest <- cacheSweave:::hashExpr(parse(text = chunk, srcfile = NULL))

          openSinput <- FALSE
          openSchunk <- FALSE

          if(length(chunkexps) == 0L)
            return(object)

          srclines <- attr(chunk, "srclines")
          linesout <- integer(0L)
          srcline <- srclines[1L]

      srcrefs <- attr(chunkexps, "srcref")
      if (options$expand)
        lastshown <- 0L
      else
        lastshown <- srcline - 1L
      thisline <- 0
          for(nce in 1L:length(chunkexps))
            {
                ce <- chunkexps[[nce]]
                if (nce <= length(srcrefs) && !is.null(srcref <- srcrefs[[nce]])) {
                    if (options$expand) {
                    srcfile <- attr(srcref, "srcfile")
                    showfrom <- srcref[1L]
                    showto <- srcref[3L]
                    } else {
                        srcfile <- object$srcfile
                        showfrom <- srclines[srcref[1L]]
                        showto <- srclines[srcref[3L]]
                    }
                    dce <- getSrcLines(srcfile, lastshown+1, showto)
                leading <- showfrom-lastshown
                lastshown <- showto
                    srcline <- srclines[srcref[3L]]
                    while (length(dce) && length(grep("^[[:blank:]]*$", dce[1L]))) {
                dce <- dce[-1L]
                leading <- leading - 1L
                }
            } else {
                    dce <- deparse(ce, width.cutoff=0.75*getOption("width"))
                    leading <- 1L
                }
                if(object$debug)
                  cat("\nRnw> ", paste(dce, collapse="\n+  "),"\n")
                if(options$echo && length(dce)){
                    if(!openSinput){
                        if(!openSchunk){
                            cat(".. code-block:: r\n\n",
                                file=chunkout, append=TRUE)
                            linesout[thisline + 1] <- srcline
                            thisline <- thisline + 1
                            openSchunk <- TRUE
                        }
                        cat("",
                            file=chunkout, append=TRUE)
                        openSinput <- TRUE
                    }
            cat("", paste(paste("  ", getOption("prompt"), sep = ""), dce[1L:leading], sep="", collapse="\n"),
                file=chunkout, append=TRUE, sep="")
                    if (length(dce) > leading)
                        cat("\n", paste("  ", paste(getOption("continue"), sep = ""), dce[-(1L:leading)], sep="", collapse="\n"),
                            file=chunkout, append=TRUE, sep="")
            linesout[thisline + 1L:length(dce)] <- srcline
            thisline <- thisline + length(dce)
                }

                                        # tmpcon <- textConnection("output", "w")
                                        # avoid the limitations (and overhead) of output text connections
                tmpcon <- file()
                sink(file=tmpcon)
                err <- NULL
                ## if(options$eval) err <- evalFunc(ce, options)

                ## [RDP] change this line to use my EvalWithOpt function (from cacheSweave package)
                if(options$eval) err <- cacheSweave:::cacheSweaveEvalWithOpt(ce, options)
                ## [RDP] end change

                cat("") # make sure final line is complete
                sink()
                output <- readLines(tmpcon)
                close(tmpcon)
                ## delete empty output
                if(length(output) == 1L & output[1L] == "") output <- NULL

                RweaveTryStop(err, options)

                if(object$debug)
                  cat(paste(output, collapse="\n"))
                  
                if (length(output) == 0 | options$results == "hide") cat("\n", file=chunkout, append=TRUE)

                if(length(output) & (options$results != "hide")){
                    addabreak <- ""

                    if(openSinput){
                        cat("\n", file=chunkout, append=TRUE)
                        linesout[thisline + 1L:2L] <- srcline
                        thisline <- thisline + 2L
                        openSinput <- FALSE
                    }
                    if(options$results=="verbatim"){
                        if(!openSchunk){
                            cat(".. code-block:: r\n\n",
                                file=chunkout, append=TRUE)
                            linesout[thisline + 1L] <- srcline
                            thisline <- thisline + 1L
                            openSchunk <- TRUE
                        }
                        cat("",
                            file=chunkout, append=TRUE)
                        linesout[thisline + 1L] <- srcline
                        thisline <- thisline + 1L
                    }
                    if(options$results=="ascii"){
                        if(openSinput){
                            cat("",
                                file=chunkout, append=TRUE)
                            linesout[thisline + 1L] <- srcline
                            thisline <- thisline + 1L
                            openSchunk <- TRUE
                        }
                        if(openSchunk){
                            cat("\n\n",
                                file=chunkout, append=TRUE)
                            linesout[thisline + 1L] <- srcline
                            thisline <- thisline + 1L
                            openSchunk <- FALSE
                        }
                        addabreak <- "\n"
                    }
                    if (options$results == "verbatim")
                      output <- paste("", output,collapse="\n", sep = "  ")
                    else
                      output <- paste(output,collapse="\n")
                    if(options$strip.white %in% c("all", "true")){
                        output <- sub("^[[:space:]]*\n", "", output)
                        output <- sub("\n[[:space:]]*$", "", output)
                        if(options$strip.white=="all")
                          output <- sub("\n[[:space:]]*\n", "\n", output)
                    }
                    cat(output,addabreak, file=chunkout, append=TRUE)
                    count <- sum(strsplit(output, NULL)[[1L]] == "\n")
                    if (count > 0L) {
                        linesout[thisline + 1L:count] <- srcline
                        thisline <- thisline + count
                    }

                    remove(output)

                    if(options$results=="verbatim"){
                        cat("\n", file=chunkout, append=TRUE)
                        linesout[thisline + 1L:2] <- srcline
                        thisline <- thisline + 2L
                    }
                }
            }

          if(openSinput){
              cat("", file=chunkout, append=TRUE)
              linesout[thisline + 1L:2L] <- srcline
              thisline <- thisline + 2L
          }

          if(openSchunk){
              cat("\n\n", file=chunkout, append=TRUE)
              linesout[thisline + 1L] <- srcline
              thisline <- thisline + 1L
          }

          if(is.null(options$label) & options$split)
            close(chunkout)

          if(options$split & options$include){
              cat(".. include::", chunkprefix, ".rst\n", sep="",
                file=object$output, append=TRUE)
              linesout[thisline + 1L] <- srcline
              thisline <- thisline + 1L
          }

          if(options$fig && options$eval){
              if(options$eps){
                  grDevices::postscript(file=paste(chunkprefix, "eps", sep="."),
                                        width=options$width, height=options$height,
                                        paper="special", horizontal=FALSE)

                  err <- try({SweaveHooks(options, run=TRUE)
                              eval(chunkexps, envir=.GlobalEnv)})
                  grDevices::dev.off()
                  if(inherits(err, "try-error")) stop(err)
              }
              if(options$pdf){
                  grDevices::pdf(file=paste(chunkprefix, "pdf", sep="."),
                                 width=options$width, height=options$height,
                                 version=options$pdf.version,
                                 encoding=options$pdf.encoding)

                  err <- try({SweaveHooks(options, run=TRUE)
                              eval(chunkexps, envir=.GlobalEnv)})
                  grDevices::dev.off()
                  if(inherits(err, "try-error")) stop(err)
              }
              if(options$png){
                  png(file=paste(chunkprefix, "png", sep="."),width=options$width,height=options$height,units="in",res=options$res)

                  err <- try({SweaveHooks(options, run=TRUE);
                        eval(chunkexps, envir=.GlobalEnv)})
                  dev.off()
                  if(inherits(err, "try-error")) stop(err)
              }
              if(options$jpg){
                  jpeg(file=paste(chunkprefix, "jpg", sep="."),width=options$width,height=options$height,quality=100,units="in",res=options$res)

                  err <- try({SweaveHooks(options, run=TRUE);
                        eval(chunkexps, envir=.GlobalEnv)})
                  dev.off()
                  if(inherits(err, "try-error")) stop(err)
              }
                            
              if(options$include) {
                  cat(".. image:: ", chunkprefix, ".", options$ext, "\n", sep="",
                      file=object$output, append=TRUE)
                  linesout[thisline + 1L] <- srcline
                  thisline <- thisline + 1L
              }
          }
          object$linesout <- c(object$linesout, linesout)
          return(object)
      }
    RweaveReSTRuncode
}

cacheRweaveReSTRuncode <- makeCacheRweaveReSTCodeRunner()
