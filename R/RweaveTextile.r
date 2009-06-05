RweaveTextile <- function()
{
    list(setup = RweaveTextileSetup,
         runcode = RweaveTextileRuncode,
         writedoc = RweaveTextileWritedoc,
         finish = RweaveTextileFinish,
         checkopts = RweaveTextileOptions)
}

RweaveTextileSetup <-
    function(file, syntax, output=NULL, quiet=FALSE, debug=FALSE,
             stylepath, ...)
{
    dots <- list(...)
    if(is.null(output)) {
        prefix.string <- basename(sub(syntax$extension, "", file))
        output <- paste(prefix.string, "txt", sep=".")
    } else prefix.string <- basename(sub("\\.txt$", "", output))

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
                    concordance=FALSE, expand=TRUE)
    options[names(dots)] <- dots

    ## to be on the safe side: see if defaults pass the check
    options <- RweaveTextileOptions(options)

    list(output=output, 
         debug=debug, quiet=quiet, syntax = syntax,
         options=options, chunkout=list(), srclines=integer(0L),
         srcfile=srcfile(file))
}

makeRweaveTextileCodeRunner <- function(evalFunc=RweaveEvalWithOpt)
{
    ## Return a function suitable as the 'runcode' element
    ## of an Sweave driver.  evalFunc will be used for the
    ## actual evaluation of chunk code.
    RweaveTextileRuncode <- function(object, chunk, options)
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
                  chunkout <- file(paste(chunkprefix, "txt", sep="."), "w")
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
                            cat("<pre>\n",
                                file=chunkout, append=TRUE)
                            linesout[thisline + 1] <- srcline
                            thisline <- thisline + 1
                            openSchunk <- TRUE
                        }
                        cat("",
                            file=chunkout, append=TRUE)
                        openSinput <- TRUE
                    }
            cat("", paste(getOption("prompt"), dce[1L:leading], sep="", collapse="\n"),
                file=chunkout, append=TRUE, sep="")
                    if (length(dce) > leading)
                        cat("\n", paste(getOption("continue"), dce[-(1L:leading)], sep="", collapse="\n"),
                            file=chunkout, append=TRUE, sep="")
            linesout[thisline + 1L:length(dce)] <- srcline
            thisline <- thisline + length(dce)
                }

                                        # tmpcon <- textConnection("output", "w")
                                        # avoid the limitations (and overhead) of output text connections
                tmpcon <- file()
                sink(file=tmpcon)
                err <- NULL
                if(options$eval) err <- evalFunc(ce, options)
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
                            cat("<pre>\n",
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
                            cat("</pre>\n",
                                file=chunkout, append=TRUE)
                            linesout[thisline + 1L] <- srcline
                            thisline <- thisline + 1L
                            openSchunk <- FALSE
                        }
                        addabreak <- "\n"
                    }
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
              cat("\n", file=chunkout, append=TRUE)
              linesout[thisline + 1L:2L] <- srcline
              thisline <- thisline + 2L
          }

          if(openSchunk){
              cat("</pre>\n", file=chunkout, append=TRUE)
              linesout[thisline + 1L] <- srcline
              thisline <- thisline + 1L
          }

          if(is.null(options$label) & options$split)
            close(chunkout)

          if(options$split & options$include){
              cat("include::", chunkprefix, "\n", sep="",
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
                  cat(":", chunkprefix, ".", options$ext, "!\n", sep="",
                      file=object$output, append=TRUE)
                  linesout[thisline + 1L] <- srcline
                  thisline <- thisline + 1L
              }
          }
          object$linesout <- c(object$linesout, linesout)
          return(object)
      }
    RweaveTextileRuncode
}

RweaveTextileRuncode <- makeRweaveTextileCodeRunner()

RweaveTextileWritedoc <- function(object, chunk)
{
    linesout <- attr(chunk, "srclines")

    while(length(pos <- grep(object$syntax$docexpr, chunk)))
    {
        cmdloc <- regexpr(object$syntax$docexpr, chunk[pos[1L]])
        cmd <- substr(chunk[pos[1L]], cmdloc,
                      cmdloc+attr(cmdloc, "match.length")-1L)
        cmd <- sub(object$syntax$docexpr, "\\1", cmd)
        if(object$options$eval){
            val <- as.character(eval(parse(text=cmd), envir=.GlobalEnv))
            ## protect against character(0L), because sub() will fail
            if(length(val) == 0L) val <- ""
        }
        else
            val <- paste("\\\\verb{<<", cmd, ">>{", sep="") # Mais qu'est-ce ??

        chunk[pos[1L]] <- sub(object$syntax$docexpr, val, chunk[pos[1L]])
    }
    while(length(pos <- grep(object$syntax$docopt, chunk)))
    {
        opts <- sub(paste(".*", object$syntax$docopt, ".*", sep=""),
                    "\\1", chunk[pos[1L]])
        object$options <- utils:::SweaveParseOptions(opts, object$options,
                                             RweaveTextileOptions)
            chunk[pos[1L]] <- sub(object$syntax$docopt, "", chunk[pos[1L]])
    }

    cat(chunk, sep="\n", file=object$output, append=TRUE)
    object$linesout <- c(object$linesout, linesout)

    return(object)
}

RweaveTextileFinish <- function(object, error=FALSE)
{
    outputname <- summary(object$output)$description
    inputname <- object$srcfile$filename
    if(!object$quiet && !error)
        cat("\n",
            gettextf("You can now run textile on '%s'", outputname),
            "\n", sep = "")
    close(object$output)
    if(length(object$chunkout))
        for(con in object$chunkout) close(con)
#    if (object$haveconcordance) {
        # This output format is subject to change.  Currently it contains
        # three parts, separated by colons:
        # 1.  The output .tex filename
        # 2.  The input .Rnw filename
        # 3.  The input line numbers corresponding to each output line.
        #     This are compressed using the following simple scheme:
        #     The first line number, followed by
        #     a run-length encoded diff of the rest of the line numbers.
#        linesout <- object$linesout
#        vals <- rle(diff(linesout))
#        vals <- c(linesout[1L], as.numeric(rbind(vals$lengths, vals$values)))
#        concordance <- paste(strwrap(paste(vals, collapse=" ")), collapse=" %\n")
#        special <- paste("\\Sconcordance{concordance:", outputname, ":", inputname, ":%\n",
#                 concordance,"}\n", sep="")
#        cat(special, file=object$concordfile)
#    }
    invisible(outputname)
}

RweaveTextileOptions <- function(options)
{

    ## ATTENTION: Changes in this function have to be reflected in the
    ## defaults in the init function!

    ## convert a character string to logical
    c2l <- function(x){
        if(is.null(x)) return(FALSE)
        else return(as.logical(toupper(as.character(x))))
    }

    NUMOPTS <- c("width", "height", "res")
    NOLOGOPTS <- c(NUMOPTS, "ext", "results", "prefix.string",
                   "engine", "label", "strip.white",
                   "pdf.version", "pdf.encoding", "pointsize")

    for(opt in names(options)){
        if(! (opt %in% NOLOGOPTS)){
            oldval <- options[[opt]]
            if(!is.logical(options[[opt]])){
                options[[opt]] <- c2l(options[[opt]])
            }
            if(is.na(options[[opt]]))
                stop(gettextf("invalid value for '%s' : %s", opt, oldval),
                     domain = NA)
        }
        else if(opt %in% NUMOPTS){
            options[[opt]] <- as.numeric(options[[opt]])
        }
    }

    if(!is.null(options$results))
        options$results <- tolower(as.character(options$results))
    options$results <- match.arg(options$results,
                                 c("verbatim", "ascii", "hide"))

    if(!is.null(options$strip.white))
        options$strip.white <- tolower(as.character(options$strip.white))
    options$strip.white <- match.arg(options$strip.white,
                                     c("true", "false", "all"))

    options
}


RweaveChunkPrefix <- function(options)
{
    if(!is.null(options$label)){
        if(options$prefix)
            chunkprefix <- paste(options$prefix.string, "-",
                                 options$label, sep="")
        else
            chunkprefix <- options$label
    }
    else
        chunkprefix <- paste(options$prefix.string, "-",
                             formatC(options$chunknr, flag="0", width=3),
                             sep="")

    return(chunkprefix)
}

RweaveEvalWithOpt <- function (expr, options){
    if(options$eval){
        res <- try(.Internal(eval.with.vis(expr, .GlobalEnv, baseenv())),
                   silent=TRUE)
        if(inherits(res, "try-error")) return(res)
        if(options$print | (options$term & res$visible))
            print(res$value)
    }
    return(res)
}


RweaveTryStop <- function(err, options){

    if(inherits(err, "try-error")){
        cat("\n")
        msg <- paste(" chunk", options$chunknr)
        if(!is.null(options$label))
            msg <- paste(msg, " (label=", options$label, ")", sep="")
        msg <- paste(msg, "\n")
        stop(msg, err, call.=FALSE)
    }
}





###**********************************************************

Stangle <- function(file, driver=Rtangle(),
                    syntax=getOption("SweaveSyntax"), ...)
{
    Sweave(file=file, driver=driver, ...)
}

RtangleTextile <-  function()
{
    list(setup = RtangleTextileSetup,
         runcode = RtangleTextileRuncode,
         writedoc = RtangleTextileWritedoc,
         finish = RtangleTextileFinish,
         checkopts = RweaveTextileOptions)
}


RtangleTextileSetup <- function(file, syntax,
                         output=NULL, annotate=TRUE, split=FALSE,
                         prefix=TRUE, quiet=FALSE)
{
    if(is.null(output)){
        prefix.string <- basename(sub(syntax$extension, "", file))
        output <- paste(prefix.string, "R", sep=".")
    }
    else{
        prefix.string <- basename(sub("\\.[rsRS]$", "", output))
    }

    if(!split){
        if(!quiet)
            cat("Writing to file", output, "\n")
        output <- file(output, open="w")
    }
    else{
        if(!quiet)
            cat("Writing chunks to files ...\n")
        output <- NULL
    }

    options <- list(split=split, prefix=prefix,
                    prefix.string=prefix.string,
                    engine="R", eval=TRUE)

    list(output=output, annotate=annotate, options=options,
         chunkout=list(), quiet=quiet, syntax=syntax)
}


RtangleTextileRuncode <-  function(object, chunk, options)
{
    if(!(options$engine %in% c("R", "S"))){
        return(object)
    }

    chunkprefix <- RweaveChunkPrefix(options)

    if(options$split){
        outfile <- paste(chunkprefix, options$engine, sep=".")
        if(!object$quiet)
            cat(options$chunknr, ":", outfile,"\n")
        ## [x][[1L]] avoids partial matching of x
        chunkout <- object$chunkout[chunkprefix][[1L]]
        if(is.null(chunkout)){
            chunkout <- file(outfile, "w")
            if(!is.null(options$label))
                object$chunkout[[chunkprefix]] <- chunkout
        }
    }
    else
        chunkout <- object$output

    if(object$annotate){
        cat("###################################################\n",
            "### chunk number ", options$chunknr,
            ": ", options$label,
            ifelse(options$eval, "", " eval=FALSE"), "\n",
            "###################################################\n",
            file=chunkout, append=TRUE, sep="")
    }

    hooks <- SweaveHooks(options, run=FALSE)
    for(k in hooks)
        cat("getOption(\"SweaveHooks\")[[\"", k, "\"]]()\n",
            file=chunkout, append=TRUE, sep="")

    if(!options$eval)
        chunk <- paste("##", chunk)

    cat(chunk,"\n", file=chunkout, append=TRUE, sep="\n")

    if(is.null(options$label) & options$split)
        close(chunkout)

    return(object)
}

RtangleTextileWritedoc <- function(object, chunk)
{
    while(length(pos <- grep(object$syntax$docopt, chunk)))
    {
        opts <- sub(paste(".*", object$syntax$docopt, ".*", sep=""),
                    "\\1", chunk[pos[1L]])
        object$options <- utils:::SweaveParseOptions(opts, object$options,
                                             RweaveTextileOptions)
        chunk[pos[1L]] <- sub(object$syntax$docopt, "", chunk[pos[1L]])
    }
    return(object)
}


RtangleTextileFinish <- function(object, error=FALSE)
{
    if(!is.null(object$output))
        close(object$output)

    if(length(object$chunkout)) {
        for(con in object$chunkout) close(con)
    }
}



