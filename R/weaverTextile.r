## Adapted from weaver package code by Seth Falcon


##' weaverTextile
##'
##' @export
weaverTextile <- function()
{
    require(weaver)
    list(setup = weaverTextileSetup,
         runcode = weaverTextileRuncode,
         writedoc = RweaveTextileWritedoc,
         finish = weaverTextileFinish,
         checkopts = RweaveTextileOptions)
}

##' weaverTexxtileSetup
##'
##' @param file file
##' @param syntax syntax
##' @param output output
##' @param quiet quiet
##' @param debug debug
##' @param stylepath stylepath
##' @param use.cache use.cache
##' @param ... ...
##' @keywords internal
weaverTextileSetup <- function(file, syntax, output=NULL, quiet=FALSE, debug=FALSE,
             stylepath, use.cache=TRUE, ...)
{
    if (!quiet)
      cat("Working dir:", getwd(), "\n")
    log_debug(paste("Working dir:", getwd()))
    res <- RweaveTextileSetup(file, syntax, output, quiet, debug, stylepath, use.cache, ...)
    res$options[["use.cache"]] <- use.cache
    res$options[["cache"]] <- FALSE
    ## to be on the safe side: see if defaults pass the check
    res$options <- RweaveTextileOptions(res$options)
    res
}

##' weaverTexxtileFinish
##'
##' @param object object
##' @param error error
##' @keywords internal
weaverTextileFinish <- function(object, error=FALSE) {
    resetStorage(cache_expr)
    RweaveTextileFinish(object, error)
}


weaverTextileRuncode <- makeRweaveTextileCodeRunner(evalFunc=weaverEvalWithOpt)
