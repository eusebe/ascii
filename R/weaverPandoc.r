## Adapted from weaver package code by Seth Falcon

##' weaverPandoc
##' 
##' @export
weaverPandoc <- function()
{
    require(weaver)
    list(setup = weaverPandocSetup,
         runcode = weaverPandocRuncode,
         writedoc = RweavePandocWritedoc,
         finish = weaverPandocFinish,
         checkopts = RweavePandocOptions)
}

##' weaverPandocSetup
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
weaverPandocSetup <- function(file, syntax, output=NULL, quiet=FALSE, debug=FALSE,
             stylepath, use.cache=TRUE, ...)
{
    if (!quiet)
      cat("Working dir:", getwd(), "\n")
    log_debug(paste("Working dir:", getwd()))
    res <- RweavePandocSetup(file, syntax, output, quiet, debug, stylepath, use.cache, ...)
    res$options[["use.cache"]] <- use.cache
    res$options[["cache"]] <- FALSE
    ## to be on the safe side: see if defaults pass the check
    res$options <- RweavePandocOptions(res$options)
    res
}
##' weaverPandocFinish
##'
##' @param object object
##' @param error error
##' @keywords internal
weaverPandocFinish <- function(object, error=FALSE) {
    resetStorage(cache_expr)
    RweavePandocFinish(object, error)
}


weaverPandocRuncode <- makeRweavePandocCodeRunner(evalFunc=weaverEvalWithOpt)
