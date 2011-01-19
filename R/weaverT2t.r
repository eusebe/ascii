## Adapted from weaver package code by Seth Falcon


##' weaverT2t
##'
##' @export
weaverT2t <- function()
{
    require(weaver)
    list(setup = weaverT2tSetup,
         runcode = weaverT2tRuncode,
         writedoc = RweaveT2tWritedoc,
         finish = weaverT2tFinish,
         checkopts = RweaveT2tOptions)
}

##' weaverT2tSetup
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
weaverT2tSetup <- function(file, syntax, output=NULL, quiet=FALSE, debug=FALSE,
             stylepath, use.cache=TRUE, ...)
{
    if (!quiet)
      cat("Working dir:", getwd(), "\n")
    log_debug(paste("Working dir:", getwd()))
    res <- RweaveT2tSetup(file, syntax, output, quiet, debug, stylepath, use.cache, ...)
    res$options[["use.cache"]] <- use.cache
    res$options[["cache"]] <- FALSE
    ## to be on the safe side: see if defaults pass the check
    res$options <- RweaveT2tOptions(res$options)
    res
}

##' weaverT2tFinish
##'
##' @param object object
##' @param error error
##' @keywords internal
weaverT2tFinish <- function(object, error=FALSE) {
    resetStorage(cache_expr)
    RweaveT2tFinish(object, error)
}


weaverT2tRuncode <- makeRweaveT2tCodeRunner(evalFunc=weaverEvalWithOpt)
