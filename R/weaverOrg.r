## Adapted from weaver package code by Seth Falcon

##' weaverOrg
##' 
##' @export
weaverOrg <- function()
{
    require(weaver)
    list(setup = weaverOrgSetup,
         runcode = weaverOrgRuncode,
         writedoc = RweaveOrgWritedoc,
         finish = weaverOrgFinish,
         checkopts = RweaveOrgOptions)
}

##' weaverOrgSetup
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
weaverOrgSetup <- function(file, syntax, output=NULL, quiet=FALSE, debug=FALSE,
             stylepath, use.cache=TRUE, ...)
{
    if (!quiet)
      cat("Working dir:", getwd(), "\n")
    log_debug(paste("Working dir:", getwd()))
    res <- RweaveOrgSetup(file, syntax, output, quiet, debug, stylepath, use.cache, ...)
    res$options[["use.cache"]] <- use.cache
    res$options[["cache"]] <- FALSE
    ## to be on the safe side: see if defaults pass the check
    res$options <- RweaveOrgOptions(res$options)
    res
}

##' weaverOrgFinish
##'
##' @param object object
##' @param error error
##' @keywords internal
weaverOrgFinish <- function(object, error=FALSE) {
    resetStorage(cache_expr)
    RweaveOrgFinish(object, error)
}


weaverOrgRuncode <- makeRweaveOrgCodeRunner(evalFunc=weaverEvalWithOpt)
