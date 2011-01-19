## Adapted from weaver package code by Seth Falcon

##' weaverAsciidoc
##' 
##' @export
weaverAsciidoc <- function()
{
    require(weaver)
    list(setup = weaverAsciidocSetup,
         runcode = weaverAsciidocRuncode,
         writedoc = RweaveAsciidocWritedoc,
         finish = weaverAsciidocFinish,
         checkopts = RweaveAsciidocOptions)
}

##' weaverAsciidocSetup
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
weaverAsciidocSetup <- function(file, syntax, output=NULL, quiet=FALSE, debug=FALSE,
             stylepath, use.cache=TRUE, ...)
{
    if (!quiet)
      cat("Working dir:", getwd(), "\n")
    log_debug(paste("Working dir:", getwd()))
    res <- RweaveAsciidocSetup(file, syntax, output, quiet, debug, stylepath, use.cache, ...)
    res$options[["use.cache"]] <- use.cache
    res$options[["cache"]] <- FALSE
    ## to be on the safe side: see if defaults pass the check
    res$options <- RweaveAsciidocOptions(res$options)
    res
}
##' weaverAsciidocFinish
##'
##' @param object object
##' @param error error
##' @keywords internal
weaverAsciidocFinish <- function(object, error=FALSE) {
    resetStorage(cache_expr)
    RweaveAsciidocFinish(object, error)
}


weaverAsciidocRuncode <- makeRweaveAsciidocCodeRunner(evalFunc=weaverEvalWithOpt)
