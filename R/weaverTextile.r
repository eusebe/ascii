## Adapted from weaver package code by Seth Falcon

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

weaverTextileSetup <-
    function(file, syntax, output=NULL, quiet=FALSE, debug=FALSE,
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

weaverTextileFinish <- function(object, error=FALSE) {
    resetStorage(cache_expr)
    RweaveTextileFinish(object, error)
}


weaverTextileRuncode <- makeRweaveTextileCodeRunner(evalFunc=weaverEvalWithOpt)
