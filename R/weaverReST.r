## Adapted from weaver package code by Seth Falcon

##' @export
weaverReST <- function()
{
    require(weaver)
    list(setup = weaverReSTSetup,
         runcode = weaverReSTRuncode,
         writedoc = RweaveReSTWritedoc,
         finish = weaverReSTFinish,
         checkopts = RweaveReSTOptions)
}

weaverReSTSetup <-
    function(file, syntax, output=NULL, quiet=FALSE, debug=FALSE,
             stylepath, use.cache=TRUE, ...)
{
    if (!quiet)
      cat("Working dir:", getwd(), "\n")
    log_debug(paste("Working dir:", getwd()))
    res <- RweaveReSTSetup(file, syntax, output, quiet, debug, stylepath, use.cache, ...)
    res$options[["use.cache"]] <- use.cache
    res$options[["cache"]] <- FALSE
    ## to be on the safe side: see if defaults pass the check
    res$options <- RweaveReSTOptions(res$options)
    res
}

weaverReSTFinish <- function(object, error=FALSE) {
    resetStorage(cache_expr)
    RweaveReSTFinish(object, error)
}


weaverReSTRuncode <- makeRweaveReSTCodeRunner(evalFunc=weaverEvalWithOpt)
