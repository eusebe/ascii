##' A driver to parse ascii noweb files with Sweave tool - cacheSweave based
##'
##' @keywords internal
##' @author David Hajage
cacheSweaveAsciiSetup <- function(..., cache = FALSE, trace = FALSE, dependson = NULL) {
  out <- RweaveAsciiSetup(...)

  ## Add the (non-standard) options for code chunks with caching
  out$options[["cache"]] <- cache
	out$options[["dependson"]] <- dependson
	out$options[["trace"]] <- trace
  out
}

##' A driver to parse ascii noweb files with Sweave tool - cacheSweave based
##'
##' @keywords internal
##' @author David Hajage
makeCacheSweaveAsciiCodeRunner <- function(evalFunc = cacheSweave:::cacheSweaveEvalWithOpt) {
  runner <- makeRweaveAsciiCodeRunner(evalFunc)
	function(object, chunk, options) {
		updatedChunk <- FALSE
		e <- runner(object, chunk, options)
		flag <- 'L'
		if(updatedChunk) {
			chunkName <- cacheSweave:::metaChunkName(options)
			cacheSweave:::metaSetCreationTime(chunkName)
			flag <- 'S'
		}
		n <- nchar(as.character(options$chunknr))
    # overwrites the : on preceding row with flag using ANSI
		cat("[F[",n+2,"C",flag,"\n",sep='')
		e
	}
}

##' A driver to parse ascii noweb files with Sweave tool - cacheSweave based
##'
##' @author David Hajage
cacheSweaveAsciiRuncode <- makeCacheSweaveAsciiCodeRunner()

cacheSweaveAsciiOptions <- function(options) {
	moreoptions <- c('dependson','cache')
	oldoptions <- options[setdiff(names(options),moreoptions)]
	newoptions <- options[intersect(names(options),moreoptions)]
	Rweaveoptions <- RweaveAsciiOptions(oldoptions)
	options <- unlist(list(Rweaveoptions,newoptions),recursive=FALSE)
  options
}

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
         checkopts = cacheSweaveAsciiOptions)
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
         checkopts = cacheSweaveAsciiOptions)
}

##' A driver to parse txt2tags noweb files with Sweave tool - cacheSweave based
##'
##' @keywords internal
##' @author David Hajage
cacheSweaveT2tSetup <- function(..., cache = FALSE, trace = FALSE, dependson = NULL) {
  out <- RweaveT2tSetup(...)

  ## Add the (non-standard) options for code chunks with caching
  out$options[["cache"]] <- cache
	out$options[["dependson"]] <- dependson
	out$options[["trace"]] <- trace
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
         checkopts = cacheSweaveAsciiOptions)
}

##' A driver to parse org noweb files with Sweave tool - cacheSweave based
##'
##' @keywords internal
##' @author David Hajage
cacheSweaveOrgSetup <- function(..., cache = FALSE, trace = FALSE, dependson = NULL) {
  out <- RweaveOrgSetup(...)

  ## Add the (non-standard) options for code chunks with caching
  out$options[["cache"]] <- cache
	out$options[["dependson"]] <- dependson
	out$options[["trace"]] <- trace
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
         checkopts = cacheSweaveAsciiOptions)
}

##' A driver to parse pandoc noweb files with Sweave tool - cacheSweave based
##'
##' @keywords internal
##' @author David Hajage
cacheSweavePandocSetup <- function(..., cache = FALSE, trace = FALSE, dependson = NULL) {
  out <- RweavePandocSetup(...)

  ## Add the (non-standard) options for code chunks with caching
  out$options[["cache"]] <- cache
	out$options[["dependson"]] <- dependson
	out$options[["trace"]] <- trace
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
         checkopts = cacheSweaveAsciiOptions)
}

##' A driver to parse textile noweb files with Sweave tool - cacheSweave based
##'
##' @keywords internal
##' @author David Hajage
cacheSweaveTextileSetup <- function(..., cache = FALSE, trace = FALSE, dependson = NULL) {
  out <- RweaveTextileSetup(...)

  ## Add the (non-standard) options for code chunks with caching
  out$options[["cache"]] <- cache
	out$options[["dependson"]] <- dependson
	out$options[["trace"]] <- trace
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
         checkopts = cacheSweaveAsciiOptions)
}

##' A driver to parse rest noweb files with Sweave tool - cacheSweave based
##'
##' @keywords internal
##' @author David Hajage
cacheSweaveReSTSetup <- function(..., cache = FALSE, trace = FALSE, dependson = NULL) {
  out <- RweaveReSTSetup(...)

  ## Add the (non-standard) options for code chunks with caching
  out$options[["cache"]] <- cache
	out$options[["dependson"]] <- dependson
	out$options[["trace"]] <- trace
  out
}
