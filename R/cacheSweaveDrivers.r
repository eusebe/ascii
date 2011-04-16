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
         checkopts = RweaveAsciiOptions)
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
         checkopts = RweaveAsciiOptions)
}

##' A driver to parse txt2tags noweb files with Sweave tool - cacheSweave based
##'
##' @keywords internal
##' @author David Hajage
cacheSweaveT2tSetup <- function(..., cache = FALSE) {
        out <- RweaveT2tSetup(...)
        out$options[["cache"]] <- cache
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
         checkopts = RweaveAsciiOptions)
}

##' A driver to parse org noweb files with Sweave tool - cacheSweave based
##'
##' @keywords internal
##' @author David Hajage
cacheSweaveOrgSetup <- function(..., cache = FALSE) {
        out <- RweaveOrgSetup(...)
        out$options[["cache"]] <- cache
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
         checkopts = RweaveAsciiOptions)
}

##' A driver to parse pandoc noweb files with Sweave tool - cacheSweave based
##'
##' @keywords internal
##' @author David Hajage
cacheSweavePandocSetup <- function(..., cache = FALSE) {
        out <- RweavePandocSetup(...)
        out$options[["cache"]] <- cache
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
         checkopts = RweaveAsciiOptions)
}

##' A driver to parse textile noweb files with Sweave tool - cacheSweave based
##'
##' @keywords internal
##' @author David Hajage
cacheSweaveTextileSetup <- function(..., cache = FALSE) {
        out <- RweaveTextileSetup(...)
        out$options[["cache"]] <- cache
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
         checkopts = RweaveAsciiOptions)
}

##' A driver to parse rest noweb files with Sweave tool - cacheSweave based
##'
##' @keywords internal
##' @author David Hajage
cacheSweaveReSTSetup <- function(..., cache = FALSE) {
        out <- RweaveReSTSetup(...)
        out$options[["cache"]] <- cache
        out
}
