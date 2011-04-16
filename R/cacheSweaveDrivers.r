## Asciidoc
cacheSweaveAsciidoc <- function()
{
    require(cacheSweave)
    list(setup = cacheSweaveAsciiSetup,
         runcode = cacheSweaveAsciiRuncode,
         writedoc = RweaveAsciiWritedoc,
         finish = RweaveAsciiFinish,
         checkopts = RweaveAsciiOptions)
}

# Txt2tags
cacheSweaveT2t <- function()
{
    list(setup = cacheSweaveT2tSetup,
         runcode = cacheSweaveAsciiRuncode,
         writedoc = RweaveAsciiWritedoc,
         finish = RweaveAsciiFinish,
         checkopts = RweaveAsciiOptions)
}

cacheSweaveT2tSetup <- function(..., cache = FALSE) {
        out <- RweaveT2tSetup(...)
        out$options[["cache"]] <- cache
        out
}

# Org
cacheSweaveOrg <- function()
{
    list(setup = cacheSweaveOrgSetup,
         runcode = cacheSweaveAsciiRuncode,
         writedoc = RweaveAsciiWritedoc,
         finish = RweaveAsciiFinish,
         checkopts = RweaveAsciiOptions)
}

cacheSweaveOrgSetup <- function(..., cache = FALSE) {
        out <- RweaveOrgSetup(...)
        out$options[["cache"]] <- cache
        out
}

# Pandoc
cacheSweavePandoc <- function()
{
    list(setup = cacheSweavePandocSetup,
         runcode = cacheSweaveAsciiRuncode,
         writedoc = RweaveAsciiWritedoc,
         finish = RweaveAsciiFinish,
         checkopts = RweaveAsciiOptions)
}

cacheSweavePandocSetup <- function(..., cache = FALSE) {
        out <- RweavePandocSetup(...)
        out$options[["cache"]] <- cache
        out
}

# Textile
cacheSweaveTextile <- function()
{
    list(setup = cacheSweaveTextileSetup,
         runcode = cacheSweaveAsciiRuncode,
         writedoc = RweaveAsciiWritedoc,
         finish = RweaveAsciiFinish,
         checkopts = RweaveAsciiOptions)
}

cacheSweaveTextileSetup <- function(..., cache = FALSE) {
        out <- RweaveTextileSetup(...)
        out$options[["cache"]] <- cache
        out
}

# ReSTructuredText
cacheSweaveReST <- function()
{
    list(setup = cacheSweaveReSTSetup,
         runcode = cacheSweaveAsciiRuncode,
         writedoc = RweaveAsciiWritedoc,
         finish = RweaveAsciiFinish,
         checkopts = RweaveAsciiOptions)
}

cacheSweaveReSTSetup <- function(..., cache = FALSE) {
        out <- RweaveReSTSetup(...)
        out$options[["cache"]] <- cache
        out
}
