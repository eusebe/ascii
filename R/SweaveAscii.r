

##' Sweave wrapper for asciidoc
##' Like \code{Sweave}, but use \code{RweaveAsciidoc} driver as default value.
##' 
##' 
##' @param file Name of Sweave source file.
##' @param driver Sweave driver
##' @param syntax Sweave syntax
##' @param ... Further arguments passed to the driver's setup function.
##' @author David Hajage \email{dhajage@@gmail.com}
##' @seealso \code{\link{Sweave}}
##' @export
##' @import utils
##' @keywords IO file
Asciidoc <- Sweave
formals(Asciidoc) <-  alist(file=, driver=RweaveAsciidoc, syntax=SweaveSyntaxNoweb, ...=)



##' Sweave wrapper for txt2tags
##' Like \code{Sweave}, but use \code{RweaveT2t} driver as default value.
##' 
##' 
##' @param file Name of Sweave source file.
##' @param driver Sweave driver
##' @param syntax Sweave syntax
##' @param ... Further arguments passed to the driver's setup function.
##' @author David Hajage \email{dhajage@@gmail.com}
##' @seealso \code{\link{Sweave}}
##' @export
##' @import utils
##' @keywords IO file
T2t <- Sweave
formals(T2t) <-  alist(file=, driver=RweaveT2t, syntax=SweaveSyntaxNoweb, ...=)



##' Sweave wrapper for reStructuredText
##' Like \code{Sweave}, but use \code{RweaveReST} driver as default value.
##' 
##' 
##' @param file Name of Sweave source file.
##' @param driver Sweave driver
##' @param syntax Sweave syntax
##' @param ... Further arguments passed to the driver's setup function.
##' @author David Hajage \email{dhajage@@gmail.com}
##' @seealso \code{\link{Sweave}}
##' @export
##' @import utils
##' @keywords IO file
ReST <- Sweave
formals(ReST) <-  alist(file=, driver=RweaveReST, syntax=SweaveSyntaxNoweb, ...=)



##' Sweave wrapper for org
##' Like \code{Sweave}, but use \code{RweaveOrg} driver as default value.
##' 
##' 
##' @param file Name of Sweave source file.
##' @param driver Sweave driver
##' @param syntax Sweave syntax
##' @param ... Further arguments passed to the driver's setup function.
##' @author David Hajage \email{dhajage@@gmail.com}
##' @seealso \code{\link{Sweave}}
##' @export
##' @import utils
##' @keywords IO file
Org <- Sweave
formals(Org) <-  alist(file=, driver=RweaveOrg, syntax=SweaveSyntaxNoweb, ...=)



##' Sweave wrapper for textile
##' Like \code{Sweave}, but use \code{RweaveTextile} driver as default value.
##' 
##' 
##' @param file Name of Sweave source file.
##' @param driver Sweave driver
##' @param syntax Sweave syntax
##' @param ... Further arguments passed to the driver's setup function.
##' @author David Hajage \email{dhajage@@gmail.com}
##' @seealso \code{\link{Sweave}}
##' @export
##' @import utils
##' @keywords IO file
Textile <- Sweave
formals(Textile) <-  alist(file=, driver=RweaveTextile, syntax=SweaveSyntaxNoweb, ...=)



##' Sweave wrapper for pandoc
##' Like \code{Sweave}, but use \code{RweavePandoc} driver as default value.
##' 
##' 
##' @param file Name of Sweave source file.
##' @param driver Sweave driver
##' @param syntax Sweave syntax
##' @param ... Further arguments passed to the driver's setup function.
##' @author David Hajage \email{dhajage@@gmail.com} Matti Pastell \email{matti.pastell@@helsinki.fi}
##' @seealso \code{\link{Sweave}}
##' @export
##' @import utils
##' @keywords IO file
Pandoc <- Sweave
formals(Pandoc) <-  alist(file=, driver=RweavePandoc, syntax=SweaveSyntaxNoweb, ...=)
