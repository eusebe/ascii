##' A driver to parse asciidoc noweb files with Sweave tool
##' This driver parses asciidoc files containing R code and replace pieces of
##' code with their output.
##' 
##' 
##' @aliases RweaveAsciidoc RtangleAsciidoc RweaveAsciidocOptions
##'   RweaveAsciidocFinish RweaveAsciidocWritedoc RweaveAsciidocSetup
##'   RweaveAsciidocRuncode cacheSweaveAsciidoc weaverAsciidoc
##' @return None value is returned. From a .Rnw noweb file, the corresponding
##'   .txt is produced (as eventuals files for graphs).
##' @note In order to work properly, noweb codes have to be located at the
##'   beginning of a line (no indentation).
##' 
##' Compare with RweaveLatex driver, RweaveAsciidoc provides four new options :
##'   \code{res} for the resolution of jpg or png figure (if produced),
##'   \code{ext} (extension) for the format of figure that will be inserted,
##'   and \code{png} and \code{jpg} (from \code{R2HTML} package) to produce png
##'   and jpg figures.
##' 
##' In addition, \code{cache} option from \code{cacheSweave} or \code{weaver}
##'   package is also available with \code{cacheSweaveAsciidoc} driver and
##'   \code{weaverAsciidoc} driver.
##' 
##' A wrapper for \code{Sweave} can be used, named \code{Asciidoc}.
##' @author David Hajage \email{dhajage@@gmail.com}
##' @seealso \code{\link[utils]{Sweave}}, \code{\link[ascii]{Asciidoc}}
##' @keywords IO file
##' @export
##' @examples
##'   \dontrun{
##' library(ascii)
##' Asciidoc("file.Rnw")
##'   }
##' 
RweaveAsciidoc <- function()
{
    list(setup = RweaveAsciiSetup,
         runcode = RweaveAsciiRuncode,
         writedoc = RweaveAsciiWritedoc,
         finish = RweaveAsciiFinish,
         checkopts = RweaveAsciiOptions)
}

##' A driver to parse txt2tags noweb files with Sweave tool
##' This driver parses txt2tags files containing R code and replace pieces of
##' code with their output.
##' 
##' 
##' @aliases RweaveT2t RtangleT2t RweaveT2tOptions RweaveT2tFinish
##'   RweaveT2tWritedoc RweaveT2tSetup RweaveT2tRuncode cacheSweaveT2t
##'   weaverT2t
##' @return None value is returned. From a .Rnw noweb file, the corresponding
##'   .t2t is produced (as eventuals files for graphs).
##' @note In order to work properly, noweb codes have to be located at the
##'   beginning of a line (no indentation).
##' 
##' Compare with RweaveLatex driver, RweaveT2t provides four new options :
##'   \code{res} for the resolution of jpg or png figure (if produced),
##'   \code{ext} (extension) for the format of figure that will be inserted,
##'   and \code{png} and \code{jpg} (from \code{R2HTML} package) to produce png
##'   and jpg figures.
##' 
##' In addition, \code{cache} option from \code{cacheSweave} or \code{weaver}
##'   package is also available with \code{cacheSweaveT2t} driver and
##'   \code{weaverT2t} driver.
##' 
##' A wrapper for \code{Sweave} can be used, named \code{T2t}.
##' @author David Hajage \email{dhajage@@gmail.com}
##' @seealso \code{\link[utils]{Sweave}}, \code{\link[ascii]{T2t}}
##' @keywords IO file
##' @export
##' @examples
##'   \dontrun{
##' library(ascii)
##' T2t("file.Rnw")
##'   }
##' 
RweaveT2t <- function()
{
    list(setup = RweaveT2tSetup,
         runcode = RweaveAsciiRuncode,
         writedoc = RweaveAsciiWritedoc,
         finish = RweaveAsciiFinish,
         checkopts = RweaveAsciiOptions)
}

##' RweaveT2tSetup
##'
##' @param file file
##' @param syntax syntax
##' @param output output
##' @param quiet quite
##' @param debug debug
##' @param stylepath stylepath
##' @param ... ...
##' @keywords internal
RweaveT2tSetup <- RweaveAsciiSetup
formals(RweaveT2tSetup) <-alist(file=, syntax=, output=NULL, quiet=FALSE, debug=FALSE,
                                extension="t2t", backend="txt2tags", openSchunk="```",
                                closeSchunk="\n```\n", openSinput="", closeSinput="",
                                openSoutput="\n", closeSoutput="", indent="", openInclude ="%!include: ",
                                closeInclude=".t2t", openFig="[", closeFig="]", ...=)

##' A driver to parse org noweb files with Sweave tool
##' This driver parses org files containing R code and replace pieces of code
##' with their output.
##' 
##' 
##' @aliases RweaveOrg RtangleOrg RweaveOrgOptions RweaveOrgFinish
##'   RweaveOrgWritedoc RweaveOrgSetup RweaveOrgRuncode cacheSweaveOrg
##'   weaverOrg
##' @return None value is returned. From a .Rnw noweb file, the corresponding
##'   .org is produced (as eventuals files for graphs).
##' @note In order to work properly, noweb codes have to be located at the
##'   beginning of a line (no indentation).
##' 
##' Compare with RweaveLatex driver, RweaveOrg provides four new options :
##'   \code{res} for the resolution of jpg or png figure (if produced),
##'   \code{ext} (extension) for the format of figure that will be inserted,
##'   and \code{png} and \code{jpg} (from \code{R2HTML} package) to produce png
##'   and jpg figures.
##' 
##' In addition, \code{cache} option from \code{cacheSweave} or \code{weaver}
##'   package is also available with \code{cacheSweaveOrg} driver and
##'   \code{weaverOrg} driver.
##' 
##' A wrapper for \code{Sweave} can be used, named \code{Org}.
##' @author David Hajage \email{dhajage@@gmail.com}
##' @seealso \code{\link[utils]{Sweave}}, \code{\link[ascii]{Org}}
##' @keywords IO file
##' @export
##' @examples
##'   \dontrun{
##' library(ascii)
##' Org("file.Rnw")
##'   }
##' 
RweaveOrg <- function()
{
    list(setup = RweaveOrgSetup,
         runcode = RweaveAsciiRuncode,
         writedoc = RweaveAsciiWritedoc,
         finish = RweaveAsciiFinish,
         checkopts = RweaveAsciiOptions)
}

##' RweaveOrgSetup
##'
##' @param file file
##' @param syntax syntax
##' @param output output
##' @param quiet quite
##' @param debug debug
##' @param stylepath stylepath
##' @param ... ...
##' @keywords internal
RweaveOrgSetup <- RweaveAsciiSetup
formals(RweaveOrgSetup) <-alist(file=, syntax=, output=NULL, quiet=FALSE, debug=FALSE,
                                extension="org", backend="org-mode", openSchunk="#+BEGIN_SRC R-transcript",
                                closeSchunk="\n#+END_SRC\n", openSinput="", closeSinput="",
                                openSoutput="\n", closeSoutput="", indent="", openInclude ="#+INCLUDE: \"",
                                closeInclude=".org\"", openFig="[[file:", closeFig="]]", ...=)

##' A driver to parse Pandoc noweb files with Sweave tool
##' This driver parses Pandoc files containing R code and replace pieces of code
##' with their output.
##' 
##' 
##' @aliases RweavePandoc RtanglePandoc RweavePandocOptions RweavePandocFinish
##'   RweavePandocWritedoc RweavePandocSetup RweavePandocRuncode cacheSweavePandoc
##'   weaverPandoc
##' @return None value is returned. From a .Rnw noweb file, the corresponding
##'   .md is produced (as eventuals files for graphs).
##' @note In order to work properly, noweb codes have to be located at the
##'   beginning of a line (no indentation).
##' 
##' Compare with RweaveLatex driver, RweavePandoc provides four new options :
##'   \code{res} for the resolution of jpg or png figure (if produced),
##'   \code{ext} (extension) for the format of figure that will be inserted,
##'   and \code{png} and \code{jpg} (from \code{R2HTML} package) to produce png
##'   and jpg figures.
##' 
##' In addition, \code{cache} option from \code{cacheSweave} or \code{weaver}
##'   package is also available with \code{cacheSweavePandoc} driver and
##'   \code{weaverPandoc} driver.
##' 
##' A wrapper for \code{Sweave} can be used, named \code{Pandoc}.
##' @author David Hajage \email{dhajage@@gmail.com} Matti Pastell \email{matti.pastell@@helsinki.fi}
##' @seealso \code{\link[utils]{Sweave}}, \code{\link[ascii]{Pandoc}}
##' @keywords IO file
##' @export
##' @examples
##'   \dontrun{
##' library(ascii)
##' Pandoc("file.Rnw")
##'   }
##' 
RweavePandoc <- function()
{
    list(setup = RweavePandocSetup,
         runcode = RweaveAsciiRuncode,
         writedoc = RweaveAsciiWritedoc,
         finish = RweaveAsciiFinish,
         checkopts = RweaveAsciiOptions)
}

##' RweavePandocSetup
##'
##' @param file file
##' @param syntax syntax
##' @param output output
##' @param quiet quite
##' @param debug debug
##' @param stylepath stylepath
##' @param ... ...
##' @keywords internal
RweavePandocSetup <- RweaveAsciiSetup
formals(RweavePandocSetup) <-alist(file=, syntax=, output=NULL, quiet=FALSE, debug=FALSE,
                                   extension="md", backend="pandoc", openSchunk="~~~~~~{.R}",
                                   closeSchunk="\n~~~~~~~~~~~\n\n", openSinput="", closeSinput="",
                                   openSoutput="\n", closeSoutput="", indent="", openInclude ="",
                                   closeInclude="", openFig="![](", closeFig=")", ...=)

##' A driver to parse textile noweb files with Sweave tool
##' This driver parses textile files containing R code and replace pieces of
##' code with their output.
##' 
##' 
##' @aliases RweaveTextile RtangleTextile RweaveTextileOptions
##'   RweaveTextileFinish RweaveTextileWritedoc RweaveTextileSetup
##'   RweaveTextileRuncode cacheSweaveTextile weaverTextile
##' @return None value is returned. From a .Rnw noweb file, the corresponding
##'   .txt is produced (as eventuals files for graphs).
##' @note In order to work properly, noweb codes have to be located at the
##'   beginning of a line (no indentation).
##' 
##' Compare with RweaveLatex driver, RweaveTextile provides four new options :
##'   \code{res} for the resolution of jpg or png figure (if produced),
##'   \code{ext} (extension) for the format of figure that will be inserted,
##'   and \code{png} and \code{jpg} (from \code{R2HTML} package) to produce png
##'   and jpg figures.
##' 
##' In addition, \code{cache} option from \code{cacheSweave} or \code{weaver}
##'   package is also available with \code{cacheSweaveTextile} driver and
##'   \code{weaverTextile} driver.
##' 
##' A wrapper for \code{Sweave} can be used, named \code{Textile}.
##' @author David Hajage \email{dhajage@@gmail.com}
##' @seealso \code{\link[utils]{Sweave}}, \code{\link[ascii]{Textile}}
##' @keywords IO file
##' @export
##' @examples
##'   \dontrun{
##' library(ascii)
##' Textile("file.Rnw")
##'   }
##' 
RweaveTextile <- function()
{
    list(setup = RweaveTextileSetup,
         runcode = RweaveAsciiRuncode,
         writedoc = RweaveAsciiWritedoc,
         finish = RweaveAsciiFinish,
         checkopts = RweaveAsciiOptions)
}

##' RweaveTextileSetup
##'
##' @param file file
##' @param syntax syntax
##' @param output output
##' @param quiet quite
##' @param debug debug
##' @param stylepath stylepath
##' @param ... ...
##' @keywords internal
RweaveTextileSetup <- RweaveAsciiSetup
formals(RweaveTextileSetup) <-alist(file=, syntax=, output=NULL, quiet=FALSE, debug=FALSE,
                                    extension="txt", backend="textile", openSchunk="\nbc.. ",
                                    closeSchunk="\n\np. \n\n", openSinput="", closeSinput="",
                                    openSoutput="\n", closeSoutput="", indent="", openInclude ="",
                                    closeInclude="", openFig="!", closeFig="!", ...=)

##' A driver to parse sphinx noweb files with Sweave tool
##' This driver parses sphinx files containing R code and replace pieces of
##' code with their output.
##' 
##' 
##' @aliases RweaveReST RtangleReST RweaveReSTOptions RweaveReSTFinish
##'   RweaveReSTWritedoc RweaveReSTSetup RweaveReSTRuncode cacheSweaveReST
##'   weaverReST
##' @return None value is returned. From a .Rnw noweb file, the corresponding
##'   .rst is produced (as eventuals files for graphs).
##' @note In order to work properly, noweb codes have to be located at the
##'   beginning of a line (no indentation).
##' 
##' Compare with RweaveLatex driver, RweaveReST provides four new options :
##'   \code{res} for the resolution of jpg or png figure (if produced),
##'   \code{ext} (extension) for the format of figure that will be inserted,
##'   and \code{png} and \code{jpg} (from \code{R2HTML} package) to produce png
##'   and jpg figures.
##' 
##' In addition, \code{cache} option from \code{cacheSweave} or \code{weaver}
##'   package is also available with \code{cacheSweaveReST} driver and
##'   \code{weaverReST} driver.
##' 
##' A wrapper for \code{Sweave} can be used, named \code{ReST}.
##' @author David Hajage \email{dhajage@@gmail.com}
##' @seealso \code{\link[utils]{Sweave}}, \code{\link[ascii]{ReST}}
##' @keywords IO file
##' @export
##' @examples
##'   \dontrun{
##' library(ascii)
##' ReST("file.Rnw")
##'   }
##' 
RweaveReST <- function()
{
    list(setup = RweaveReSTSetup,
         runcode = RweaveAsciiRuncode,
         writedoc = RweaveAsciiWritedoc,
         finish = RweaveAsciiFinish,
         checkopts = RweaveAsciiOptions)
}

##' RweaveReSTSetup
##'
##' @param file file
##' @param syntax syntax
##' @param output output
##' @param quiet quite
##' @param debug debug
##' @param stylepath stylepath
##' @param ... ...
##' @keywords internal
RweaveReSTSetup <- RweaveAsciiSetup
formals(RweaveReSTSetup) <-alist(file=, syntax=, output=NULL, quiet=FALSE, debug=FALSE,
                                 extension="rst", backend="docutils, sphinx, ...", openSchunk=".. code-block:: r\n",
                                 closeSchunk="\n\n", openSinput="", closeSinput="",
                                 openSoutput="\n", closeSoutput="", indent="  ", openInclude =".. include::",
                                 closeInclude=".rst", openFig=".. image:: ", closeFig="", ...=)
