\name{cacheSweavePandoc}
\alias{cacheSweavePandoc}
\alias{cacheSweavePandocSetup}
\alias{RtanglePandoc}
\alias{RweavePandoc}
\alias{RweavePandocFinish}
\alias{RweavePandocOptions}
\alias{RweavePandocRuncode}
\alias{RweavePandocSetup}
\alias{RweavePandocWritedoc}
\alias{weaverPandoc}
\alias{weaverPandocSetup}
\title{A driver to parse pandoc noweb files with Sweave tool - cacheSweave based}
\usage{
cacheSweavePandocSetup(..., cache = FALSE, trace = FALSE, dependson = NULL)

RweavePandocSetup(file, syntax, output = NULL, quiet = FALSE, debug = FALSE, extension = "md", backend = "pandoc", openSchunk = "~~~~~~{.R}", closeSchunk = "\n~~~~~~~~~~~\n\n", openSinput = "", closeSinput = "", openSoutput = "\n", closeSoutput = "", indent = "", openInclude = "", closeInclude = "", openFig = "![](", closeFig = ")", ...)

weaverPandocSetup(file, syntax, output = NULL, quiet = FALSE, debug = FALSE, extension = "md", backend = "pandoc", openSchunk = "~~~~~~{.R}", closeSchunk = "\n~~~~~~~~~~~\n\n", openSinput = "", closeSinput = "", openSoutput = "\n", closeSoutput = "", indent = "", openInclude = "", closeInclude = "", openFig = "![](", closeFig = ")", use.cache = TRUE, ...)
}
\arguments{
  \item{trace}{trace}

  \item{dependson}{dependson}

  \item{file}{file}

  \item{syntax}{syntax}

  \item{output}{output}

  \item{quiet}{quite}

  \item{debug}{debug}

  \item{stylepath}{stylepath}

  \item{...}{...}

  \item{use.cache}{use.cache}
}
\value{
None value is returned. From a .Rnw noweb file, the corresponding
.md is produced (as eventuals files for graphs).
}
\description{
A driver to parse pandoc noweb files with Sweave tool - cacheSweave based

A driver to parse pandoc noweb files with Sweave tool - cacheSweave based

A driver to parse Pandoc noweb files with Sweave tool
This driver parses Pandoc files containing R code and replace pieces of code
with their output.

RweavePandocSetup

weaverPandocSetup

weaverPandoc
}
\note{
In order to work properly, noweb codes have to be located at the
beginning of a line (no indentation).

Compare with RweaveLatex driver, RweavePandoc provides one new option :
\code{format} to choose the format of figure that will be inserted in the
final document.

In addition, \code{cache} option from \code{cacheSweave} or \code{weaver}
package is also available with \code{cacheSweavePandoc} driver and
\code{weaverPandoc} driver.

A wrapper for \code{Sweave} can be used, named \code{Pandoc}.
}
\examples{
\dontrun{
library(ascii)
Pandoc("file.Rnw")
}
}
\author{
David Hajage

David Hajage

David Hajage \email{dhajage@gmail.com} Matti Pastell \email{matti.pastell@helsinki.fi}
}
\seealso{
\code{\link[utils]{Sweave}}, \code{\link[ascii]{Pandoc}}
}
\keyword{file}
\keyword{internal}
\keyword{IO}

