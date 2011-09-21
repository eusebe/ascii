\name{cacheSweaveTextile}
\alias{cacheSweaveTextile}
\alias{cacheSweaveTextileSetup}
\alias{RtangleTextile}
\alias{RweaveTextile}
\alias{RweaveTextileFinish}
\alias{RweaveTextileOptions}
\alias{RweaveTextileRuncode}
\alias{RweaveTextileSetup}
\alias{RweaveTextileWritedoc}
\alias{weaverTextile}
\alias{weaverTextileSetup}
\title{A driver to parse textile noweb files with Sweave tool - cacheSweave based}
\usage{
cacheSweaveTextileSetup(..., cache = FALSE, trace = FALSE, dependson = NULL)

RweaveTextileSetup(file, syntax, output = NULL, quiet = FALSE, debug = FALSE, extension = "txt", backend = "textile", openSchunk = "\nbc.. ", closeSchunk = "\n\np. \n\n", openSinput = "", closeSinput = "", openSoutput = "\n", closeSoutput = "", indent = "", openInclude = "", closeInclude = "", openFig = "!", closeFig = "!", ...)

weaverTextileSetup(file, syntax, output = NULL, quiet = FALSE, debug = FALSE, extension = "txt", backend = "textile", openSchunk = "\nbc.. ", closeSchunk = "\n\np. \n\n", openSinput = "", closeSinput = "", openSoutput = "\n", closeSoutput = "", indent = "", openInclude = "", closeInclude = "", openFig = "!", closeFig = "!", use.cache = TRUE, ...)
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
.txt is produced (as eventuals files for graphs).
}
\description{
A driver to parse textile noweb files with Sweave tool - cacheSweave based

A driver to parse textile noweb files with Sweave tool - cacheSweave based

A driver to parse textile noweb files with Sweave tool
This driver parses textile files containing R code and replace pieces of
code with their output.

RweaveTextileSetup

weaverTextileSetup

weaverTextile
}
\note{
In order to work properly, noweb codes have to be located at the
beginning of a line (no indentation).

Compare with RweaveLatex driver, RweaveTextile provides one new option :
\code{format} to choose the format of figure that will be inserted in the
final document.

In addition, \code{cache} option from \code{cacheSweave} or \code{weaver}
package is also available with \code{cacheSweaveTextile} driver and
\code{weaverTextile} driver.

A wrapper for \code{Sweave} can be used, named \code{Textile}.
}
\examples{
\dontrun{
library(ascii)
Textile("file.Rnw")
}
}
\author{
David Hajage

David Hajage

David Hajage \email{dhajage@gmail.com}
}
\seealso{
\code{\link[utils]{Sweave}}, \code{\link[ascii]{Textile}}
}
\keyword{file}
\keyword{internal}
\keyword{IO}

