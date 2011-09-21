\name{cacheSweaveT2t}
\alias{cacheSweaveT2t}
\alias{cacheSweaveT2tSetup}
\alias{RtangleT2t}
\alias{RweaveT2t}
\alias{RweaveT2tFinish}
\alias{RweaveT2tOptions}
\alias{RweaveT2tRuncode}
\alias{RweaveT2tSetup}
\alias{RweaveT2tWritedoc}
\alias{weaverT2t}
\alias{weaverT2tSetup}
\title{A driver to parse txt2tags noweb files with Sweave tool - cacheSweave based}
\usage{
cacheSweaveT2tSetup(..., cache = FALSE, trace = FALSE, dependson = NULL)

RweaveT2tSetup(file, syntax, output = NULL, quiet = FALSE, debug = FALSE, extension = "t2t", backend = "txt2tags", openSchunk = "```", closeSchunk = "\n```\n", openSinput = "", closeSinput = "", openSoutput = "\n", closeSoutput = "", indent = "", openInclude = "\%!include: ", closeInclude = ".t2t", openFig = "[", closeFig = "]", ...)

weaverT2tSetup(file, syntax, output = NULL, quiet = FALSE, debug = FALSE, extension = "t2t", backend = "txt2tags", openSchunk = "```", closeSchunk = "\n```\n", openSinput = "", closeSinput = "", openSoutput = "\n", closeSoutput = "", indent = "", openInclude = "\%!include: ", closeInclude = ".t2t", openFig = "[", closeFig = "]", use.cache = TRUE, ...)
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
.t2t is produced (as eventuals files for graphs).
}
\description{
A driver to parse txt2tags noweb files with Sweave tool - cacheSweave based

A driver to parse txt2tags noweb files with Sweave tool - cacheSweave based

A driver to parse txt2tags noweb files with Sweave tool
This driver parses txt2tags files containing R code and replace pieces of
code with their output.

RweaveT2tSetup

weaverT2tSetup

weaverT2t
}
\note{
In order to work properly, noweb codes have to be located at the
beginning of a line (no indentation).

Compare with RweaveLatex driver, RweaveT2t provides one new option :
\code{format} to choose the format of figure that will be inserted in the
final document.

In addition, \code{cache} option from \code{cacheSweave} or \code{weaver}
package is also available with \code{cacheSweaveT2t} driver and
\code{weaverT2t} driver.

A wrapper for \code{Sweave} can be used, named \code{T2t}.
}
\examples{
\dontrun{
library(ascii)
T2t("file.Rnw")
}
}
\author{
David Hajage

David Hajage

David Hajage \email{dhajage@gmail.com}
}
\seealso{
\code{\link[utils]{Sweave}}, \code{\link[ascii]{T2t}}
}
\keyword{file}
\keyword{internal}
\keyword{IO}

