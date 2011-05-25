\name{RweaveReST}
\alias{RweaveReST}
\title{A driver to parse sphinx noweb files with Sweave tool...}
\description{A driver to parse sphinx noweb files with Sweave tool
This driver parses sphinx files containing R code and replace pieces of
code with their output.}
\alias{RweaveReST}
\alias{RtangleReST}
\alias{RweaveReSTOptions}
\alias{RweaveReSTFinish}
\alias{RweaveReSTWritedoc}
\alias{RweaveReSTSetup}
\alias{RweaveReSTRuncode}
\alias{cacheSweaveReST}
\alias{weaverReST}
\value{None value is returned. From a .Rnw noweb file, the corresponding
.rst is produced (as eventuals files for graphs).}
\note{In order to work properly, noweb codes have to be located at the
beginning of a line (no indentation).

Compare with RweaveLatex driver, RweaveReST provides one new option :
\code{format} to choose the format of figure that will be inserted in the
final document.

In addition, \code{cache} option from \code{cacheSweave} or \code{weaver}
package is also available with \code{cacheSweaveReST} driver and
\code{weaverReST} driver.

A wrapper for \code{Sweave} can be used, named \code{ReST}.}
\author{David Hajage \email{dhajage@gmail.com}}
\seealso{\code{\link[utils]{Sweave}}, \code{\link[ascii]{ReST}}}
\keyword{IO}
\keyword{file}
\examples{\dontrun{
library(ascii)
ReST("file.Rnw")
}}

