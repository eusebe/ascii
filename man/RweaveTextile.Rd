\name{RweaveTextile}
\alias{RweaveTextile}
\title{A driver to parse textile noweb files with Sweave tool...}
\description{A driver to parse textile noweb files with Sweave tool
This driver parses textile files containing R code and replace pieces of
code with their output.}
\alias{RweaveTextile}
\alias{RtangleTextile}
\alias{RweaveTextileOptions}
\alias{RweaveTextileFinish}
\alias{RweaveTextileWritedoc}
\alias{RweaveTextileSetup}
\alias{RweaveTextileRuncode}
\alias{cacheSweaveTextile}
\alias{weaverTextile}
\value{None value is returned. From a .Rnw noweb file, the corresponding
.txt is produced (as eventuals files for graphs).}
\note{In order to work properly, noweb codes have to be located at the
beginning of a line (no indentation).

Compare with RweaveLatex driver, RweaveTextile provides one new option :
\code{format} to choose the format of figure that will be inserted in the
final document.

In addition, \code{cache} option from \code{cacheSweave} or \code{weaver}
package is also available with \code{cacheSweaveTextile} driver and
\code{weaverTextile} driver.

A wrapper for \code{Sweave} can be used, named \code{Textile}.}
\author{David Hajage \email{dhajage@gmail.com}}
\seealso{\code{\link[utils]{Sweave}}, \code{\link[ascii]{Textile}}}
\keyword{IO}
\keyword{file}
\examples{\dontrun{
library(ascii)
Textile("file.Rnw")
}}

