\name{RweavePandoc}
\alias{RweavePandoc}
\title{A driver to parse Pandoc noweb files with Sweave tool...}
\description{A driver to parse Pandoc noweb files with Sweave tool
This driver parses Pandoc files containing R code and replace pieces of code
with their output.}
\alias{RweavePandoc}
\alias{RtanglePandoc}
\alias{RweavePandocOptions}
\alias{RweavePandocFinish}
\alias{RweavePandocWritedoc}
\alias{RweavePandocSetup}
\alias{RweavePandocRuncode}
\alias{cacheSweavePandoc}
\alias{weaverPandoc}
\value{None value is returned. From a .Rnw noweb file, the corresponding
.md is produced (as eventuals files for graphs).}
\note{In order to work properly, noweb codes have to be located at the
beginning of a line (no indentation).

Compare with RweaveLatex driver, RweavePandoc provides four new options :
\code{res} for the resolution of jpg or png figure (if produced),
\code{ext} (extension) for the format of figure that will be inserted,
and \code{png} and \code{jpg} (from \code{R2HTML} package) to produce png
and jpg figures.

In addition, \code{cache} option from \code{cacheSweave} or \code{weaver}
package is also available with \code{cacheSweavePandoc} driver and
\code{weaverPandoc} driver.

A wrapper for \code{Sweave} can be used, named \code{Pandoc}.}
\author{David Hajage \email{dhajage@gmail.com} Matti Pastell \email{matti.pastell@helsinki.fi}}
\seealso{\code{\link[utils]{Sweave}}, \code{\link[ascii]{Pandoc}}}
\keyword{IO}
\keyword{file}
\examples{\dontrun{
library(ascii)
Pandoc("file.Rnw")
}}

