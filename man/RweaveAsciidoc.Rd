\name{RweaveAsciidoc}
\title{A driver to parse asciidoc noweb files with Sweave tool...}
\description{A driver to parse asciidoc noweb files with Sweave tool
This driver parses asciidoc files containing R code and replace pieces of
code with their output.}
\alias{RweaveAsciidoc}
\alias{RtangleAsciidoc}
\alias{RweaveAsciidocOptions}
\alias{RweaveAsciidocFinish}
\alias{RweaveAsciidocWritedoc}
\alias{RweaveAsciidocSetup}
\alias{RweaveAsciidocRuncode}
\alias{cacheSweaveAsciidoc}
\alias{weaverAsciidoc}
\value{None value is returned. From a .Rnw noweb file, the corresponding
.txt is produced (as eventuals files for graphs).}
\note{In order to work properly, noweb codes have to be located at the
beginning of a line (no indentation).

Compare with RweaveLatex driver, RweaveAsciidoc provides four new options :
\code{res} for the resolution of jpg or png figure (if produced),
\code{ext} (extension) for the format of figure that will be inserted,
and \code{png} and \code{jpg} (from \code{R2HTML} package) to produce png
and jpg figures.

In addition, \code{cache} option from \code{cacheSweave} or \code{weaver}
package is also available with \code{cacheSweaveAsciidoc} driver and
\code{weaverAsciidoc} driver.

A wrapper for \code{Sweave} can be used, named \code{Asciidoc}.}
\author{David Hajage \email{dhajage@gmail.com}}
\seealso{\code{\link[utils]{Sweave}}, \code{\link[ascii]{Asciidoc}}}
\keyword{IO}
\keyword{file}
\examples{\dontrun{
library(ascii)
Asciidoc("file.Rnw")
}}
