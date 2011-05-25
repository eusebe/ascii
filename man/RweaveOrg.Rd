\name{RweaveOrg}
\alias{RweaveOrg}
\title{A driver to parse org noweb files with Sweave tool...}
\description{A driver to parse org noweb files with Sweave tool
This driver parses org files containing R code and replace pieces of code
with their output.}
\alias{RweaveOrg}
\alias{RtangleOrg}
\alias{RweaveOrgOptions}
\alias{RweaveOrgFinish}
\alias{RweaveOrgWritedoc}
\alias{RweaveOrgSetup}
\alias{RweaveOrgRuncode}
\alias{cacheSweaveOrg}
\alias{weaverOrg}
\value{None value is returned. From a .Rnw noweb file, the corresponding
.org is produced (as eventuals files for graphs).}
\note{In order to work properly, noweb codes have to be located at the
beginning of a line (no indentation).

Compare with RweaveLatex driver, RweaveOrg provides one new option :
\code{format} to choose the format of figure that will be inserted in the
final document.

In addition, \code{cache} option from \code{cacheSweave} or \code{weaver}
package is also available with \code{cacheSweaveOrg} driver and
\code{weaverOrg} driver.

A wrapper for \code{Sweave} can be used, named \code{Org}.}
\author{David Hajage \email{dhajage@gmail.com}}
\seealso{\code{\link[utils]{Sweave}}, \code{\link[ascii]{Org}}}
\keyword{IO}
\keyword{file}
\examples{\dontrun{
library(ascii)
Org("file.Rnw")
}}

