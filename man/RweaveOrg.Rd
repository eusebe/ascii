\name{RweaveOrg}
\alias{RweaveOrg}
\alias{RtangleOrg}
\alias{RweaveOrgOptions}
\alias{RweaveOrgFinish}
\alias{RweaveOrgWritedoc}
\alias{RweaveOrgSetup}
\alias{RweaveOrgRuncode}
\alias{cacheSweaveOrg}
\alias{weaverOrg}

\title{ A driver to parse org noweb files with Sweave tool}
\description{
     This driver parses org files containing R code and replace pieces of code with their output. 
}
\usage{
  RweaveOrg()
  cacheSweaveOrg()
  weaverOrg()
}
\value{
    None value is returned. From a .Rnw noweb file, the corresponding .org is produced (as eventuals files for graphs).
}

\author{ David Hajage \email{dhajage@gmail.com}}
\note{
  In order to work properly, noweb codes have to be located at the beginning of a line (no indentation). 
  
  Compare with RweaveLatex driver, RweaveOrg provides four new options : 
  \code{res} for the resolution of jpg or png figure (if produced),  
  \code{ext} (extension) for the format of figure that will be inserted, and
  \code{png} and \code{jpg} (from \code{R2HTML} package) to produce png and jpg figures.

  In addition, \code{cache} option from \code{cacheSweave} or
  \code{weaver} package is also available with
  \code{cacheSweaveOrg} driver and \code{weaverOrg} driver.

  A wrapper for \code{Sweave} can be used, named \code{Org}.
}


\seealso{ \code{\link[utils]{Sweave}}, \code{\link[ascii]{Org}}}
\examples{

  \dontrun{
library(ascii)
Org("file.Rnw")
  }
}
\keyword{ IO}
\keyword{ file}
