\name{RweaveT2t}
\alias{RweaveT2t}
\alias{RtangleT2t}
\alias{RweaveT2tOptions}
\alias{RweaveT2tFinish}
\alias{RweaveT2tWritedoc}
\alias{RweaveT2tSetup}
\alias{RweaveT2tRuncode}
\alias{cacheSweaveT2t}
\alias{weaverT2t}

\title{ A driver to parse txt2tags noweb files with Sweave tool}
\description{
     This driver parses txt2tags files containing R code and replace pieces of code with their output. 
}
\usage{
  RweaveT2t()
  cacheSweaveT2t()
  weaverT2t()
}
\value{
    None value is returned. From a .Rnw noweb file, the corresponding .t2t is produced (as eventuals files for graphs).
}

\author{ David Hajage \email{dhajage@gmail.com}}
\note{
  In order to work properly, noweb codes have to be located at the beginning of a line (no indentation). 
  
  Compare with RweaveLatex driver, RweaveT2t provides four new options : 
  \code{res} for the resolution of jpg or png figure (if produced),  
  \code{ext} (extension) for the format of figure that will be inserted, and
  \code{png} and \code{jpg} (from \code{R2HTML} package) to produce png and jpg figures.
  
  In addition, \code{cache} option from \code{cacheSweave} or
  \code{weaver} package is also available with
  \code{cacheSweaveT2t} driver and \code{weaverT2t} driver.

  A wrapper for \code{Sweave} can be used, named \code{T2t}.
}


\seealso{ \code{\link[utils]{Sweave}}, \code{\link[ascii]{T2t}}}
\examples{

  \dontrun{
library(ascii)
T2t("file.Rnw")
  }
}
\keyword{ IO}
\keyword{ file}
