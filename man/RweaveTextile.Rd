\name{RweaveTextile}
\alias{RweaveTextile}
\alias{RtangleTextile}
\alias{RweaveTextileOptions}
\alias{RweaveTextileFinish}
\alias{RweaveTextileWritedoc}
\alias{RweaveTextileSetup}
\alias{RweaveTextileRuncode}
\alias{cacheSweaveTextile}
\alias{weaverTextile}

\title{ A driver to parse textile noweb files with Sweave tool}
\description{
     This driver parses textile files containing R code and replace pieces of code with their output. 
}
\usage{
  RweaveTextile()
  cacheSweaveTextile()
  weaverTextile()
}
\value{
    None value is returned. From a .Rnw noweb file, the corresponding .txt is produced (as eventuals files for graphs).
}

\author{ David Hajage \email{dhajage@gmail.com}}
\note{
  In order to work properly, noweb codes have to be located at the beginning of a line (no indentation). 
  
  Compare with RweaveLatex driver, RweaveTextile provides four new options : 
  \code{res} for the resolution of jpg or png figure (if produced),  
  \code{ext} (extension) for the format of figure that will be inserted, and
  \code{png} and \code{jpg} (from \code{R2HTML} package) to produce png and jpg figures.
  
  In addition, \code{cache} option from \code{cacheSweave} or
  \code{weaver} package is also available with
  \code{cacheSweaveTextile} driver and \code{weaverTextile} driver.

  A wrapper for \code{Sweave} can be used, named \code{Textile}.
}


\seealso{ \code{\link[utils]{Sweave}}, \code{\link[ascii]{Textile}}}
\examples{

  \dontrun{
library(ascii)
Textile("file.Rnw")
  }
}
\keyword{ IO}
\keyword{ file}
