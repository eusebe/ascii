\name{ReST}
\alias{ReST}
\title{Sweave wrapper for reStructuredText}
\description{Like \code{Sweave}, but use \code{RweaveReST} driver as default value.}
\usage{
ReST(file, driver = RweaveReST, syntax = SweaveSyntaxNoweb, ...)
}
\arguments{
  \item{file}{Name of Sweave source file.}
  \item{driver}{Sweave driver}
  \item{syntax}{Sweave syntax}
  \item{...}{Further arguments passed to the driver's setup function.}
}
\author{David Hajage \email{dhajage@gmail.com}}
\seealso{\code{\link{Sweave}}}
\keyword{ IO}
\keyword{ file}
