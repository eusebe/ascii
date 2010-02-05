\name{Textile}
\alias{Textile}
\title{Sweave wrapper for textile}
\description{Like \code{Sweave}, but use \code{RweaveTextile} driver as default value.}
\usage{
Textile(file, driver = RweaveTextile, syntax = SweaveSyntaxNoweb, ...)
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
