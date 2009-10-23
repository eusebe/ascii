\name{Sphinx}
\alias{Sphinx}
\title{Sweave wrapper for sphinx}
\description{Like \code{Sweave}, but use \code{RweaveSphinx} driver as default value.}
\usage{
Sphinx(file, driver = RweaveSphinx, syntax = SweaveSyntaxNoweb, ...)
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
