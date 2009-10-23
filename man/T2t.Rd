\name{T2t}
\alias{T2t}
\title{Sweave wrapper for txt2tags}
\description{Like \code{Sweave}, but use \code{RweaveT2t} driver as default value.}
\usage{
T2t(file, driver = RweaveT2t, syntax = SweaveSyntaxNoweb, ...)
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
