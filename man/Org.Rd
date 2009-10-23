\name{Org}
\alias{Org}
\title{Sweave wrapper for org}
\description{Like \code{Sweave}, but use \code{RweaveOrg} driver as default value.}
\usage{
Org(file, driver = RweaveOrg, syntax = SweaveSyntaxNoweb, ...)
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
