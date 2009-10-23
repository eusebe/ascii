\name{Asciidoc}
\alias{Asciidoc}
\title{Sweave wrapper for asciidoc}
\description{Like \code{Sweave}, but use \code{RweaveAsciidoc} driver as default value.}
\usage{
Asciidoc(file, driver = RweaveAsciidoc, syntax = SweaveSyntaxNoweb, ...)
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
