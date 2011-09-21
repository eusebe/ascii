\name{Asciidoc}
\alias{Asciidoc}
\alias{Org}
\alias{Pandoc}
\alias{ReST}
\alias{T2t}
\alias{Textile}
\title{Sweave wrappers}
\usage{
Asciidoc(file, driver = RweaveAsciidoc, syntax = SweaveSyntaxNoweb, encoding = "", ...)

T2t(file, driver = RweaveT2t, syntax = SweaveSyntaxNoweb, encoding = "", ...)

ReST(file, driver = RweaveReST, syntax = SweaveSyntaxNoweb, encoding = "", ...)

Org(file, driver = RweaveOrg, syntax = SweaveSyntaxNoweb, encoding = "", ...)

Textile(file, driver = RweaveTextile, syntax = SweaveSyntaxNoweb, encoding = "", ...)

Pandoc(file, driver = RweavePandoc, syntax = SweaveSyntaxNoweb, encoding = "", ...)
}
\arguments{
  \item{file}{Name of Sweave source file.}

  \item{driver}{Sweave driver}

  \item{syntax}{Sweave syntax}

  \item{encoding}{Encoding}

  \item{...}{Further arguments passed to the driver's setup
  function.}
}
\description{
Sweave wrappers
}
\author{
David Hajage \email{dhajage@gmail.com}
}
\seealso{
\code{\link{Sweave}}
}
\keyword{file}
\keyword{IO}

