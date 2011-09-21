\name{print}
\alias{print,asciiCbind-method}
\alias{print,asciiList-method}
\alias{print,asciiMixed-method}
\alias{print,asciiTable-method}
\alias{print,Report-method}
\alias{show,asciiCbind-method}
\alias{show,asciiList-method}
\alias{show,asciiMixed-method}
\alias{show,asciiTable-method}
\alias{show,Report-method}
\title{Print ascii object}
\arguments{
  \item{x}{An object of class \code{"asciiTable"},
  \code{"asciiList"}, \code{"asciiMixed"},
  \code{"asciiCbind"} or \code{"Report"}.}

  \item{type}{Type of syntax produce.  Possible values for
  \code{type} are \code{"asciidoc"}, \code{"t2t"},
  \code{"rest"}, \code{"org"}, \code{"textile"} or
  \code{"pandoc"}.  Default value produce asciidoc syntax.}

  \item{file}{A character string naming the file to print
  to. Default is \code{NULL} (print to the console).}

  \item{append}{If \code{TRUE}, code will be appended to
  \code{file} instead of overwriting it. Default value is
  \code{FALSE}}

  \item{escape}{If \code{TRUE}, characters in
  \code{list.escape} will be be printed with a \code{\\}.
  Default value is \code{FALSE}}

  \item{list.escape}{Character vector. Default value is
  \code{c("\\\\_", "\\\\^")}}

  \item{help}{logical print help? (objects of class
  \code{"Report"})}

  \item{...}{Additional arguments.  (Currently ignored.)}

  \item{object}{ascii or Report object}
}
\description{
Function displaying the asciidoc, txt2tags, reStructuredText, org or
textile code associated with the supplied object of class \code{ascii}.

Show method for ascii objects
}
\details{
The package provides the new global option \code{asciiType}. Default value
is \code{"asciidoc"} (see examples).
}
\examples{
data(esoph)
ascii(esoph[1:10,])
print(ascii(esoph[1:10,]), type = "t2t")
print(ascii(esoph[1:10,]), type = "rest")
print(ascii(esoph[1:10,]), type = "org")
print(ascii(esoph[1:10,]), type = "textile")
print(ascii(esoph[1:10,]), type = "pandoc")
options(asciiType = "rest")
ascii(esoph[1:10,])
options(asciiType = "asciidoc")
}
\author{
David Hajage \email{dhajage@gmail.com}
}
\seealso{
\code{\link{ascii}}
}
\keyword{print}

