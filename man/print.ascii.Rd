\name{print.ascii}
\alias{print.ascii}
\title{Print ascii object}
\description{Function displaying the asciiDoc, txt2tags, reStructuredText or org code associated with the supplied object of class \code{ascii}.}
\usage{
print.ascii(x, type = getOption("asciiType"), file = NULL, append = FALSE, ...)
}
\arguments{
  \item{x}{An object of class \code{"ascii"}}
  \item{type}{Type of syntax produce.  Possible values for \code{type}
    are \code{"asciidoc"}, \code{"t2t"}, \code{"rest"} or \code{"org"}.
              Default value produce asciiDoc syntax.}
  \item{file}{A character string naming the file to print to. Default is \code{NULL} (print to the console).}
  \item{append}{If \code{TRUE}, code will be appended to \code{file} instead of overwriting it. Default value is \code{FALSE}}.
  \item{...}{Additional arguments.  (Currently ignored.)}
}
\details{
  The package provides the new global option \code{asciiType}. Default value is \code{"asciidoc"} (see examples).
}
\author{David Hajage \email{dhajage@gmail.com}}
\seealso{\code{\link{ascii}}}
\examples{
data(esoph)
ascii(esoph[1:10,])
print(ascii(esoph[1:10,]), type = "t2t")
print(ascii(esoph[1:10,]), type = "rest")
print(ascii(esoph[1:10,]), type = "org")
options(asciiType = "rest")
ascii(esoph[1:10,])
options(asciiType = "asciidoc")
}
\keyword{print}
