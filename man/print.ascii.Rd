\name{print.ascii}
\alias{print.ascii}
\title{Print ascii object}
\description{Function displaying the AsciiDoc or txt2tags code associated with the supplied object of class \code{ascii}.}
\usage{
print.ascii(x, type = getOption("asciiType"), ...)
}
\arguments{
  \item{x}{An object of class \code{"ascii"}}
  \item{type}{Type of syntax produce.  Possible values for \code{type} are \code{"asciidoc"} or \code{"t2t"}.
              Default value produce AsciiDoc syntax.}
  \item{...}{Additional arguments.  (Currently ignored.)}
}
\author{David Hajage \email{dhajage@gmail.com}}
\seealso{\code{\link{ascii}}}
\examples{
data(esoph)
ascii(esoph[1:20,])
print(ascii(esoph[1:20,]), type = "t2t")
options(asciiType = "t2t")
ascii(esoph[1:20,])
options(asciiType = "asciidoc")
}
\keyword{print}
