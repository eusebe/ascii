\name{print.ascii}
\alias{print.ascii}
\title{Print ascii object...}
\usage{print.ascii(x, type=getOption("asciiType"), file=NULL, append=FALSE,
    escape=FALSE, list.escape=c("\\\\_", "\\\\^"), ...)}
\description{Print ascii object
Function displaying the asciiDoc, txt2tags, reStructuredText, org or
textile code associated with the supplied object of class \code{ascii}.}
\details{The package provides the new global option \code{asciiType}. Default value
is \code{"asciidoc"} (see examples).}
\author{David Hajage \email{dhajage@gmail.com}}
\seealso{\code{\link{ascii}}}
\keyword{print}
\arguments{\item{x}{An object of class \code{"ascii"}}
\item{type}{Type of syntax produce.  Possible values for \code{type} are
\code{"asciidoc"}, \code{"t2t"}, \code{"rest"}, \code{"org"} or
\code{"textile"}.  Default value produce asciiDoc syntax.}
\item{file}{A character string naming the file to print to. Default is
\code{NULL} (print to the console).}
\item{append}{If \code{TRUE}, code will be appended to \code{file} instead
of overwriting it. Default value is \code{FALSE}}
\item{escape}{If \code{TRUE}, characters in \code{list.escape} will be be
printed with a \code{\\}. Default value is \code{FALSE}}
\item{list.escape}{Character vector. Default value is \code{c("\\\\_",
"\\\\^")}}
\item{...}{Additional arguments.  (Currently ignored.)}}
\examples{data(esoph)
ascii(esoph[1:10,])
print(ascii(esoph[1:10,]), type = "t2t")
print(ascii(esoph[1:10,]), type = "rest")
print(ascii(esoph[1:10,]), type = "org")
print(ascii(esoph[1:10,]), type = "textile")
options(asciiType = "rest")
ascii(esoph[1:10,])
options(asciiType = "asciidoc")}
