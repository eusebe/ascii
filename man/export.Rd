\name{export}
\alias{export}
\title{Use asciidoc from R...}
\usage{export(..., list, file, format="xhtml", open, main, author, email,
    revdate, revnumber, encoding, cmd, cygwin=FALSE)}
\description{Use asciidoc from R
These functions allow to use asciidoc and a2x toolchains directly from R.}
\keyword{print}
\value{a list with all \code{...} arguments.}
\author{David Hajage \email{dhajage@gmail.com}}
\arguments{\item{...}{\code{section}, \code{paragraph}, \code{sexpr}, \code{out}, \code{math}, \code{ascii}, or what-you-want (exported as verbatim) objects}
\item{list}{list of objects}
\item{file}{output file (no extension)}
\item{format}{format of the output (chunked, epub, htmlhelp, pdf, text, xhtml, slidy, odt, dvi, ps, tex, docbook, asciidoc)}
\item{open}{open or not resulting file}
\item{main}{main title of the document}
\item{author}{author of the document}
\item{email}{email of the document}
\item{revdate}{revision date of the document}
\item{revnumber}{revision number of the document}
\item{encoding}{encoding format of input file}
\item{cmd}{eventually define the asciidoc command}
\item{cygwin}{do you use asciidoc through cygwin}}
\examples{\dontrun{
export(ascii(head(esoph)))}}
