\name{convert}
\alias{convert}
\title{Convert a file with specified backend}
\usage{convert(i, d=NULL, f=NULL, e=NULL, O=NULL, backend="asciidoc",
    cygwin=FALSE, open=FALSE)}
\description{Concert a file with specified backend}
\details{This function convert a file with asciidoc, txt2tags or pandoc backend}
\value{Nothing}
\author{David Hajage}
\arguments{\item{i}{input file}
\item{d}{output directory}
\item{f}{format}
\item{e}{encoding}
\item{O}{other options}
\item{backend}{backend (\code{"asciidoc"}, \code{"txt2tags"} or \code{"pandoc"})}
\item{cygwin}{use cygwin?}
\item{open}{open resulting file?}}
