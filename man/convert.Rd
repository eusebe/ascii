\name{convert}
\alias{convert}
\title{Convert a file with specified backend}
\usage{
convert(i, d = NULL, f = NULL, e = NULL, O = NULL, backend = getOption("asciiBackend"), cygwin = FALSE, open = FALSE)
}
\arguments{
  \item{i}{input file}

  \item{d}{output directory}

  \item{f}{format}

  \item{e}{encoding}

  \item{O}{other options}

  \item{backend}{backend (\code{"asciidoc"}, \code{"t2t"}
  or \code{"pandoc"})}

  \item{cygwin}{use cygwin?}

  \item{open}{open resulting file?}
}
\value{
Nothing
}
\description{
Convert a file with specified backend
}
\details{
This function convert a file with asciidoc, txt2tags or pandoc backend
}
\author{
David Hajage
}

