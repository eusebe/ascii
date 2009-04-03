\name{RweaveAsciidoc}
\alias{RweaveAsciidoc}
\alias{RweaveAsciidocOptions}
\alias{RweaveAsciidocFinish}
\alias{RweaveAsciidocWritedoc}
\alias{RweaveAsciidocSetup}
\alias{RweaveAsciidocRuncode}
\alias{SweaveSyntaxAsciidoc}

\title{ A driver to parse asciidoc noweb files with Sweave tool}
\description{
     This driver parses asciidoc files containing R code and replace pieces of code with their output. 
}
\usage{
  RweaveAsciidoc()
}
\value{
    None value is returned. From a .Rnw noweb file, the corresponding .txt is produced (as eventuals files for graphs).
}

\author{ David Hajage \email{dhajage@gmail.com}}
\note{
  In order to work properly, noweb codes have to be located at the beginning of a line (no indentation). 
  Compare with RweaveLatex driver, RweaveAsciidoc provides two new options : 
  \code{res} for the resolution of jpg or png figure (if produced), and 
  \code{ext} (extension) for the format of figure that will be inserted.
}


\seealso{ \code{\link[tools]{Sweave}}}
\examples{

  \dontrun{
library(tools)
Sweave("file.Rnw",driver=RweaveAsciidoc)
  }
}
\keyword{ IO}
\keyword{ file}
