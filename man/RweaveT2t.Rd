\name{RweaveT2t}
\alias{RweaveT2t}
\alias{RweaveT2tOptions}
\alias{RweaveT2tFinish}
\alias{RweaveT2tWritedoc}
\alias{RweaveT2tSetup}
\alias{RweaveT2tRuncode}
\alias{SweaveSyntaxT2t}

\title{ A driver to parse txt2tags noweb files with Sweave tool}
\description{
     This driver parses txt2tags files containing R code and replace pieces of code with their output. 
}
\usage{
  RweaveT2t()
}
\value{
    None value is returned. From a .Rnw noweb file, the corresponding .txt is produced (as eventuals files for graphs).
}

\author{ David Hajage \email{dhajage@gmail.com}}
\note{
  In order to work properly, noweb codes have to be located at the beginning of a line (no indentation). See samples in the samples directory of the package.
}


\seealso{ \code{\link[tools]{Sweave}}}
\examples{

  \dontrun{
library(tools)
Sweave("file.Rnw",driver=RweaveT2t)
  }
}
\keyword{ IO}
\keyword{ file}
