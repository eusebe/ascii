\name{ascii.ftable}
\alias{ascii.ftable}
\title{ascii method for class ftable...}
\usage{\method{ascii}{ftable}(x, digits=getOption("digits"), header=TRUE, ...)
}
\description{ascii method for class ftable}
\value{An ascii object.}
\author{David Hajage}
\arguments{\item{x}{An R object of class found among \code{methods(ascii)}.}
\item{digits}{Numeric vector of length equal to the number of columns of
the resulting table (otherwise it will be replicated or truncated as
necessary) indicating the number of digits to display in the
corresponding columns.  Default is \code{2}.}
\item{header}{logical or numeric. If \code{TRUE} or \code{1}, \code{2},
\dots{}, the first line(s) of the table is (are) emphasized. The default
value depends of class of \code{x}.}
\item{...}{Additional arguments.  (see \code{?ascii.default}).}
}

