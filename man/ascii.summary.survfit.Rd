\name{ascii.summary.survfit}
\alias{ascii.summary.survfit}
\title{ascii method for class summary...}
\usage{\method{ascii}{summary.survfit} (x, include.colnames=TRUE, header=TRUE,
    digits=c(0, 0, 0, 3, 3, 3, 3), ...)}
\description{ascii method for class summary.survfit}
\details{\code{survival} package}
\value{An ascii object.}
\author{David Hajage}
\arguments{\item{x}{An R object of class found among \code{methods(ascii)}.}
\item{include.colnames}{logical. If \code{TRUE} the columns names are
printed. Default value depends of class of \code{x}.}
\item{header}{logical or numeric. If \code{TRUE} or \code{1}, \code{2},
\dots{}, the first line(s) of the table is (are) emphasized. The default
value depends of class of \code{x}.}
\item{digits}{Numeric vector of length equal to the number of columns of
the resulting table (otherwise it will be replicated or truncated as
necessary) indicating the number of digits to display in the
corresponding columns.  Default is \code{2}.}
\item{...}{Additional arguments.  (see \code{?ascii.default}).}}
