\name{ascii.survfit}
\alias{ascii.survfit}
\title{ascii method for class survfit...}
\usage{\method{ascii}{survfit}(x, scale=1, print.rmean=getOption("survfit.print.rmean"),
    rmean=getOption("survfit.rmean"), include.rownames=TRUE,
    include.colnames=TRUE, header=TRUE, ...)
}
\description{ascii method for class survfit}
\details{\code{survival} package}
\value{An ascii object.}
\author{David Hajage}
\arguments{\item{x}{An R object of class found among \code{methods(ascii)}.}
\item{scale}{A numeric value to rescale the survival time, e.g., if the
input data to survfit were in days, \code{scale=365} would scale the
printout to years (see \code{print.survfit()} in package
\code{survival}).}
\item{print.rmean}{Option for computation and display of the restricted mean (see \code{print.survfit()} in package \code{survival}).}
\item{rmean}{Option for computation and display of the restricted mean (see \code{print.survfit()} in package \code{survival}).}
\item{include.rownames}{logical. If \code{TRUE} the rows names are printed.
Default value depends of class of \code{x}.}
\item{include.colnames}{logical. If \code{TRUE} the columns names are
printed. Default value depends of class of \code{x}.}
\item{header}{logical or numeric. If \code{TRUE} or \code{1}, \code{2},
\dots{}, the first line(s) of the table is (are) emphasized. The default
value depends of class of \code{x}.}
\item{...}{Additional arguments.  (see \code{?ascii.default}).}
}

