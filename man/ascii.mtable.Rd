\name{ascii.mtable}
\alias{ascii.mtable}
\title{ascii method for class mtable...}
\usage{\method{ascii}{mtable}(x, lgroup=c(dimnames(x$coefficients)[[3]], rownames(x$summaries)),
    n.lgroup=c(rep(2, dim(x$coefficients)[x$as.row[2]]), rep(1,
    nrow(x$summaries))), include.rownames=FALSE, include.colnames=TRUE,
    ...)
}
\description{ascii method for class mtable}
\details{\code{memisc} package}
\value{An ascii object.}
\author{David Hajage}
\arguments{\item{x}{An R object of class found among \code{methods(ascii)}.}
\item{lgroup}{Character vector or list of character vectors defining major
left row headings.  The default is to have none (\code{NULL}).}
\item{n.lgroup}{A numeric vector containing the number of rows for which
each element in lgroup is a heading. Column names count in the row
numbers if \code{include.colnames = TRUE}.}
\item{include.rownames}{logical. If \code{TRUE} the rows names are printed.
Default value depends of class of \code{x}.}
\item{include.colnames}{logical. If \code{TRUE} the columns names are
printed. Default value depends of class of \code{x}.}
\item{...}{Additional arguments.  (see \code{?ascii.default}).}
}

