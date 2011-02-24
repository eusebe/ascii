\name{ascii.meanscomp}
\alias{ascii.meanscomp}
\title{ascii method for class meanscomp...}
\usage{\method{ascii}{meanscomp}(x, header=TRUE, caption, include.rownames=TRUE, include.colnames=TRUE,
    ...)
}
\description{ascii method for class meanscomp}
\details{\code{descr} package}
\value{An ascii object.}
\author{David Hajage}
\arguments{\item{x}{An R object of class found among \code{methods(ascii)}.}
\item{header}{logical or numeric. If \code{TRUE} or \code{1}, \code{2},
\dots{}, the first line(s) of the table is (are) emphasized. The default
value depends of class of \code{x}.}
\item{caption}{Character vector of length 1 containing the table's caption
or title.  Set to \code{""} to suppress the caption.  Default value is
\code{NULL}.}
\item{include.rownames}{logical. If \code{TRUE} the rows names are printed.
Default value depends of class of \code{x}.}
\item{include.colnames}{logical. If \code{TRUE} the columns names are
printed. Default value depends of class of \code{x}.}
\item{...}{Additional arguments.  (see \code{?ascii.default}).}
}

