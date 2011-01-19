\name{ascii.freqtable}
\alias{ascii.freqtable}
\title{ascii method for class freqtable...}
\usage{\method{ascii}{freqtable} (x, header=TRUE, footer=TRUE, digits=c(0, 2,
    2), format="f", na.print="", include.rownames=TRUE,
    include.colnames=TRUE, caption=x$label, ...)}
\description{ascii method for class freqtable}
\details{\code{descr} package}
\value{An ascii object.}
\author{David Hajage}
\arguments{\item{x}{An R object of class found among \code{methods(ascii)}.}
\item{header}{logical or numeric. If \code{TRUE} or \code{1}, \code{2},
\dots{}, the first line(s) of the table is (are) emphasized. The default
value depends of class of \code{x}.}
\item{footer}{logical or numeric. If \code{TRUE} or \code{1}, the last
line(s) of the table is (are) emphasized. The default value depends of
class of \code{x}.}
\item{digits}{Numeric vector of length equal to the number of columns of
the resulting table (otherwise it will be replicated or truncated as
necessary) indicating the number of digits to display in the
corresponding columns.  Default is \code{2}.}
\item{format}{Character vector or matrix indicating the format for the
corresponding columns.  These values are passed to the \code{formatC}
function.  Use \code{"d"} (for integers), \code{"f"}, \code{"e"},
\code{"E"}, \code{"g"}, \code{"G"}, \code{"fg"} (for reals), or
\code{"s"} (for strings).  \code{"f"} gives numbers in the usual
\code{xxx.xxx} format; \code{"e"} and \code{"E"} give \code{n.ddde+nn} or
\code{n.dddE+nn} (scientific format); \code{"g"} and \code{"G"} put
\code{x[i]} into scientific format only if it saves space to do so.
\code{"fg"} uses fixed format as \code{"f"}, but \code{digits} as number
of \emph{significant} digits.  Note that this can lead to quite long
result strings. Finaly, \code{"nice"} is like \code{"f"}, but with 0
digits if \code{x} is an integer. Default depends on the class of
\code{x}.}
\item{na.print}{The character string specifying how \code{NA} should be
formatted specially. Default is "".}
\item{include.rownames}{logical. If \code{TRUE} the rows names are printed.
Default value depends of class of \code{x}.}
\item{include.colnames}{logical. If \code{TRUE} the columns names are
printed. Default value depends of class of \code{x}.}
\item{caption}{Character vector of length 1 containing the table's caption
or title.  Set to \code{""} to suppress the caption.  Default value is
\code{NULL}.}
\item{...}{Additional arguments.  (see \code{?ascii.default}).}}
