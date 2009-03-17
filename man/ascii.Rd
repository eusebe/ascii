\name{ascii}
\alias{ascii}
\alias{ascii.default}
\alias{ascii.anova}
\alias{ascii.aov}
\alias{ascii.aovlist}
\alias{ascii.cast_df}
\alias{ascii.character}
\alias{ascii.data.frame}
\alias{ascii.density}
\alias{ascii.factor}
\alias{ascii.glm}
\alias{ascii.htest}
\alias{ascii.integer}
\alias{ascii.lm}
\alias{ascii.matrix}
\alias{ascii.numeric}
\alias{ascii.summary.aov}
\alias{ascii.summary.aovlist}
\alias{ascii.summary.glm}
\alias{ascii.summary.lm}
\alias{ascii.summary.table}
\alias{ascii.survdiff}
\alias{ascii.tab1}
\alias{ascii.table}
\alias{package-ascii}
\title{Export R object to asciidoc or txt2tags}
\description{
  Function converting an R object to an \code{ascii} object, which can then be printed with Asciidoc or txt2tags syntax.
}
\usage{
  \method{ascii}{default}(x, 
    include.rownames = TRUE, include.colnames = TRUE, 
    format = "f", digits = 2, decimal.mark = ".", na.print = "", 
    caption = "", width = 0, frame = "", grid = "", valign = "", 
    header = TRUE, footer = FALSE, align = "", col.width = 1, style = "", \dots)
  \method{ascii}{summary.table}(x, caption = "", \dots)
}
\arguments{
  \item{x}{An R object of class found among \code{methods(ascii)}.}
  \item{include.rownames}{logical. If \code{TRUE} the rows names are printed. Default value depends of class of \code{x}.}
  \item{include.colnames}{logical. If \code{TRUE} the columns names are printed. Default value depends of class of \code{x}.}
  \item{format}{Character vector of length equal to the number of columns of the resulting table 
                (otherwise it will be replicated or truncated as necessary)
                indicating the format for the corresponding columns.
                These values are passed to the \code{formatC} function.  Use \code{"d"} (for integers),
                \code{"f"}, \code{"e"}, \code{"E"}, \code{"g"}, \code{"G"}, \code{"fg"} (for
                reals), or \code{"s"} (for strings).
                \code{"f"} gives numbers in the usual \code{xxx.xxx} format;  \code{"e"} and
                \code{"E"} give \code{n.ddde+nn} or \code{n.dddE+nn} (scientific format);
                \code{"g"} and \code{"G"} put \code{x[i]} into scientific format only if it saves
                space to do so.  \code{"fg"} uses fixed format as \code{"f"}, but \code{digits} as
                number of \emph{significant} digits.  Note that this can lead to
                quite long result strings.  Default depends on the class of \code{x}.}
  \item{digits}{Numeric vector of length equal to the number of columns of the resulting table 
                (otherwise it will be replicated or truncated as necessary)
                indicating the number of digits to display in the corresponding columns.
                Default is \code{2}.}
  \item{decimal.mark}{The character to be used to indicate the numeric decimal point. 
                      Default is \code{"."}.}
  \item{na.print}{The character string specifying how \code{NA} should be formatted specially. Default is "".}
  \item{caption}{Character vector of length 1 containing the table's caption or title.
                 Set to \code{""} to suppress the caption.  Default value is \code{""}.}
  \item{width}{Numeric vector of length one containing the table width
               relative to the available width (expressed as a percentage value, \code{1}\dots \code{99}).
               Default is \code{0} (all available width).}
  \item{frame}{Character vector of length one. Defines the table border, 
               and can take the following values: \code{"topbot"} (top and bottom), 
               \code{"all"} (all sides), \code{"none"} and \code{"sides"} (left and right).
               The default value is \code{""} (same as \code{"all"})}
  \item{grid}{Character vector of length one. Defines which ruler lines are drawn between table rows and columns, 
              and can take the following values: \code{"all"}, \code{"rows"}, \code{"cols"} and \code{"none"}. 
              Default is \code{""} (same as \code{"all"})}
  \item{valign}{Character vector of length one indicating vertical alignment of all cells in table. 
                Can take the following values: \code{"top"}, \code{"bottom"} and \code{"middle"}.
                Default is \code{""} (same as \code{"top"}.}
  \item{header}{logical. If \code{TRUE} the first line of the table is emphasized. The default value depends of class of \code{x}.}
  \item{footer}{logical. If \code{TRUE} the last line of the table is emphasized. The default value depends of class of \code{x}.}
  \item{align}{Character vector of length one indicating the alignment of the corresponding columns. 
               Can be composed with \code{"r"} (right), \code{"l"} (left) and \code{"c"} (center). 
               Default value is \code{""} (same as \code{"l"}).} 
  \item{col.width}{Numeric vector of length equal to the number of columns of the resulting table 
                   (otherwise it will be replicated or truncated as necessary)
                   indicating width of the corresponding columns (integer proportional values).
                   Default is \code{1}.}
  \item{style}{Character vector of length one indicating the style of the corresponding columns. 
               Can be composed with \code{"d"} (default), \code{"e"} (emphasis), \code{"m"} (monospaced), 
               \code{"a"} (cells can contain any of the AsciiDoc elements that are allowed inside document), 
               \code{"l"} (literal), \code{"v"} (verse; all line breaks are retained). 
               Default is \code{""} (same as \code{"d"}).}
  \item{...}{Additional arguments.  (Currently ignored.)}
}
\details{
  The nature of the output generated depends on the class of \code{x}.
  For example, \code{summary.table} objects produce
  a bulleted list while \code{data.frame} objects produce a table of the entire data.frame.

  All arguments are active when Asciidoc syntax is produced. Whith txt2tags, \code{grid}, 
  \code{valign}, \code{col.width} and \code{style} are inactive.

  The available method functions for \code{ascii} are given by \code{methods(ascii)}.
  Users can extend the list of available classes by writing methods for the generic function \code{ascii}.
  All method functions should return an object of class \code{c("ascii","proto","environment")}.
}
\value{
  This function returns an object of class \code{c("ascii","proto","environment")}.
}
\author{David Hajage \email{dhajage@gmail.com}}
\seealso{\code{\link{print.ascii}}}
\examples{
data(esoph)
ascii(esoph[1:10,])
tab <- table(esoph$agegp, esoph$alcgp)
ascii(tab)
print(ascii(tab), type = "t2t")
ascii(summary(tab))
}
\keyword{print}
