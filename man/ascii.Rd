\name{ascii.default}
\alias{ascii}
\alias{ascii.anova}
\alias{ascii.aov}
\alias{ascii.aovlist}
\alias{ascii.cast_df}
\alias{ascii.character}
\alias{ascii.coxph}
\alias{ascii.CrossTable}
\alias{ascii.data.frame}
\alias{ascii.default}
\alias{ascii.density}
\alias{ascii.describe}
\alias{ascii.describe.single}
\alias{ascii.factor}
\alias{ascii.freqtable}
\alias{ascii.ftable}
\alias{ascii.glm}
\alias{ascii.htest}
\alias{ascii.integer}
\alias{ascii.list}
\alias{ascii.lm}
\alias{ascii.matrix}
\alias{ascii.meanscomp}
\alias{ascii.mtable}
\alias{ascii.numeric}
\alias{ascii.packageDescription}
\alias{ascii.prcomp}
\alias{ascii.sessionInfo}
\alias{ascii.simple.list}
\alias{ascii.smooth.spline}
\alias{ascii.stat.table}
\alias{ascii.summary.aov}
\alias{ascii.summary.aovlist}
\alias{ascii.summary.formula.cross}
\alias{ascii.summary.formula.response}
\alias{ascii.summary.formula.reverse}
\alias{ascii.summary.glm}
\alias{ascii.summary.lm}
\alias{ascii.summary.prcomp}
\alias{ascii.summary.survfit}
\alias{ascii.summary.table}
\alias{ascii.survdiff}
\alias{ascii.survfit}
\alias{ascii.table}
\alias{ascii.ts}
\alias{ascii.zoo}
\alias{"package-ascii"}
\title{Export R objects to several markup languages}
\usage{
\method{ascii}{default}(x, include.rownames = TRUE, include.colnames = TRUE, rownames = NULL, colnames = NULL, format = "f", digits = 2, decimal.mark = ".", na.print = "", caption = NULL, caption.level = NULL, width = 0, frame = NULL, grid = NULL, valign = NULL, header = TRUE, footer = FALSE, align = NULL, col.width = 1, style = NULL, tgroup = NULL, n.tgroup = NULL, talign = "c", tvalign = "middle", tstyle = "h", bgroup = NULL, n.bgroup = NULL, balign = "c", bvalign = "middle", bstyle = "h", lgroup = NULL, n.lgroup = NULL, lalign = "c", lvalign = "middle", lstyle = "h", rgroup = NULL, n.rgroup = NULL, ralign = "c", rvalign = "middle", rstyle = "h", list.type = "bullet", ...)

\method{ascii}{describe}(x, condense = TRUE, ...)

\method{ascii}{summary.formula.response}(x, vnames = c("labels", "names"), prUnits = TRUE, lgroup = list(dimnames(stats)[[1]], if (ul) vlabels else at$vname[at$vname != ""]), n.lgroup = list(1, at$nlevels), include.rownames = FALSE, include.colnames = TRUE, format = "nice", caption = paste(at$ylabel, if (ns > 1) paste(" by", if (ul) at$strat.label else at$strat.name), " N = ", at$n, if (at$nmiss) paste(", ", at$nmiss, " Missing", sep = ""), sep = ""), caption.level = "s", header = TRUE, ...)

\method{ascii}{summary.formula.reverse}(x, digits, prn = any(n != N), pctdig = 0, npct = c("numerator", "both", "denominator", "none"), exclude1 = TRUE, vnames = c("labels", "names"), prUnits = TRUE, sep = "/", formatArgs = NULL, round = NULL, prtest = c("P", "stat", "df", "name"), prmsd = FALSE, pdig = 3, eps = 0.001, caption = paste("Descriptive Statistics", if (length(x$group.label)) paste(" by", x$group.label) else paste(" (N = ", x$N, ")", sep = ""), sep = ""), caption.level = "s", include.rownames = FALSE, include.colnames = TRUE, colnames = gl, header = TRUE, lgroup = lgr, n.lgroup = n.lgr, rgroup = rgr, n.rgroup = n.rgr, rstyle = "d", ...)

\method{ascii}{summary.formula.cross}(x, twoway = nvar == 2, prnmiss = any(stats$Missing > 0), prn = TRUE, formatArgs = NULL, caption = a$heading, caption.level = "s", include.rownames = FALSE, include.colnames = TRUE, header = TRUE, format = "nice", lgroup = v, n.lgroup = rep(length(z), length(v)), ...)

\method{ascii}{sessionInfo}(x, locale = TRUE, ...)

\method{ascii}{survfit}(x, scale = 1, print.rmean = getOption("survfit.print.rmean"), rmean = getOption("survfit.rmean"), include.rownames = TRUE, include.colnames = TRUE, header = TRUE, ...)

ascii(x, ...)
}
\arguments{
  \item{x}{An R object of class found among
  \code{methods(ascii)}. If \code{x} is a list, it should
  be a list of character strings (it will produce a
  bulleted list output by default).}

  \item{include.rownames}{logical. If \code{TRUE} the rows
  names are printed. Default value depends of class of
  \code{x}.}

  \item{include.colnames}{logical. If \code{TRUE} the
  columns names are printed. Default value depends of class
  of \code{x}.}

  \item{rownames}{Character vector (replicated or truncated
  as necessary) indicating rownames of the corresponding
  rows. If \code{NULL} (default) the row names are not
  modified}

  \item{colnames}{Character vector (replicated or truncated
  as necessary) indicating colnames of the corresponding
  columns. If \code{NULL} (default) the column names are
  not modified}

  \item{format}{Character vector or matrix indicating the
  format for the corresponding columns.  These values are
  passed to the \code{formatC} function.  Use \code{"d"}
  (for integers), \code{"f"}, \code{"e"}, \code{"E"},
  \code{"g"}, \code{"G"}, \code{"fg"} (for reals), or
  \code{"s"} (for strings).  \code{"f"} gives numbers in
  the usual \code{xxx.xxx} format; \code{"e"} and
  \code{"E"} give \code{n.ddde+nn} or \code{n.dddE+nn}
  (scientific format); \code{"g"} and \code{"G"} put
  \code{x[i]} into scientific format only if it saves space
  to do so. \code{"fg"} uses fixed format as \code{"f"},
  but \code{digits} as number of \emph{significant} digits.
  Note that this can lead to quite long result strings.
  Finaly, \code{"nice"} is like \code{"f"}, but with 0
  digits if \code{x} is an integer. Default depends on the
  class of \code{x}.}

  \item{digits}{Numeric vector of length equal to the
  number of columns of the resulting table (otherwise it
  will be replicated or truncated as necessary) indicating
  the number of digits to display in the corresponding
  columns.  Default is \code{2}.}

  \item{decimal.mark}{The character to be used to indicate
  the numeric decimal point.  Default is \code{"."}.}

  \item{na.print}{The character string specifying how
  \code{NA} should be formatted specially. Default is "".}

  \item{caption}{Character vector of length 1 containing
  the table's caption or title.  Set to \code{""} to
  suppress the caption.  Default value is \code{NULL}.}

  \item{caption.level}{Character or numeric vector of
  length 1 containing the caption's level.  Can take the
  following values: \code{0} to \code{5}, \code{"."} (block
  titles in asciidoc markup), \code{"s"} (strong),
  \code{"e"} (emphasis), \code{"m"} (monospaced) or
  \code{""} (no markup). Default is NULL.}

  \item{width}{Numeric vector of length one containing the
  table width relative to the available width (expressed as
  a percentage value, \code{1}\dots{} \code{99}).  Default
  is \code{0} (all available width).}

  \item{frame}{Character vector of length one. Defines the
  table border, and can take the following values:
  \code{"topbot"} (top and bottom), \code{"all"} (all
  sides), \code{"none"} and \code{"sides"} (left and
  right).  The default value is \code{NULL}.}

  \item{grid}{Character vector of length one. Defines which
  ruler lines are drawn between table rows and columns, and
  can take the following values: \code{"all"},
  \code{"rows"}, \code{"cols"} and \code{"none"}.  Default
  is \code{NULL}.}

  \item{valign}{Vector or matrix indicating vertical
  alignment of all cells in table.  Can take the following
  values: \code{"top"}, \code{"bottom"} and
  \code{"middle"}.  Default is \code{""}.}

  \item{header}{logical or numeric. If \code{TRUE} or
  \code{1}, \code{2}, \dots{}, the first line(s) of the
  table is (are) emphasized. The default value depends of
  class of \code{x}.}

  \item{footer}{logical or numeric. If \code{TRUE} or
  \code{1}, the last line(s) of the table is (are)
  emphasized. The default value depends of class of
  \code{x}.}

  \item{align}{Vector or matrix indicating the alignment of
  the corresponding columns.  Can be composed with
  \code{"r"} (right), \code{"l"} (left) and \code{"c"}
  (center).  Default value is \code{NULL}.}

  \item{col.width}{Numeric vector of length equal to the
  number of columns of the resulting table (otherwise it
  will be replicated or truncated as necessary) indicating
  width of the corresponding columns (integer proportional
  values).  Default is \code{1}.}

  \item{style}{Character vector or matrix indicating the
  style of the corresponding columns.  Can be composed with
  \code{"d"} (default), \code{"s"} (strong), \code{"e"}
  (emphasis), \code{"m"} (monospaced), \code{"h"} (header)
  \code{"a"} (cells can contain any of the AsciiDoc
  elements that are allowed inside document), \code{"l"}
  (literal), \code{"v"} (verse; all line breaks are
  retained).  Default is \code{NULL}.}

  \item{tgroup}{Character vector or a list of character
  vectors defining major top column headings.  The default
  is to have none (\code{NULL}).}

  \item{n.tgroup}{A numeric vector or a list of numeric
  vectors containing the number of columns for which each
  element in tgroup is a heading.  For example, specify
  \code{tgroup=c("Major 1","Major 2")},
  \code{n.tgroup=c(3,3)} if \code{"Major 1"} is to span
  columns 1-3 and \code{"Major 2"} is to span columns 4-6.}

  \item{talign}{Character vector of length one defining
  alignment of major top column headings.}

  \item{tvalign}{Character vector of length one defining
  vertical alignment of major top column headings.}

  \item{tstyle}{Character vector of length one indicating
  the style of major top column headings}

  \item{bgroup}{Character vector or list of character
  vectors defining major bottom column headings.  The
  default is to have none (\code{NULL}).}

  \item{n.bgroup}{A numeric vector containing the number of
  columns for which each element in bgroup is a heading.}

  \item{balign}{Character vector of length one defining
  alignment of major bottom column headings.}

  \item{bvalign}{Character vector of length one defining
  vertical alignment of major bottom column headings.}

  \item{bstyle}{Character vector of length one indicating
  the style of major bottom column headings}

  \item{lgroup}{Character vector or list of character
  vectors defining major left row headings.  The default is
  to have none (\code{NULL}).}

  \item{n.lgroup}{A numeric vector containing the number of
  rows for which each element in lgroup is a heading.
  Column names count in the row numbers if
  \code{include.colnames = TRUE}.}

  \item{lalign}{Character vector of length one defining
  alignment of major left row headings.}

  \item{lvalign}{Character vector of length one defining
  vertical alignment of major left row headings.}

  \item{lstyle}{Character vector of length one indicating
  the style of major left row headings}

  \item{rgroup}{Character vector or list of character
  vectors defining major right row headings.  The default
  is to have none (\code{NULL}).}

  \item{n.rgroup}{A numeric vector containing the number of
  rows for which each element in rgroup is a heading.
  Column names count in the row numbers if
  \code{include.colnames = TRUE}.}

  \item{ralign}{Character vector of length one defining
  alignment of major right row headings.}

  \item{rvalign}{Character vector of length one defining
  vertical alignment of major right row headings.}

  \item{rstyle}{Character vector of length one indicating
  the style of major right row headings}

  \item{list.type}{Character vector of length one
  indicating the list type (\code{"bullet"},
  \code{"number"}, \code{"label"} or \code{"none"}). If
  \code{"label"}, \code{names(list)} is used for labels.
  Default is \code{"bullet"}.}

  \item{...}{Additional arguments. (Currently ignored.)}

  \item{condense}{default is TRUE to condense the output
  with regard to the 5 lowest and highest values and the
  frequency table (\code{describe()} in package
  \code{Hmisc}).}

  \item{vnames}{By default, tables and plots are usually
  labeled with variable labels (see \code{summary.formula}
  in package \code{Hmisc}).}

  \item{prUnits}{set to \code{FALSE} to suppress printing
  or latexing \code{units} attributes of variables (see
  \code{summary.formula} in package \code{Hmisc}).}

  \item{prn}{set to \code{TRUE} to print the number of
  non-missing observations on the current (row) variable
  (see \code{summary.formula} in package \code{Hmisc}).}

  \item{pctdig}{number of digits to the right of the
  decimal place for printing percentages (see
  \code{summary.formula} in package \code{Hmisc}).}

  \item{npct}{specifies which counts are to be printed to
  the right of percentages (see \code{summary.formula} in
  package \code{Hmisc}).}

  \item{exclude1}{by default, \code{method="reverse"}
  objects will be printed, plotted, or typeset by removing
  redundant entries from percentage tables for categorical
  variables (see \code{summary.formula} in package
  \code{Hmisc}).}

  \item{sep}{character to use to separate quantiles when
  printing \code{method="reverse"} tables (see
  \code{summary.formula} in package \code{Hmisc}).}

  \item{formatArgs}{a list containing other arguments to
  pass to \code{format.default} (see \code{summary.formula}
  in package \code{Hmisc}).}

  \item{round}{Specify \code{round} to round the quantiles
  and optional mean and standard deviation to \code{round}
  digits after the decimal point (see
  \code{summary.formula} in package \code{Hmisc}).}

  \item{prtest}{a vector of test statistic components to
  print if \code{test=TRUE} (see \code{summary.formula} in
  package \code{Hmisc}).}

  \item{prmsd}{set to \code{TRUE} to print mean and SD
  after the three quantiles, for continuous variables (see
  \code{summary.formula} in package \code{Hmisc}).}

  \item{pdig}{number of digits to the right of the decimal
  place for printing P-values. (see \code{summary.formula}
  in package \code{Hmisc}).}

  \item{eps}{P-values less than \code{eps} will be printed
  as \code{< eps} (see \code{summary.formula} in package
  \code{Hmisc}).}

  \item{twoway}{controls whether the resulting table will
  be printed in enumeration format or as a two-way table
  (the default) (see \code{summary.formula} in package
  \code{Hmisc}).}

  \item{prnmiss}{set to \code{FALSE} to suppress printing
  counts of missing values}

  \item{locale}{show locale information?}

  \item{scale}{A numeric value to rescale the survival
  time, e.g., if the input data to survfit were in days,
  \code{scale=365} would scale the printout to years (see
  \code{print.survfit()} in package \code{survival}).}

  \item{print.rmean}{Option for computation and display of
  the restricted mean (see \code{print.survfit()} in
  package \code{survival}).}

  \item{rmean}{Option for computation and display of the
  restricted mean (see \code{print.survfit()} in package
  \code{survival}).}
}
\value{
This function returns an object of class
\code{"asciiTable"}, \code{"asciiList"} or \code{"asciiMixed"}.
}
\description{
Convert an R object to an \code{ascii} object, which can then be
printed with asciidoc, txt2tags, reStructuredText, org, textile or
pandoc syntax.
}
\details{
The nature of the generated output depends on the class of
\code{x}.  For example, \code{summary.table} objects produce a
bulleted list while \code{data.frame} objects produce a table of
the entire data.frame.

Sometimes, arguments are not active, depending of the features
implemented in the markup language generated. All arguments are
active when asciidoc syntax is produced.

The available method functions for \code{ascii} are given by
\code{methods(ascii)}.  Users can extend the list of available
classes by writing methods for the generic function \code{ascii}.
All method functions should return an object of class
\code{"ascii"}.
}
\examples{
data(esoph)
ascii(esoph[1:10,])
tab <- table(esoph$agegp, esoph$alcgp)
ascii(tab)
print(ascii(tab), type = "t2t")
print(ascii(tab), type = "rest")
print(ascii(tab), type = "org")
ascii(summary(tab))
}
\author{
David Hajage \email{dhajage@gmail.com}
}
\keyword{print}

