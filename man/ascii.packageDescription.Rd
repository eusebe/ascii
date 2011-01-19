\name{ascii.packageDescription}
\alias{ascii.packageDescription}
\title{ascii method for class packageDescription...}
\usage{\method{ascii}{packageDescription} (x, caption, caption.level,
    list.type="label", ...)}
\description{ascii method for class packageDescription}
\value{An ascii object.}
\author{David Hajage}
\arguments{\item{x}{An R object of class found among \code{methods(ascii)}.}
\item{caption}{Character vector of length 1 containing the table's caption
or title.  Set to \code{""} to suppress the caption.  Default value is
\code{NULL}.}
\item{caption.level}{Character or numeric vector of length 1 containing the
caption's level.  Can take the following values: \code{0} to \code{5},
\code{"."} (block titles in asciidoc markup), \code{"s"} (strong),
\code{"e"} (emphasis), \code{"m"} (monospaced) or \code{""} (no markup).
Default is NULL.}
\item{list.type}{Character vector of length one indicating the list type
(\code{"bullet"}, \code{"number"}, \code{"label"} or \code{"none"}). If
\code{"label"}, \code{names(list)} is used for labels. Default is
\code{"bullet"}.}
\item{...}{Additional arguments.  (Currently ignored.)}}
