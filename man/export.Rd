\name{export}
\alias{export}
\title{Report creation}
\usage{export(..., list, file, format, open, backend="asciidoc", encoding, options,
    cygwin=FALSE, title, author, email, date)
}
\description{Produce a report}
\details{Produce a report from a list of R objects. This function can be used directly,
or through a \code{Report} proto object (see examples). \code{Report$new()} creates
a new object, \code{Report$export()} produce a report. Options can be specified
with \code{Report$nameoftheoption <- option}. Special objects can be used to create
sections (see \code{?section}), paragraphs (see \code{?paragraph}) or inline results
(see \code{?sexpr}).}
\alias{Report}
\value{Nothing}
\author{David Hajage}
\arguments{\item{...}{R objects (not used if \code{"list"} is not NULL)}
\item{list}{list of R objects}
\item{file}{name of the output file (without extension)}
\item{format}{format of the output file}
\item{open}{open resulting file?}
\item{backend}{backend}
\item{encoding}{encoding}
\item{options}{other options}
\item{cygwin}{use cygwin?}
\item{title}{title of the report}
\item{author}{author of the report}
\item{email}{email of the author}
\item{date}{date}
}
\examples{\dontrun{
options(asciiType = "asciidoc")
export(head(esoph))

r <- Report$new(author = "David Hajage", email = "dhajage at gmail dot com")
r$add(section("First section"))
r$add(section("First subsection", 2))
r$add(paragraph("The data set has", sexpr(nrow(esoph)), " lines. See yourself:"), esoph)
r$add(section("Second subsection: age and alc group", 2))
tab <- with(esoph, table(alcgp, agegp))
r$add(ascii(tab), ascii(summary(tab), format = "nice"))
r$export()
r$format <- "slidy"
r$export()

r$title <- "R report example"
r$author <- "David Hajage"
r$email <- "dhajage at gmail dot com"
options(asciiType = "pandoc")
r$backend <- "pandoc"
r$format <- "odt"
r$export()

r$backend <- "markdown2pdf"
r$format <- "pdf"
r$export()
}}

