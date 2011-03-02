\name{export}
\alias{export}
\title{Report creation}
\usage{export(..., list, file, format, open=TRUE, backend=getOption("asciiBackend"),
    encoding, options, cygwin=FALSE, title, author, email, date)
}
\description{Produce a report}
\details{Produce a report from a list of R objects. This function can be
used directly, or through a \code{Report} proto object (see
examples). \code{Report$new()} creates a new object,
\code{Report$export()} produce a report. Exportation options can
be specified with \code{Report$nameoftheoption <- option} or
directly in \code{Report$export(nameoftheoption = option)}.

Special objects can be used to create sections (see
\code{?section}), paragraphs (see \code{?paragraph}), verbatim
environment (see \code{?verbatim} and to insert figures (see
\code{?fig}) or inline results (see \code{?sexpr}). Helpers exist:
\code{Report$addSection()}, \code{Report$addParagraph()},
\code{Report$addVerbatim()}, \code{Report$addFig()}.

It needs a working installation of asciidoc, a2x tool chain,
txt2tags, pandoc and/or markdown2pdf.}
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
r$addSection("First subsection", 2)
r$add(paragraph("The data set has", sexpr(nrow(esoph)), " lines. See yourself:"), esoph)
r$addSection("Second subsection: age and alc group", 2)
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

r$export(backend = "markdown2pdf", format = "pdf")
}}

