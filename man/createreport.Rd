\name{createreport}
\alias{createreport}
\alias{Report}
\title{Report creation}
\usage{
createreport(..., list = NULL, file = NULL, format = NULL, open = TRUE, backend = getOption("asciiBackend"), encoding = NULL, options = NULL, cygwin = FALSE, title = NULL, author = NULL, email = NULL, date = NULL)
}
\arguments{
  \item{...}{R objects (not used if \code{"list"} is not
  NULL)}

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
\value{
Nothing
}
\description{
Produce a report

Report generator
}
\details{
Produce a report from a list of R objects. This function can be
used directly, or through a \code{Report} object (see
examples). \code{Report$new()} creates a new object,
\code{Report$create()} produce a report. Exportation options can
be specified with \code{Report$nameoftheoption <- option} or
directly in \code{Report$create(nameoftheoption = option)}.

Special objects can be used to create sections (see
\code{?section}), paragraphs (see \code{?paragraph}), verbatim
environment (see \code{?verbatim} and to insert figures (see
\code{?fig}) or inline results (see \code{?sexpr}). Helpers exist:
\code{Report$addSection()}, \code{Report$addParagraph()},
\code{Report$addVerbatim()}, \code{Report$addFig()}.

It needs a working installation of asciidoc, a2x tool chain,
txt2tags, pandoc and/or markdown2pdf.
}
\examples{
\dontrun{
options(asciiType = "asciidoc")
createreport(head(esoph))

r <- Report$new(author = "David Hajage", email = "dhajage at gmail dot com")
r$add(section("First section"))
r$addSection("First subsection", 2)
r$add(paragraph("The data set has", sexpr(nrow(esoph)), " lines. See yourself:"), esoph)
r$addSection("Second subsection: age and alc group", 2)
tab <- with(esoph, table(alcgp, agegp))
r$add(ascii(tab), ascii(summary(tab), format = "nice"))
r$create()
r$format <- "slidy"
r$createt()

r$title <- "R report example"
r$author <- "David Hajage"
r$email <- "dhajage at gmail dot com"
options(asciiType = "pandoc")
r$backend <- "pandoc"
r$format <- "odt"
r$create()

r$create(backend = "markdown2pdf", format = "pdf")
}
}
\author{
David Hajage

David Hajage
}

