# Asciidoc
RweaveAsciidoc <- function()
{
    list(setup = RweaveAsciiSetup,
         runcode = RweaveAsciiRuncode,
         writedoc = RweaveAsciiWritedoc,
         finish = RweaveAsciiFinish,
         checkopts = RweaveAsciiOptions)
}

# Txt2tags
RweaveT2t <- function()
{
    list(setup = RweaveT2tSetup,
         runcode = RweaveAsciiRuncode,
         writedoc = RweaveAsciiWritedoc,
         finish = RweaveAsciiFinish,
         checkopts = RweaveAsciiOptions)
}

RweaveT2tSetup <- RweaveAsciiSetup
formals(RweaveT2tSetup) <-alist(file=, syntax=, output=NULL, quiet=FALSE, debug=FALSE,
                                extension="t2t", backend="txt2tags", openSchunk="```",
                                closeSchunk="\n```\n", openSinput="", closeSinput="",
                                openSoutput="\n", closeSoutput="", indent="", openInclude ="%!include: ",
                                closeInclude=".t2t", openFig="[", closeFig="]", ...=)

# Org
RweaveOrg <- function()
{
    list(setup = RweaveOrgSetup,
         runcode = RweaveAsciiRuncode,
         writedoc = RweaveAsciiWritedoc,
         finish = RweaveAsciiFinish,
         checkopts = RweaveAsciiOptions)
}

RweaveOrgSetup <- RweaveAsciiSetup
formals(RweaveOrgSetup) <-alist(file=, syntax=, output=NULL, quiet=FALSE, debug=FALSE,
                                extension="org", backend="org-mode", openSchunk="#+BEGIN_SRC R-transcript",
                                closeSchunk="\n#+END_SRC\n", openSinput="", closeSinput="",
                                openSoutput="\n", closeSoutput="", indent="", openInclude ="#+INCLUDE: \"",
                                closeInclude=".org\"", openFig="[[file:", closeFig="]]", ...=)

# Pandoc
RweavePandoc <- function()
{
    list(setup = RweavePandocSetup,
         runcode = RweaveAsciiRuncode,
         writedoc = RweaveAsciiWritedoc,
         finish = RweaveAsciiFinish,
         checkopts = RweaveAsciiOptions)
}

RweavePandocSetup <- RweaveAsciiSetup
formals(RweavePandocSetup) <-alist(file=, syntax=, output=NULL, quiet=FALSE, debug=FALSE,
                                   extension="md", backend="pandoc", openSchunk="~~~~~~{.R}",
                                   closeSchunk="\n~~~~~~~~~~~\n\n", openSinput="", closeSinput="",
                                   openSoutput="\n", closeSoutput="", indent="", openInclude ="",
                                   closeInclude="", openFig="![](", closeFig=")", ...=)

# Textile
RweaveTextile <- function()
{
    list(setup = RweaveTextileSetup,
         runcode = RweaveAsciiRuncode,
         writedoc = RweaveAsciiWritedoc,
         finish = RweaveAsciiFinish,
         checkopts = RweaveAsciiOptions)
}

RweaveTextileSetup <- RweaveAsciiSetup
formals(RweaveTextileSetup) <-alist(file=, syntax=, output=NULL, quiet=FALSE, debug=FALSE,
                                    extension="txt", backend="textile", openSchunk="\nbc.. ",
                                    closeSchunk="\n\np. \n\n", openSinput="", closeSinput="",
                                    openSoutput="\n", closeSoutput="", indent="", openInclude ="",
                                    closeInclude="", openFig="!", closeFig="!", ...=)

# ReSTructuredText
RweaveReST <- function()
{
    list(setup = RweaveReSTSetup,
         runcode = RweaveAsciiRuncode,
         writedoc = RweaveAsciiWritedoc,
         finish = RweaveAsciiFinish,
         checkopts = RweaveAsciiOptions)
}

RweaveReSTSetup <- RweaveAsciiSetup
formals(RweaveReSTSetup) <-alist(file=, syntax=, output=NULL, quiet=FALSE, debug=FALSE,
                                 extension="rst", backend="docutils, sphinx, ...", openSchunk=".. code-block:: r\n",
                                 closeSchunk="\n\n", openSinput="", closeSinput="",
                                 openSoutput="\n", closeSoutput="", indent="  ", openInclude =".. include::",
                                 closeInclude=".rst", openFig=".. image:: ", closeFig="", ...=)
