Asciidoc <- Sweave
formals(Asciidoc) <-  alist(file=, driver=RweaveAsciidoc, syntax=SweaveSyntaxNoweb, ...=)

T2t <- Sweave
formals(T2t) <-  alist(file=, driver=RweaveT2t, syntax=SweaveSyntaxNoweb, ...=)

Sphinx <- Sweave
formals(Sphinx) <-  alist(file=, driver=RweaveSphinx, syntax=SweaveSyntaxNoweb, ...=)

Org <- Sweave
formals(Org) <-  alist(file=, driver=RweaveOrg, syntax=SweaveSyntaxNoweb, ...=)
