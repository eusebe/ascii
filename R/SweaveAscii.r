Asciidoc <- Sweave
formals(Asciidoc) <-  alist(file=, driver=RweaveAsciidoc, syntax=SweaveSyntaxNoweb, ...=)

T2t <- Sweave
formals(T2t) <-  alist(file=, driver=RweaveT2t, syntax=SweaveSyntaxNoweb, ...=)

ReST <- Sweave
formals(ReST) <-  alist(file=, driver=RweaveReST, syntax=SweaveSyntaxNoweb, ...=)

Org <- Sweave
formals(Org) <-  alist(file=, driver=RweaveOrg, syntax=SweaveSyntaxNoweb, ...=)
