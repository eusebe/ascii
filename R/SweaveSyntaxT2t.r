SweaveSyntaxT2t <- SweaveSyntaxNoweb

SweaveSyntaxT2t$docopt <- "^[[:space:]]*SweaveOpts:\\[([^]]*)\\]"
SweaveSyntaxT2t$trans$docopt <- "SweaveOpts:\\[\\1\\]"

SweaveSyntaxT2t$docexpr <- "Sexpr:\\[([^]]*)\\]"
SweaveSyntaxT2t$trans$docexpr <- "Sexpr:\\[\\1\\]"

SweaveSyntaxT2t$syntaxname <- "^[[:space:]]*SweaveSyntax:\\[([^]]*)\\]"
SweaveSyntaxT2t$trans$syntaxname <- "SweaveOpts:\\[SweaveSyntaxT2t]"

SweaveSyntaxT2t$input <- "^[[:space:]]*SweaveInput:\\[([^]]*)\\]"
SweaveSyntaxT2t$trans$input <- "SweaveInput:\\[\\1\\]"

