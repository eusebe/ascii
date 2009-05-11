SweaveSyntaxAsciidoc <- SweaveSyntaxNoweb

SweaveSyntaxAsciidoc$docopt <- "^[[:space:]]*SweaveOpts:\\[([^]]*)\\]"
SweaveSyntaxAsciidoc$trans$docopt <- "SweaveOpts:\\[\\1\\]"

SweaveSyntaxAsciidoc$docexpr <- "Sexpr:\\[([^]]*)\\]"
SweaveSyntaxAsciidoc$trans$docexpr <- "Sexpr:\\[\\1\\]"

SweaveSyntaxAsciidoc$syntaxname <- "^[[:space:]]*SweaveSyntax:\\[([^]]*)\\]"
SweaveSyntaxAsciidoc$trans$syntaxname <- "SweaveOpts:\\[SweaveSyntaxAsciidoc]"

SweaveSyntaxAsciidoc$input <- "^[[:space:]]*SweaveInput:\\[([^]]*)\\]"
SweaveSyntaxAsciidoc$trans$input <- "SweaveInput:\\[\\1\\]"

