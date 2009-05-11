SweaveSyntaxAscii <- SweaveSyntaxNoweb

SweaveSyntaxAscii$docopt <- "^[[:space:]]*SweaveOpts:\\[([^]]*)\\]"
SweaveSyntaxAscii$trans$docopt <- "SweaveOpts:\\[\\1\\]"

SweaveSyntaxAscii$docexpr <- "Sexpr:\\[([^]]*)\\]"
SweaveSyntaxAscii$trans$docexpr <- "Sexpr:\\[\\1\\]"

SweaveSyntaxAscii$syntaxname <- "^[[:space:]]*SweaveSyntax:\\[([^]]*)\\]"
SweaveSyntaxAscii$trans$syntaxname <- "SweaveOpts:\\[SweaveSyntaxAscii]"

SweaveSyntaxAscii$input <- "^[[:space:]]*SweaveInput:\\[([^]]*)\\]"
SweaveSyntaxAscii$trans$input <- "SweaveInput:\\[\\1\\]"

