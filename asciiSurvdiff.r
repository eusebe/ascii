setMethod(
  "ascii",
  "survdiff",
  function (x, include.rownames = TRUE, include.colnames = TRUE, format = "f", digits = 2, decimal.mark = ".", na.print = "", caption = "", width = 0, frame = "", grid = "", valign = "", header = TRUE, footer = FALSE, align = "", col.width = 1, style = ""){

  N <- x[[1]]
  n <- length(N)
  df <- c(n, rep(NA, n-1))
  obs <- x[[2]]
  exp <- x[[3]]
  var <- diag(x$var)
  c <- c(x$chisq, rep(NA, n-1))
  p <- c(1-pchisq(x$chisq, n-1), rep(NA, n-1))
  tmp <- cbind(N, obs, exp, (obs-exp)^2/exp, (obs-exp)^2/var, c, df, p)
  dimnames(tmp) <- list(names(N), c("N", "Obs", "Exp", "(O-E)^2/E", "(O-E)^2/V", "Chisq", "df", "p"))
  tmp <- as.data.frame(tmp, checknames = FALSE)
  new("R2asciidocDataFrame", x = tmp, include.rownames = include.rownames,
      include.colnames = include.colnames, format = format,
      digits = digits, decimal.mark = decimal.mark, na.print = na.print,
      caption = caption, width = width, frame = frame, grid = grid,
      valign = valign, header = header, footer = footer, align = align,
      col.width = col.width, style = style)
  }
)
