ascii.htest <- function (x, include.rownames = TRUE, include.colnames = TRUE, format = "f", digits = 2, decimal.mark = ".", na.print = "", caption = "", width = 0, frame = "", grid = "", valign = "", header = TRUE, footer = FALSE, align = "", col.width = 1, style = ""){
  if (x$method == "Fisher's Exact Test for Count Data") {
    res <- cbind(x$estimate, x$conf.int[1], x$conf.int[2], x$p.value)
    colnames(res) <- c("Odds ratio", "lower .95", "upper .95", "p-value")
  }
  if (x$method == "Pearson's Chi-squared test with Yates' continuity correction" | x$method == "Pearson's Chi-squared test" | x$method == "Chi-squared test for given probabilities"){
    res <- cbind(x$statistic, x$parameter, x$p.value)
    colnames(res) <- c("X-squared", "df", "p-value")
  }
  ascii(res, include.rownames = include.rownames,
         include.colnames = include.colnames, format = format,
         digits = digits, decimal.mark = decimal.mark, na.print = na.print,
         caption = caption, width = width, frame = frame, grid = grid,
         valign = valign, header = header, footer = footer, align = align,
         col.width = col.width, style = style)
}
