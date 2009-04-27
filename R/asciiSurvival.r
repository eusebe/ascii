ascii.survdiff <- function (x, include.rownames = TRUE, include.colnames = TRUE, format = "f", digits = 2, decimal.mark = ".", na.print = "", caption = "", caption.level = "", width = 0, frame = "", grid = "", valign = "", header = TRUE, footer = FALSE, align = "", col.width = 1, style = "", ...){
    # From print.survdiff
    if (length(x$n) == 1) {
        z <- sign(x$exp - x$obs) * sqrt(x$chisq)
        temp <- c(x$obs, x$exp, z, signif(1 - pchisq(x$chisq,
            1), digits))
        names(temp) <- c("Observed", "Expected", "Z", "p")
        temp <- t(temp)
        include.rownames = FALSE
    } else {
        if (is.matrix(x$obs)) {
            otmp <- apply(x$obs, 1, sum)
            etmp <- apply(x$exp, 1, sum)
        } else {
            otmp <- x$obs
            etmp <- x$exp
        }
        df <- c((sum(1 * (etmp > 0))) - 1, rep(NA, length(x$n) - 1))
        p <- c(1 - pchisq(x$chisq, df[!is.na(df)]), rep(NA, length(x$n) - 1))
        temp <- cbind(x$n, otmp, etmp, ((otmp - etmp)^2)/etmp,
            ((otmp - etmp)^2)/diag(x$var), df, p)
        dimnames(temp) <- list(names(x$n), c("N", "Observed",
            "Expected", "(O-E)^2/E", "(O-E)^2/V", "df", "p"))
    }

  temp <- as.data.frame(temp, checknames = FALSE)

  obj <- asciiDataFrame$new(x = temp, include.rownames = include.rownames,
      include.colnames = include.colnames, format = format,
      digits = digits, decimal.mark = decimal.mark, na.print = na.print,
      caption = caption, caption.level = caption.level, width = width, frame = frame, grid = grid,
      valign = valign, header = header, footer = footer, align = align,
      col.width = col.width, style = style)
  class(obj) <- c("ascii", "proto", "environment")
  return(obj)
}

# from xtable package
ascii.coxph <- function (x, include.rownames = TRUE, include.colnames = TRUE, format = "f", digits = 2, decimal.mark = ".", na.print = "", caption = "", caption.level = "", width = 0, frame = "", grid = "", valign = "", header = TRUE, footer = FALSE, align = "", col.width = 1, style = "", ...){

    cox <- x
    beta <- cox$coef
    se <- sqrt(diag(cox$var))
    tmp <- cbind(beta, exp(beta), se, beta/se, 1 - pchisq((beta/se)^2, 1))
    dimnames(tmp) <- list(names(beta), c("coef", "exp(coef)", "se(coef)", "z", "p"))

    obj <- asciiDataFrame$new(x = as.data.frame(tmp), include.rownames = include.rownames,
      include.colnames = include.colnames, format = format,
      digits = digits, decimal.mark = decimal.mark, na.print = na.print,
      caption = caption, caption.level = caption.level, width = width, frame = frame, grid = grid,
      valign = valign, header = header, footer = footer, align = align,
      col.width = col.width, style = style)
  class(obj) <- c("ascii", "proto", "environment")
  return(obj)
}
