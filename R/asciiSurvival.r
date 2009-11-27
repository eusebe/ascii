ascii.survdiff <- function (x, include.rownames = TRUE, include.colnames = TRUE, rownames = NULL, colnames = NULL, format = "f", digits = 2, decimal.mark = ".", na.print = "", caption = "", caption.level = "", width = 0, frame = "", grid = "", valign = "", header = TRUE, footer = FALSE, align = "", col.width = 1, style = "", tgroup = NULL, n.tgroup = NULL, talign = "", tvalign = "", tstyle = "", bgroup = NULL, n.bgroup = NULL, balign = "", bvalign = "", bstyle = "", lgroup = NULL, n.lgroup = NULL, lalign = "", lvalign = "", lstyle = "", rgroup = NULL, n.rgroup = NULL, ralign = "", rvalign = "", rstyle = "", ...){
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
      include.colnames = include.colnames, rownames = rownames, colnames = colnames,
      format = format, digits = digits, decimal.mark = decimal.mark, na.print = na.print,
      caption = caption, caption.level = caption.level, width = width, frame = frame,
      grid = grid, valign = valign, header = header, footer = footer, align = align,
      col.width = col.width, style = style,
      tgroup = tgroup, n.tgroup = n.tgroup, talign = talign,
      tvalign = tvalign, tstyle = tstyle,
      bgroup = bgroup, n.bgroup = n.bgroup, balign = balign,
      bvalign = bvalign, bstyle = bstyle,
      lgroup = lgroup, n.lgroup = n.lgroup, lalign = lalign,
      lvalign = lvalign, lstyle = lstyle,
      rgroup = rgroup, n.rgroup = n.rgroup, ralign = ralign,
      rvalign = rvalign, rstyle = rstyle)
  class(obj) <- c("ascii", "proto", "environment")
  return(obj)
}

ascii.survfit <- function (x, scale = 1, print.rmean = getOption("survfit.print.rmean"), rmean = getOption("survfit.rmean"), include.rownames = T, include.colnames = T, header = T, ...) {
    omit <- x$na.action
    na <- NULL
    if (length(omit))
         na <- ascii(list(naprint(omit)), list.type = "none")
    if (!missing(print.rmean) && is.logical(print.rmean) && missing(rmean)) {
        if (print.rmean)
            rmean <- "common"
        else rmean <- "none"
    }
    if (is.null(rmean)) {
        if (is.logical(print.rmean)) {
            if (print.rmean)
                rmean <- "common"
            else rmean <- "none"
        }
        else rmean <- "none"
    }
    if (is.numeric(rmean)) {
        if (is.null(x$start.time)) {
            if (rmean < min(x$time))
                stop("Truncation point for the mean is < smallest survival")
        }
        else if (rmean < x$start.time)
            stop("Truncation point for the mean is < smallest survival")
    }
    else {
        rmean <- match.arg(rmean, c("none", "common", "individual"))
        if (length(rmean) == 0)
            stop("Invalid value for rmean option")
    }
    temp <- survival:::survmean(x, scale = scale, rmean)
    mat <- ascii(temp$matrix, include.rownames = include.rownames, include.colnames = include.colnames, header = header, ...)

    restrm <- NULL
    if (rmean != "none") {
        if (rmean == "individual")
            restrm <- ascii(list("* restricted mean with variable upper limit"))
        else restrm <- ascii(list(paste("* restricted mean with upper limit = ",
            format(temp$end.time[1]))))
    }
    obj <- asciiMixed$new(na, mat, restrm)
    class(obj) <- c("ascii", "proto", "environment")
    obj
}

# from xtable package
ascii.coxph <- function (x, include.rownames = TRUE, include.colnames = TRUE, rownames = NULL, colnames = NULL, format = "f", digits = 2, decimal.mark = ".", na.print = "", caption = "", caption.level = "", width = 0, frame = "", grid = "", valign = "", header = TRUE, footer = FALSE, align = "", col.width = 1, style = "", tgroup = NULL, n.tgroup = NULL, talign = "", tvalign = "", tstyle = "", bgroup = NULL, n.bgroup = NULL, balign = "", bvalign = "", bstyle = "", lgroup = NULL, n.lgroup = NULL, lalign = "", lvalign = "", lstyle = "", rgroup = NULL, n.rgroup = NULL, ralign = "", rvalign = "", rstyle = "", ...){

    cox <- x
    beta <- cox$coef
    se <- sqrt(diag(cox$var))
    tmp <- cbind(beta, exp(beta), se, beta/se, 1 - pchisq((beta/se)^2, 1))
    dimnames(tmp) <- list(names(beta), c("coef", "exp(coef)", "se(coef)", "z", "p"))

    obj <- asciiDataFrame$new(x = as.data.frame(tmp), include.rownames = include.rownames,
      include.colnames = include.colnames, rownames = rownames, colnames = colnames,
      format = format, digits = digits, decimal.mark = decimal.mark, na.print = na.print,
      caption = caption, caption.level = caption.level, width = width, frame = frame,
      grid = grid, valign = valign, header = header, footer = footer, align = align,
      col.width = col.width, style = style,
      tgroup = tgroup, n.tgroup = n.tgroup, talign = talign,
      tvalign = tvalign, tstyle = tstyle,
      bgroup = bgroup, n.bgroup = n.bgroup, balign = balign,
      bvalign = bvalign, bstyle = bstyle,
      lgroup = lgroup, n.lgroup = n.lgroup, lalign = lalign,
      lvalign = lvalign, lstyle = lstyle,
      rgroup = rgroup, n.rgroup = n.rgroup, ralign = ralign,
      rvalign = rvalign, rstyle = rstyle)
  class(obj) <- c("ascii", "proto", "environment")
  return(obj)
}
