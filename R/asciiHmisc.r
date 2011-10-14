# From Hmisc package

##' @export
##' @method ascii describe.single
ascii.describe.single <- function (x, condense = TRUE, ...) {
   wide <- .Options$width
    # des : le titre
    des <- x$descript
    if (length(x$units))
      des <- paste(des, " [", x$units, "]", sep = "")
        if (length(x$format))
          des <- paste(des, "  Format:", x$format, sep = "")
            dim.counts <- dim(x$count)
            if (is.null(dim.counts)) {counts <-as.character(x$count)} else {
            counts <- matrix(as.character(x$count), dim.counts[1], dim.counts[2])}
            names(counts) <- names(x$count)
            counts <- ascii(counts, include.colnames = TRUE, caption = des, caption.level = "s")
            val <- x$values
            if (length(val)) {
              if (!is.matrix(val)) {
                if (length(val) != 10 || !all(names(val) == c("L1",
                        "L2", "L3", "L4", "L5", "H5", "H4", "H3", "H2",
                        "H1"))) {
                  cat("\n")
                    val <- paste(names(val), ifelse(val > 1, paste(" (",
                            val, ")", sep = ""), ""), sep = "")
                    val <- strwrap(val, exdent = 4)
                    val <- as.list(sub('(^    )(.*)', '\t\\2', val))
                    val <- ascii(val, list.type = "none")
                }
                else {
                  if (condense) {
                    low <- paste("lowest:", paste(val[1:5], collapse = " "))
                      hi <- paste("highest:", paste(val[6:10], collapse = " "))
                      if (nchar(low) + nchar(hi) + 2 > wide)
                        val <- as.list(c(low, hi))
                      else val <- as.list(paste(low, hi, sep = ", "))
                    val <- ascii(val, list.type = "none")
                  }
                  else {
                    dim.val <- dim(val)
                    if (is.null(dim.val)) {val <- as.character(val)} else {
                    val <- matrix(as.character(val), dim.val[1], dim.val[2])}
                    names(val) <- names(x$values)
                    val <- ascii(val, include.colnames = TRUE)
                  }
                }
              }
              else {
                lev <- dimnames(val)[[2]]
                  if (condense && (mean(nchar(lev)) > 10 | length(lev) <
                        5)) {
                    z <- ""
                      len <- 0
                      for (i in 1:length(lev)) {
                        w <- paste(lev[i], " (", val[1, i], ", ", val[2,
                            i], "%)", sep = "")
                          if (i == 1) z <- w
                          else z <- paste(z, w, sep = ", ")
                  }
                  val <- ascii(as.list(z), list.type = "none")
                  }
                  else {
                    dim.val <- dim(val)
                    if (is.null(dim.val)) {val <- as.character(val)} else {
                    val <- matrix(as.character(val), dim.val[1], dim.val[2])}
                    rownames(val) <- rownames(x$values)
                    colnames(val) <- colnames(x$values)
                    val <- ascii(val, include.rownames = TRUE, include.colnames = TRUE)
                  }
              }
            }
#   if (length(x$mChoice)) {
#       print(x$mChoice, prlabel = FALSE)
#   }
  res <- asciiMixed$new(args = list(counts, val))
  return(res)
}

##' @param condense default is TRUE to condense the output with regard to the 5
##'   lowest and highest values and the frequency table (\code{describe()} in
##'   package \code{Hmisc}).
##' @export
##' @method ascii describe
##' @rdname ascii
ascii.describe <- function (x, condense = TRUE, ...) {
  at <- attributes(x)
  descrip <- ifelse(is.null(at$descript), "", at$descrip)
  if (is.null(at$dimensions[2])) {
    variable <- NULL
  } else {
    variable <- paste(at$dimensions[2], "Variable")
  }
  if (is.null(at$dimensions[1])) {
    observation <- NULL
  } else {
    observation <- paste(at$dimensions[1], "Observations")
  }
  if (!is.null(variable) | !is.null(observation) | descrip != "") {
    des <- ascii(list(variable, observation), caption = descrip, caption.level = NULL)
  } else {des <- NULL}
    if (length(at$dimensions)) {
      xx <- lapply(x, ascii.describe.single, condense = condense)
        res <- NULL
        for (z in 1:length(x)) {
          if (length(x[[z]]) == 0) next
          res <- asciiMixed$new(args = list(res, xx[[z]]))
        }
    }
    else res <- ascii.describe.single(x, condense = condense)

  if (length(at$naprint)) { na <- ascii(as.list(at$naprint)) ; res <- asciiMixed$new(args = list(des, na, res)) }
  else res <- asciiMixed$new(args = list(des, res))

  return(res)
}



##' @param vnames By default, tables and plots are usually labeled
##' with variable labels (see \code{summary.formula} in package
##' \code{Hmisc}).
##' @param prUnits set to \code{FALSE} to suppress printing or latexing
##' \code{units} attributes of variables (see \code{summary.formula} in
##' package \code{Hmisc}).
##' @export
##' @method ascii summary.formula.response
##' @rdname ascii
ascii.summary.formula.response <- function(x, vnames = c('labels', 'names'), prUnits = TRUE, lgroup = list(dimnames(stats)[[1]], if (ul) vlabels else at$vname[at$vname != ""]), n.lgroup = list(1, at$nlevels), include.rownames = FALSE, include.colnames = TRUE, format = "nice", caption = paste(at$ylabel, if(ns > 1) paste(' by', if(ul) at$strat.label else at$strat.name), ' N = ', at$n, if(at$nmiss) paste(', ', at$nmiss, ' Missing', sep=''), sep = ''), caption.level = "s", header = TRUE, ...) {
  stats <- x
  stats <- oldUnclass(stats)
  vnames <- match.arg(vnames)
  ul <- vnames=='labels'

  at <- attributes(stats)
  ns <- length(at$strat.levels)

  vlabels <- at$labels
  if(prUnits) {
    atu <- translate(at$units, '*',' ') ## 31jan03
    vlabels <- ifelse(atu == '',vlabels,   ## 28jan03
                      paste(vlabels,' [',atu,']',sep=''))
  }

  ascii(stats, lgroup = lgroup, n.lgroup = n.lgroup, include.colnames = include.colnames, include.rownames = include.rownames, format = "nice", caption = caption, caption.level = caption.level, header = header, ...)
}

##' @param prn set to \code{TRUE} to print the number of non-missing
##' observations on the current (row) variable (see
##' \code{summary.formula} in package \code{Hmisc}).
##' @param pctdig number of digits to the right of the decimal place
##' for printing percentages (see \code{summary.formula} in package
##' \code{Hmisc}).
##' @param npct specifies which counts are to be printed to the right
##' of percentages (see \code{summary.formula} in package
##' \code{Hmisc}).
##' @param exclude1 by default, \code{method="reverse"} objects will be
##' printed, plotted, or typeset by removing redundant entries from
##' percentage tables for categorical variables (see
##' \code{summary.formula} in package \code{Hmisc}).
##' @param sep character to use to separate quantiles when printing
##' \code{method="reverse"} tables (see \code{summary.formula} in package
##' \code{Hmisc}).
##' @param formatArgs a list containing other arguments to pass to
##' \code{format.default} (see \code{summary.formula} in package
##' \code{Hmisc}).
##' @param round Specify \code{round} to round the quantiles and
##' optional mean and standard deviation to \code{round} digits after
##' the decimal point (see \code{summary.formula} in package
##' \code{Hmisc}).
##' @param prtest a vector of test statistic components to print if
##' \code{test=TRUE} (see \code{summary.formula} in package
##' \code{Hmisc}).
##' @param prmsd set to \code{TRUE} to print mean and SD after the
##' three quantiles, for continuous variables (see
##' \code{summary.formula} in package \code{Hmisc}).
##' @param pdig number of digits to the right of the decimal place for
##' printing P-values. (see \code{summary.formula} in package
##' \code{Hmisc}).
##' @param eps P-values less than \code{eps} will be printed as
##' \code{< eps} (see \code{summary.formula} in package \code{Hmisc}).
##' @export
##' @method ascii summary.formula.reverse
##' @rdname ascii
ascii.summary.formula.reverse <- function(x, digits, prn = any(n != N), pctdig = 0, npct = c('numerator', 'both', 'denominator', 'none'), exclude1 = TRUE, vnames = c("labels","names"), prUnits = TRUE, sep = "/", formatArgs = NULL, round = NULL, prtest = c('P','stat','df','name'), prmsd = FALSE, pdig = 3, eps = 0.001, caption = paste("Descriptive Statistics", if(length(x$group.label)) paste(" by", x$group.label) else paste(" (N = ", x$N, ")", sep=""), sep = ""), caption.level = "s", include.rownames = FALSE, include.colnames = TRUE, colnames = gl, header = TRUE, lgroup = lgr, n.lgroup = n.lgr, rgroup = rgr, n.rgroup = n.rgr, rstyle = "d", ...) {
  long <- TRUE
  npct   <- match.arg(npct)
  vnames <- match.arg(vnames)
  if(is.logical(prtest) && !prtest)
    prtest <- 'none'

  stats  <- x$stats
  nv     <- length(stats)
  cstats <- lab <- character(0)
  nn     <- integer(0)
  type   <- x$type
  n      <- x$n
  N      <- x$N
  nams   <- names(stats)
  labels <- x$labels
  Units  <- x$units
  test   <- x$testresults
  if(!length(test))
    prtest <- 'none'

  nw     <- if(lg <- length(x$group.freq)) lg else 1  #23Nov98

  gnames <- names(x$group.freq)
  if(!missing(digits)) {    #.Options$digits <- digits 6Aug00
    oldopt <- options(digits=digits)
    on.exit(options(oldopt))
  }

  cstats <- lgroup1 <- lgroup2 <- n.lgroup1 <- n.lgroup2 <- rgroup1 <- n.rgroup1 <- NULL
  for(i in 1:nv) {
    nn <- c(nn, n[i])
    nam <- if(vnames=="names") nams[i] else labels[i]

    if(prUnits && nchar(Units[i]))
      nam <- paste(nam,' [',translate(Units[i],'*',' '),']',sep='')

    tr <- if(length(test) && all(prtest!='none')) test[[nams[i]]] else NULL

    rg <- NULL
    if(type[i] == 1 || type[i] == 3) {
      cs <- formatCats(stats[[i]], nam, tr, type[i],
                       if(length(x$group.freq)) x$group.freq else x$n[i],
                       npct, pctdig, exclude1, long, prtest,
                       pdig=pdig, eps=eps)
      if (!is.null(tr)) {
        rg <- cs[1, ncol(cs)]
        cs <- cs[, -ncol(cs)]
      }
      if (nrow(cs) > 1)
        cs <- cs[-1, , drop = FALSE]
      nn <- c(nn, rep(NA, nrow(cs)-1))

      lg1 <- ascii:::trim(rownames(cs))
      n.lg1 <- 1
      n.lg2 <- n.rg <- nrow(cs)
    } else {
      cs <- formatCons(stats[[i]], nam, tr, x$group.freq, prmsd,
                       sep, formatArgs, round, prtest,
                       pdig=pdig, eps=eps)
      if (!is.null(tr)) {
        rg <- cs[ncol(cs)]
        cs <- cs[-ncol(cs)]
      }
      lg1 <- ""
      n.lg1 <- n.lg2 <- n.rg <- 1
    }
    lgroup1 <- c(lgroup1, lg1)
    n.lgroup1 <- c(n.lgroup1, n.lg1)
    lgroup2 <- c(lgroup2, nam)
    n.lgroup2 <- c(n.lgroup2, n.lg2)
    rgroup1 <- c(rgroup1, rg)
    n.rgroup1 <- c(n.rgroup1, n.rg)
    cstats <- rbind(cstats, cs)
  }

  lgr <- list(lgroup1, lgroup2)
  n.lgr <- list(n.lgroup1, n.lgroup2)
  rgr <- rgroup1
  n.rgr <- n.rgroup1

  ## lab <- dimnames(cstats)[[1]]
  gl <- names(x$group.freq)
  gl <- if(length(gl)) paste(gl," (N = ", x$group.freq, ")", sep= "")  else ""


  ## ## Currently impossible with ascii to add a colname to rgroup
  ## if (length(test) && !all(prtest == 'none'))
  ##   gl <- c(gl,
  ##           if(length(prtest) == 1 && prtest != 'stat')
  ##           if(prtest == 'P') 'P-value'
  ##           else prtest
  ##           else 'Test Statistic')

  if(prn) {
    cnn <- format(nn)
    cnn[is.na(nn)] <- ''
    cstats <- cbind(cnn, cstats)
    gl <- c('N', gl)
  }

  ascii(cstats, include.colnames = include.colnames, include.rownames = include.rownames, colnames = gl, lgroup = lgroup, n.lgroup = n.lgroup, rgroup = rgroup, n.rgroup = n.rgroup, caption = caption, caption.level = caption.level, header = header, rstyle = rstyle, ...)
}

##' @param twoway controls whether the resulting table will be printed
##' in enumeration format or as a two-way table (the default) (see
##' \code{summary.formula} in package \code{Hmisc}).
##' @param prnmiss set to \code{FALSE} to suppress printing counts of
##' missing values
##' @export
##' @method ascii summary.formula.cross
##' @rdname ascii
ascii.summary.formula.cross <- function(x, twoway = nvar==2, prnmiss = any(stats$Missing > 0), prn = TRUE, formatArgs = NULL, caption = a$heading, caption.level = "s", include.rownames = FALSE, include.colnames = TRUE, header = TRUE, format = "nice", lgroup = v, n.lgroup = rep(length(z), length(v)), ...) {
  stats <- x
  a <- attributes(stats)
  attr(stats, 'class') <- NULL
  ylab <- attr(stats$S, "label")
  nvar <- length(a$Levels)
  vnames <- names(a$Levels)
  nam <- c(vnames, if(prn) "N", if(prnmiss) "Missing", "S")
  stats <- stats[nam]
  S <- stats$S
  ars <- length(dim(S))
  attr(stats,"row.names") <- rep("",length(a$row.names))
  if(twoway && nvar == 2) {
    V <- stats[[vnames[1]]]
    H <- stats[[vnames[2]]]
    v <- levels(V)
    h <- levels(H)
    z <- dimnames(stats$S)[[2]]
    if(!length(z))
      z <- ylab

    z <- c(if(prn) "N",
           if(prnmiss) "Missing",
           z)  # 5Oct00

    head <- ascii(as.list(z), caption = caption, caption.level = caption.level)

    d <- c(length(v),length(h),length(z))
    st <- array(NA, dim = d, dimnames = list(v, h, z))
    cstats <- array("", dim = d, dimnames=list(v, h, z))
    for(i in 1:length(V)) {
      j <- V == V[i, drop = FALSE] &  H == H[i, drop = FALSE]
      st[V[i, drop = FALSE], H[i, drop = FALSE],] <-
        c(if (prn) stats$N[j],
          if (prnmiss) stats$Missing[j],
          if (ars) S[j, ]
          else S[j])  # 5Oct00
    }

    for(k in 1:d[3]) {
      ww <- c(list(st[, , k]), formatArgs)  #10Feb00
      cstats[, , k] <- ifelse(is.na(st[, , k]), "", do.call('format', ww))
    }

    cstats2 <- do.call(interleave.matrix, lapply(apply(cstats, 3, list), function(x) x[[1]]))

    tab <- ascii(cstats2, include.rownames = include.rownames, include.colnames = include.colnames, header = header, lgroup = lgroup, n.lgroup = n.lgroup, ...)
    return(asciiMixed$new(args = list(head, tab)))
  }

  if(ars) {
    stats$S <- NULL
    snam <- dimnames(S)[[2]]
    for(i in 1:ncol(S))
      stats[[snam[i]]] <- S[, i]

  } else names(stats)[length(stats)] <- ylab

  stats <- as.data.frame(stats, stringsAsFactors = FALSE)
  return(ascii(stats, include.rownames = include.rownames, include.colnames = include.colnames, format = format, caption = caption, caption.level = caption.level, ...))
}
