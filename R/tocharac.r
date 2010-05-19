safe.as.character <- function(x) {
  # preserves dim of x
  if (is.null(dim(x))) {
    x <- as.character(x)
  } else if (nrow(x) == 1) {
    x <- t(as.character(x))
  } else if (ncol(x) == 1) {
    x <- t(t(as.character(x)))
  } else {
    x <-  sapply(as.data.frame(x, stringAsFactor = FALSE, check.names = FALSE), as.character)
  }
  x
}

trim <- function (x) {
  x <- sub("^ +", "", x)
  x <- sub(" +$", "", x)
  x
}

formatCustom <- function(x, format = "nice", digits = 2, ...) {
  sapply(x, function(x) {
    if (format != "nice") {
      formatC(x, format = format, digits = digits, ...)
    } else {
      if (as.integer(x) != x | is.na(x)) {
        formatC(x, format = "f", digits = digits, ...)
      } else {
        formatC(x, format = "f", digits = 0, ...)
      }
    }
  })
}

tocharac <- function(x, include.rownames = FALSE, include.colnames = FALSE, rownames = NULL, colnames = NULL, format = "f", digits = 2, decimal.mark = ".", na.print = "") {
  if (is.factor(x))
    x <- as.character(x)
  if (is.vector(x))
    x <- t(x)

  numerics <- sapply(as.data.frame(x), is.numeric)
  
  format <- expand(format, nrow(x), ncol(x), drop = FALSE)
  digits <- expand(digits, nrow(x), ncol(x), drop = FALSE)
  
  rn <- rep(rownames, length = nrow(x))
  cn <- rep(colnames, length = ncol(x))
  if (include.rownames & is.null(rn)) {
    rn <- rownames(x, do.NULL = F, prefix = "")
  }
  if (include.colnames & is.null(cn)) {
    cn <- colnames(x, do.NULL = F, prefix = "")
  }

  xx <- safe.as.character(x)
  
  for (i in 1:ncol(xx)) {
    if (numerics[i]) {
      xx[,i] <- apply(as.matrix(as.numeric(xx[,i])), 2, Vectorize(formatCustom), digits = digits[,i], format = format[,i], decimal.mark = decimal.mark)
    }
    xx[,i] <- trim(xx[, i]) 
  }
  xx[is.na(xx)] <- na.print
  xx[xx == "NA"] <- na.print
  xx[xx == "NaN"] <- na.print
  
  if (include.colnames) {
    xx <- rbind(cn, xx)
  }
  if (include.rownames) {
    if (include.colnames)
      xx <- cbind(c("", rn), xx)
    if (!include.colnames)
      xx <- cbind(rn, xx)
  }

  names(xx) <- NULL
  rownames(xx) <- NULL
  colnames(xx) <- NULL

  xx
}
