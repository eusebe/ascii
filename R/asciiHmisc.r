# From Hmisc package
ascii.describe.single <- function (x, condense = TRUE, ...) 
{
   wide <- .Options$width
    # des : le titre 
    des <- x$descript
    if (length(x$units)) 
      des <- paste(des, " \\[", x$units, "\\]", sep = "")
        if (length(x$format)) 
          des <- paste(des, "  Format:", x$format, sep = "")
            dim.counts <- dim(x$count)
            if (is.null(dim.counts)) {counts <-as.character(x$count)} else {
            counts <- matrix(as.character(x$count), dim.counts[1], dim.counts[2])}
            names(counts) <- names(x$count)
            counts <- ascii(counts, include.colnames = TRUE, caption = des)
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
  res <- asciiMixed$new(counts, val)
  class(res) <- c("ascii", "proto", "environment")
  return(res)
}



ascii.describe <- function (x, condense = TRUE, ...) 
{
  at <- attributes(x)
  xx <- lapply(x, ascii.describe.single, condense = condense)
    if (length(at$dimensions)) {
            res <- NULL
            for (z in 1:length(x)) {
              if (length(x[[z]]) == 0) next
              res <- asciiMixed$new(res, xx[[z]])
              class(res) <- c("ascii", "proto", "environment")
            }
    }
    else res <- ascii.describe.single(x, condense = condense)
  return(res)
}





