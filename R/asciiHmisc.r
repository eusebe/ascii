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
            counts <- ascii(x$count, include.colnames = T, caption = des)
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
                    val <- ascii(val)
                }
                else {
                  if (condense) {
                    low <- paste("lowest:", paste(val[1:5], collapse = " "))
                      hi <- paste("highest:", paste(val[6:10], collapse = " "))
                      if (nchar(low) + nchar(hi) + 2 > wide) 
                        val <- as.list(c(low, hi))
                      else val <- as.list(paste(low, hi, sep = ", "))
                    val <- ascii(val)
                  }
                  else {
                    val <- ascii(val)
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
                  val <- ascii(as.list(z))
                  }
                  else {
                      val <- ascii(val, include.rownames = T, include.colnames = T)
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
  xx <- lapply(x, ascii)
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





