##' ascii method for class meanscomp
##'
##' \code{descr} package
##' @param x An R object of class found among \code{methods(ascii)}.
##' @param header logical or numeric. If \code{TRUE} or \code{1}, \code{2},
##'   \dots{}, the first line(s) of the table is (are) emphasized. The default
##'   value depends of class of \code{x}.
##' @param caption Character vector of length 1 containing the table's caption
##'   or title.  Set to \code{""} to suppress the caption.  Default value is
##'   \code{NULL}.
##' @param include.rownames logical. If \code{TRUE} the rows names are printed.
##'   Default value depends of class of \code{x}.
##' @param include.colnames logical. If \code{TRUE} the columns names are
##'   printed. Default value depends of class of \code{x}.
##' @param ... Additional arguments.  (see \code{?ascii.default}).
##' @return An ascii object.
##' @S3method ascii meanscomp
##' @author David Hajage
ascii.meanscomp <- function (x, header = TRUE, caption = NULL, include.rownames = TRUE, include.colnames = TRUE, ...) {                 
    rlab <- ifelse(is.null(x$row.label), x$row, x$row.label)
    clab <- ifelse(is.null(x$column.label), x$column, x$column.label)
    msg1 <- gettext("Mean value of", domain = "R-descr")             
    msg2 <- gettext("according to", domain = "R-descr")              
    msg <- switch(1 + (caption == ""), caption, paste(msg1, " \"", clab, "\" ", msg2, " \"", rlab, "\"", sep = ""))
    ascii(x$table, caption = msg, include.rownames = include.rownames, include.colnames = include.colnames, header = header, ...)
}

##' ascii methodd for class CrossTable
##'
##' \code{descr} package
##' @return An ascii object.
##' @S3method ascii CrossTable
##' @author David Hajage
##' @param x A crosstable object
##' @param ... Additional arguments.  (see \code{?ascii.default}).
ascii.CrossTable <- function (x, ...) {
  t <- x$t      
  CPR <- x$prop.row
  CPC <- x$prop.col
  CPT <- x$prop.tbl
  GT <- x$gt       
  RS <- x$rs       
  CS <- x$cs       
  TotalN <- x$total.n
  CST <- x$chisq     
  CSTc <- x$chisq.corr
  FTt <- x$fisher.ts  
  FTl <- x$fisher.lt  
  FTg <- x$fisher.gt  
  McN <- x$mcnemar    
  McNc <- x$mcnemar.corr
  ASR <- x$asr          
  RowData <- x$RowData  
  ColData <- x$ColData  
  digits <- x$digits    
  max.width <- x$max.width
  vector.x <- x$vector.x  
  expected <- x$expected  
  prop.r <- (is.na(CPR[1]) == FALSE)
  prop.c <- (is.na(CPC[1]) == FALSE)
  prop.t <- (is.na(CPT[1]) == FALSE)
  chisq <- (is.na(CST[1]) == FALSE) 
  prop.chisq <- x$prop.chisq        
  fisher <- (class(FTt) == "htest") 
  resid <- x$resid                  
  sresid <- x$sresid                
  asresid <- x$asresid              
  mcnemar <- x$print.mcnemar        
  missing.include <- x$missing.include
  format <- x$format                  
  outDec <- getOption("OutDec")       
  nsep <- "  | "                      
  if (format == "SAS") {              
    resid <- sresid <- asresid <- FALSE
    hdd <- 1                           
    psep <- "  | "                     
  } else {                                 
    if (format == "SPSS") {            
      hdd <- 100                     
      psep <- "% | "                 
    } else {                             
      stop("unknown format")         
    }                                  
  }                                      
  if (vector.x)                          
    expected <- prop.chisq <- prop.c <- prop.t <- resid <- sresid <- asresid <- FALSE
  ColTotal <- gettext("Total", domain = "R-descr")                                     
  RowTotal <- ColTotal                                                                 
  CWidth <- max(digits + 2, c(nchar(t), nchar(dimnames(t)[[2]]),                       
                              nchar(RS), nchar(CS), nchar(RowTotal)))                                          
  RWidth <- max(c(nchar(dimnames(t)[[1]]), nchar(ColTotal)))                           
  if (is.na(RowData) == FALSE)                                                         
    RWidth <- max(RWidth, nchar(RowData))                                            
  RowSep <- paste(rep("-", CWidth + 2), collapse = "")                                 
  RowSep1 <- paste(rep("-", RWidth + 1), collapse = "")                                
  SpaceSep1 <- paste(rep(" ", RWidth), collapse = "")                                  
  SpaceSep2 <- paste(rep(" ", CWidth), collapse = "")                                  
  FirstCol <- formatC(dimnames(t)[[1]], width = RWidth, format = "s")                  
  ColTotal <- formatC(ColTotal, width = RWidth, format = "s")                          
  RowTotal <- formatC(RowTotal, width = CWidth, format = "s")                          
  caption <- gettext("Cell Contents", domain = "R-descr")
  if (format == "SAS") {                                                               
    content <- c("N", 
                 ifelse(expected, "Expected N", NA), 
                 ifelse(prop.chisq, "Chi-square contribution", NA), 
                 ifelse(prop.r, "N / Row Total", NA), 
                 ifelse(prop.c, "N / Col Total", NA), 
                 ifelse(prop.t, "N / Table Total", NA)
                 )
    content <- as.list(content[!is.na(content)])
  } else if (format == "SPSS") {                                                         
    content <- c("Count", 
                 ifelse(expected, "Expected Values", NA), 
                 ifelse(prop.chisq, "Chi-square contribution", NA), 
                 ifelse(prop.r, "Row Percent", NA), 
                 ifelse(prop.c, "Column Percent", NA), 
                 ifelse(prop.t, "Total Percent", NA), 
                 ifelse(resid, "Residual", NA), 
                 ifelse(sresid, "Std Residual", NA), 
                 ifelse(asresid, "Adj Std Resid", NA)
                 )
    content <- as.list(content[!is.na(content)])
  }                                                                                    
  if (vector.x) {                                                                      
    stop("For single vector description, use freq() function in package descr.\n")
  }                                                                                               
  nelements <- 1 + expected + prop.chisq + prop.r + prop.c +                                      
    prop.t + resid + sresid + asresid                                                        
  nr <- nrow(t) * nelements + 1 + prop.c                                                           
  nc <- ncol(t)                                                                                   
  m <- matrix(nrow = nr, ncol = nc + 1)                                                           
  rnames <- rownames(t)                                              
  n.rnames <- vector(mode = "numeric", length = nrow(t) + 1)
  k <- 1                                                                                          
  for (i in 1:nrow(t)) {                                                                          
    for (l in 1:nc) m[k, l] <- formatC(t[i, l], format = "d")                                   
    m[k, nc + 1] <- RS[i]                                                                       
    n.rnames[i] <- 1
    k <- k + 1                                                                                  
    if (expected) {                                                                             
      for (l in 1:nc) m[k, l] <- formatC(CST$expected[i,                                      
                                                      l], digits = 1, format = "f", decimal.mark = outDec)                                
      m[k, nc + 1] <- " "                                                                     
      n.rnames[i] <- n.rnames[i] + 1
      k <- k + 1                                                                              
    }                                                                                           
    if (prop.chisq) {                                                                           
      for (l in 1:nc) m[k, l] <- formatC((((CST$expected[i,                                   
                                                         l] - t[i, l])^2)/CST$expected[i, l]), digits = digits,                              
                                         format = "f", decimal.mark = outDec)                                                
      m[k, nc + 1] <- " "                                                                     
      n.rnames[i] <- n.rnames[i] + 1
      k <- k + 1                                                                              
    }                                                                                           
    if (prop.r) {                                                                               
      for (l in 1:nc) m[k, l] <- formatC(CPR[i, l] * hdd,                                     
                                         digits = digits, format = "f", decimal.mark = outDec)                               
      m[k, nc + 1] <- formatC(hdd * RS[i]/GT, digits = digits,                                
                              format = "f", decimal.mark = outDec)                                                
      n.rnames[i] <- n.rnames[i] + 1
      k <- k + 1                                                                              
    }                                                                                           
    if (prop.c) {                                                                               
      for (l in 1:nc) m[k, l] <- formatC(CPC[i, l] * hdd,                                     
                                         digits = digits, format = "f", decimal.mark = outDec)                               
      m[k, nc + 1] <- " "                                                                     
      n.rnames[i] <- n.rnames[i] + 1
      k <- k + 1                                                                              
    }                                                                                           
    if (prop.t) {                                                                               
      for (l in 1:nc) m[k, l] <- formatC(CPT[i, l] * hdd,                                     
                                         digits = digits, format = "f", decimal.mark = outDec)                               
      m[k, nc + 1] <- " "                                                                     
      n.rnames[i] <- n.rnames[i] + 1
      k <- k + 1                                                                              
    }                                                                                           
    if (resid) {                                                                                
      for (l in 1:nc) m[k, l] <- formatC(CST$observed[i,                                      
                                                      l] - CST$expected[i, l], digits = digits, format = "f",                             
                                         decimal.mark = outDec)                                                              
      m[k, nc + 1] <- " "                                                                     
      n.rnames[i] <- n.rnames[i] + 1
      k <- k + 1                                                                              
    }                                                                                           
    if (sresid) {                                                                               
      for (l in 1:nc) m[k, l] <- formatC(CST$residual[i,                                      
                                                      l], digits = digits, format = "f", decimal.mark = outDec)                           
      m[k, nc + 1] <- " "                                                                     
      n.rnames[i] <- n.rnames[i] + 1
      k <- k + 1                                                                              
    }                                                                                           
    if (asresid) {                                                                              
      for (l in 1:nc) m[k, l] <- formatC(ASR[i, l], digits = digits,                          
                                         format = "f", decimal.mark = outDec)                                                
      m[k, nc + 1] <- " "                                                                     
      n.rnames[i] <- n.rnames[i] + 1
      k <- k + 1                                                                              
    }                                                                                           
  }
  ColTotal <- gettext("Total", domain = "R-descr")                                                
  RowTotal <- ColTotal                                                                            
  rnames[length(rnames) + 1] <- RowTotal                                                                           
  for (l in 1:nc) m[k, l] <- formatC(c(CS[l]), format = "d")                                      
  m[k, nc + 1] <- formatC(GT, format = "d")                                                       
  n.rnames[length(n.rnames)] <- 1
  if (prop.c) {
    k <- k + 1
    for (l in 1:nc) m[k, l] <- formatC(hdd * CS[l]/GT, digits = digits,
                                       format = "f", decimal.mark = outDec)
    n.rnames[length(n.rnames)] <- 2
  }                                                                                           
  colnames(m) <- c(colnames(t), ColTotal)                                                         
  nc <- nc + 1                                                                                    
  mcolnames <- colnames(m)                                                                        
  res.m <- ascii(m, include.colnames = T, header = T, lgroup = rnames, n.lgroup = n.rnames, lstyle = "s")
  res.t <- NULL
  if (chisq) {                                                                                    
    res.t <- list("Pearson's Chi-squared test" = paste(gettext("Chi^2 = ", domain = "R-descr"), format(CST$statistic),
                    ", ", gettext("d.f. = ", domain = "R-descr"), format(CST$parameter),                           
                    ", ", gettext("p = ", domain = "R-descr"), format(CST$p.value), sep = ""))
    if (all(dim(t) == 2)) {                                                                     
      res.t <- c(res.t, "Pearson's Chi-squared test with Yates' continuity correction" = paste(gettext("Chi^2 = ", domain = "R-descr"),
                          format(CSTc$statistic),                             
                          ", ", gettext("d.f. = ", domain = "R-descr"),                                      
                          format(CSTc$parameter), ", ", gettext("p = ", domain = "R-descr"),                         
                          format(CSTc$p.value), sep = ""))
    }
  }                                                                                               
  if (is.na(McN[1]) == FALSE) {                                                                   
    res.t <- c(res.t, "McNemar's Chi-squared test" = paste(gettext("Chi^2 = ", domain = "R-descr"), format(McN$statistic),                        
                        ", ", gettext("d.f. = ", domain = "R-descr"), format(McN$parameter),
                        ", ", gettext("p = ", domain = "R-descr"), format(McN$p.value), sep = ""))                                                            
    if (is.na(McNc[1]) == FALSE) {                                                              
      res.t <- c(res.t, "McNemar's Chi-squared test with continuity correction" = paste(gettext("Chi^2 = ", domain = "R-descr"), format(McNc$statistic),
                          ", ", gettext("d.f. = ", domain = "R-descr"),                                      
                          format(McNc$parameter), ", ", gettext("p = ", domain = "R-descr"),                         
                          format(McNc$p.value), sep = ""))                                                                 
    }                                                                                           
  }                                                                                               
  if (fisher) {                                                                                   
    res.t <- c(res.t, "Fisher's Exact Test for Count Data" = paste(gettext("Alternative hypothesis: two.sided",
                        domain = "R-descr"), gettext(", p = ", domain = "R-descr"), format(FTt$p.value), sep = ""))
  }
  if (format == "SPSS") {
    if (any(dim(t) >= 2) & any(chisq, mcnemar, fisher)) {
      MinExpF = min(CST$expected)
      res.t <- c(res.t, list("Minimum expected frequency" = format(MinExpF)))
      NMinExpF = length(CST$expected[which(CST$expected <
        5)])
      if (NMinExpF > 0) {
        NCells = length(CST$expected)
        res.t <- c(res.t, "Cells with Expected Frequency < 5" = paste(format(NMinExpF), " ", "of ",
                            NCells, " (", format(100 * NMinExpF/NCells),
                            "%)", sep = ""))
      }
      ## cat("\n")
    }
  }
  content <- ascii(content, caption = "Cell Contents", caption.level = "s")
  if (!is.null(res.t))
    res.t <- ascii(res.t, caption = "Statistics for All Table Factors", caption.level = "s", list.type = "label")
  res <- asciiMixed$new(content, res.m, res.t)
  class(res) <- c("ascii", "proto", "environment")
  return(res)
}

##' ascii method for class freqtable
##'
##' \code{descr} package
##' @return An ascii object.
##' @S3method ascii freqtable
##' @author David Hajage
##' @param x An R object of class found among \code{methods(ascii)}.
##' @param header logical or numeric. If \code{TRUE} or \code{1}, \code{2},
##'   \dots{}, the first line(s) of the table is (are) emphasized. The default
##'   value depends of class of \code{x}.
##' @param footer logical or numeric. If \code{TRUE} or \code{1}, the last
##'   line(s) of the table is (are) emphasized. The default value depends of
##'   class of \code{x}.
##' @param digits Numeric vector of length equal to the number of columns of
##'   the resulting table (otherwise it will be replicated or truncated as
##'   necessary) indicating the number of digits to display in the
##'   corresponding columns.  Default is \code{2}.
##' @param format Character vector or matrix indicating the format for the
##'   corresponding columns.  These values are passed to the \code{formatC}
##'   function.  Use \code{"d"} (for integers), \code{"f"}, \code{"e"},
##'   \code{"E"}, \code{"g"}, \code{"G"}, \code{"fg"} (for reals), or
##'   \code{"s"} (for strings).  \code{"f"} gives numbers in the usual
##'   \code{xxx.xxx} format; \code{"e"} and \code{"E"} give \code{n.ddde+nn} or
##'   \code{n.dddE+nn} (scientific format); \code{"g"} and \code{"G"} put
##'   \code{x[i]} into scientific format only if it saves space to do so.
##'   \code{"fg"} uses fixed format as \code{"f"}, but \code{digits} as number
##'   of \emph{significant} digits.  Note that this can lead to quite long
##'   result strings. Finaly, \code{"nice"} is like \code{"f"}, but with 0
##'   digits if \code{x} is an integer. Default depends on the class of
##'   \code{x}.
##' @param na.print The character string specifying how \code{NA} should be
##'   formatted specially. Default is "".
##' @param include.rownames logical. If \code{TRUE} the rows names are printed.
##'   Default value depends of class of \code{x}.
##' @param include.colnames logical. If \code{TRUE} the columns names are
##'   printed. Default value depends of class of \code{x}.
##' @param caption Character vector of length 1 containing the table's caption
##'   or title.  Set to \code{""} to suppress the caption.  Default value is
##'   \code{NULL}.
##' @param ... Additional arguments.  (see \code{?ascii.default}).
ascii.freqtable <- function (x, header = TRUE, footer = TRUE, digits = c(0, 2, 2), format = "f", na.print = "", include.rownames = TRUE, include.colnames = TRUE, caption = x$label, ...) {
  res <- x$freqtable
  ascii(res, header = header, footer = footer, include.rownames = include.rownames, include.colnames = include.colnames, caption = caption, digits = digits, format = format, na.print = na.print, ...)
}
