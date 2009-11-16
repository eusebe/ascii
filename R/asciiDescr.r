## print.compmeans <- function (x, ...) {                 
##     rlab <- ifelse(is.null(x$row.label), x$row, x$row.label)
##     clab <- ifelse(is.null(x$column.label), x$column, x$column.label)
##     msg1 <- gettext("Mean value of", domain = "R-descr")             
##     msg2 <- gettext("according to", domain = "R-descr")              
##     lwd <- getOption("width")                                        
##     msg <- paste(msg1, " \"", clab, "\" ", msg2, " \"", rlab,        
##         "\"", sep = "")                                              
##     if (nchar(msg) < lwd) {                                          
##         cat(msg, "\n", sep = "")                                     
##     }                                                                
##     else {                                                           
##         if ((nchar(msg1) + nchar(clab)) < lwd) {                     
##             msg <- paste(msg1, " \"", clab, "\" ", sep = "")         
##             if ((nchar(msg) + nchar(msg2)) < lwd) {                  
##                 cat(msg, msg2, "\n", "\"", rlab, "\"", "\n",         
##                   sep = "")                                          
##             }                                                        
##             else {                                                   
##                 cat(msg, "\n", sep = "")                             
##                 if ((nchar(msg2) + nchar(rlab)) < (lwd - 1)) {
##                   cat(msg2, " \"", rlab, "\"\n", sep = "")
##                 }
##                 else {
##                   cat(msg2, "\n\"", rlab, "\"\n", sep = "")
##                 }
##             }
##         }
##         else {
##             cat(msg1, "\n\"", clab, "\"\n", sep = "")
##             if ((nchar(msg2) + nchar(rlab)) < (lwd - 1)) {
##                 cat(msg2, " \"", rlab, "\"\n", sep = "")
##             }
##             else {
##                 cat(msg2, "\n\"", rlab, "\"\n", sep = "")
##             }
##         }
##     }
##     print(x$table, ...)
## }

ascii.meanscomp <- function (x, caption = "", caption.level = "s", include.rownames = TRUE, include.colnames = TRUE, ...) {                 
    rlab <- ifelse(is.null(x$row.label), x$row, x$row.label)
    clab <- ifelse(is.null(x$column.label), x$column, x$column.label)
    msg1 <- gettext("Mean value of", domain = "R-descr")             
    msg2 <- gettext("according to", domain = "R-descr")              
    msg <- switch(1 + (caption == ""), caption, paste(msg1, " \"", clab, "\" ", msg2, " \"", rlab, "\"", sep = ""))
    ascii(x$table, caption = msg, caption.level = caption.level, include.rownames = include.rownames, include.colnames = include.colnames, ...)
}

print.CrossTable <- function (x, ...) {                 
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
    }                                      
    else {                                 
        if (format == "SPSS") {            
            hdd <- 100                     
            psep <- "% | "                 
        }                                  
        else {                             
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
    cat(rep("\n", 2))                                                                    
    cat("  ", gettext("Cell Contents", domain = "R-descr"), "\n")                        
    if (format == "SAS") {                                                               
        cat("|-------------------------|\n")                                             
        cat(gettext("|                       N |", domain = "R-descr"),                  
            "\n")                                                                        
        if (expected)                                                                    
            cat(gettext("|              Expected N |", domain = "R-descr"),              
                "\n")                                                                    
        if (prop.chisq)                                                                  
            cat(gettext("| Chi-square contribution |", domain = "R-descr"),              
                "\n")                                                                    
        if (prop.r)                                                                      
            cat(gettext("|           N / Row Total |", domain = "R-descr"),              
                "\n")                                                                    
        if (prop.c)                                                                      
            cat(gettext("|           N / Col Total |", domain = "R-descr"),              
                "\n")                                                                    
        if (prop.t)                                                                      
            cat(gettext("|         N / Table Total |", domain = "R-descr"),              
                "\n")                                                                    
        cat("|-------------------------|\n")                                             
    }                                                                                    
    else if (format == "SPSS") {                                                         
        cat("|-------------------------|\n")                                             
        cat(gettext("|                   Count |", domain = "R-descr"),                  
            "\n")                                                                        
        if (expected)                                                                    
            cat(gettext("|         Expected Values |", domain = "R-descr"),              
                "\n")                                                                    
        if (prop.chisq)                                                                  
            cat(gettext("| Chi-square contribution |", domain = "R-descr"),              
                "\n")                                                                    
        if (prop.r)                                                                      
            cat(gettext("|             Row Percent |", domain = "R-descr"),              
                "\n")                                                                    
        if (prop.c)                                                                      
            cat(gettext("|          Column Percent |", domain = "R-descr"),              
                "\n")                                                                    
        if (prop.t)                                                                      
            cat(gettext("|           Total Percent |", domain = "R-descr"),              
                "\n")                                                                    
        if (resid)                                                                       
            cat(gettext("|                Residual |", domain = "R-descr"),              
                "\n")                                                                    
        if (sresid)                                                                      
            cat(gettext("|            Std Residual |", domain = "R-descr"),              
                "\n")                                                                    
        if (asresid)                                                                     
            cat(gettext("|           Adj Std Resid |", domain = "R-descr"),              
                "\n")                                                                    
        cat("|-------------------------|\n")                                             
    }                                                                                    
    if (vector.x) {                                                                      
        if (length(t) > max.width) {                                                     
            final.row <- length(t)%%max.width                                            
            max <- length(t) - final.row                                                 
            start <- seq(1, max, max.width)                                              
            end <- start + (max.width - 1)                                               
            if (final.row > 0) {                                                         
                start <- c(start, end[length(end)] + 1)                                  
                end <- c(end, end[length(end)] + final.row)                              
            }                                                                            
        }                                                                                
        else {                                                                           
            start <- 1                                                                   
            end <- length(t)                                                             
        }                                                                                
        SpaceSep3 <- paste(SpaceSep2, " ", sep = "")                                     
        for (i in 1:length(start)) {                                                     
            cat(cat(SpaceSep2, sep = " | ", collapse = ""), cat(formatC(dimnames(t)[[2]][start[i]:end[i]],                                                                                              
                width = CWidth - 1, format = "s"), sep = nsep,                                      
                collapse = "\n"), sep = "", collapse = "")                                          
            cat(SpaceSep3, rep(RowSep, (end[i] - start[i]) +                                        
                1), sep = "|", collapse = "\n")                                                     
            cat(cat(SpaceSep2, sep = " | ", collapse = ""), cat(formatC(t[,                         
                start[i]:end[i]], width = CWidth - 1, format = "d"),                                
                sep = nsep, collapse = "\n"), sep = "", collapse = "")                              
            if (prop.r)                                                                             
                cat(cat(SpaceSep2, sep = " | ", collapse = ""),                                     
                  cat(formatC(CPT[, start[i]:end[i]] * hdd, width = CWidth -                        
                    1, digits = digits, format = "f", decimal.mark = outDec),                       
                    sep = psep, collapse = ""), sep = "", collapse = "\n")                          
            cat(SpaceSep3, rep(RowSep, (end[i] - start[i]) +                                        
                1), sep = "|", collapse = "\n")                                                     
        }                                                                                           
        if (format == "SPSS" && GT < TotalN)                                                        
            cat("\n", gettext("Number of Missing Observations:",                                    
                domain = "R-descr"), " ", TotalN - GT, " (",                                        
                100 * (TotalN - GT)/TotalN, "%)\n", sep = "")                                       
        return(invisible(x))                                                                        
    }                                                                                               
    nelements <- 1 + expected + prop.chisq + prop.r + prop.c +                                      
        prop.t + resid + sresid + asresid                                                           
    nr <- nrow(t) * nelements + 1                                                                   
    nc <- ncol(t)                                                                                   
    m <- matrix(nrow = nr, ncol = nc + 1)                                                           
    rnames <- vector(mode = "character", length = nr)                                               
    k <- 1                                                                                          
    for (i in 1:nrow(t)) {                                                                          
        for (l in 1:nc) m[k, l] <- formatC(t[i, l], format = "d")                                   
        m[k, nc + 1] <- RS[i]                                                                       
        rnames[k] <- rownames(t)[i]                                                                 
        k <- k + 1                                                                                  
        if (expected) {                                                                             
            for (l in 1:nc) m[k, l] <- formatC(CST$expected[i,                                      
                l], digits = 1, format = "f", decimal.mark = outDec)                                
            m[k, nc + 1] <- " "                                                                     
            rnames[k] <- paste("<=>", as.character(k))                                              
            k <- k + 1                                                                              
        }                                                                                           
        if (prop.chisq) {                                                                           
            for (l in 1:nc) m[k, l] <- formatC((((CST$expected[i,                                   
                l] - t[i, l])^2)/CST$expected[i, l]), digits = digits,                              
                format = "f", decimal.mark = outDec)                                                
            m[k, nc + 1] <- " "                                                                     
            rnames[k] <- paste("<=>", as.character(k))                                              
            k <- k + 1                                                                              
        }                                                                                           
        if (prop.r) {                                                                               
            for (l in 1:nc) m[k, l] <- formatC(CPR[i, l] * hdd,                                     
                digits = digits, format = "f", decimal.mark = outDec)                               
            m[k, nc + 1] <- formatC(hdd * RS[i]/GT, digits = digits,                                
                format = "f", decimal.mark = outDec)                                                
            rnames[k] <- paste("<=>", as.character(k))                                              
            k <- k + 1                                                                              
        }                                                                                           
        if (prop.c) {                                                                               
            for (l in 1:nc) m[k, l] <- formatC(CPC[i, l] * hdd,                                     
                digits = digits, format = "f", decimal.mark = outDec)                               
            m[k, nc + 1] <- " "                                                                     
            rnames[k] <- paste("<=>", as.character(k))                                              
            k <- k + 1                                                                              
        }                                                                                           
        if (prop.t) {                                                                               
            for (l in 1:nc) m[k, l] <- formatC(CPT[i, l] * hdd,                                     
                digits = digits, format = "f", decimal.mark = outDec)                               
            m[k, nc + 1] <- " "                                                                     
            rnames[k] <- paste("<=>", as.character(k))                                              
            k <- k + 1                                                                              
        }                                                                                           
        if (resid) {                                                                                
            for (l in 1:nc) m[k, l] <- formatC(CST$observed[i,                                      
                l] - CST$expected[i, l], digits = digits, format = "f",                             
                decimal.mark = outDec)                                                              
            m[k, nc + 1] <- " "                                                                     
            rnames[k] <- paste("<=>", as.character(k))                                              
            k <- k + 1                                                                              
        }                                                                                           
        if (sresid) {                                                                               
            for (l in 1:nc) m[k, l] <- formatC(CST$residual[i,                                      
                l], digits = digits, format = "f", decimal.mark = outDec)                           
            m[k, nc + 1] <- " "                                                                     
            rnames[k] <- paste("<=>", as.character(k))                                              
            k <- k + 1                                                                              
        }                                                                                           
        if (asresid) {                                                                              
            for (l in 1:nc) m[k, l] <- formatC(ASR[i, l], digits = digits,                          
                format = "f", decimal.mark = outDec)                                                
            m[k, nc + 1] <- " "                                                                     
            rnames[k] <- paste("<=>", as.character(k))                                              
            k <- k + 1                                                                              
        }                                                                                           
    }                                                                                               
    ColTotal <- gettext("Total", domain = "R-descr")                                                
    RowTotal <- ColTotal                                                                            
    rnames[k] <- RowTotal                                                                           
    for (l in 1:nc) m[k, l] <- formatC(c(CS[l]), format = "d")                                      
    m[k, nc + 1] <- formatC(GT, format = "d")                                                       
    colnames(m) <- c(colnames(t), ColTotal)                                                         
    nc <- nc + 1                                                                                    
    colWidths <- vector(mode = "numeric", length = nc)                                              
    mcolnames <- colnames(m)                                                                        
    cat("\n\n")                                                                                     
    for (i in 1:nc) colWidths[i] <- max(c(nchar(m[, i]), nchar(mcolnames[i])))                      
    labelwidth <- max(nchar(rnames)) + 1                                                            
    dashedline <- rep("-", sum(colWidths) + 3 * nc + labelwidth)                                    
    ddashedline <- gsub("-", "=", dashedline)                                                       
    cat("\n", ddashedline, "\n", sep = "")                                                          
    cat(formatC(" ", width = labelwidth))                                                           
    for (j in 1:nc) cat("  ", formatC(mcolnames[j], width = colWidths[j]))                          
    for (i in 1:nr) {                                                                               
        if (length(grep("<=>", rnames[i])) == 0) {                                                  
            cat("\n", dashedline, "\n", sep = "")                                                   
            cat(formatC(rnames[i], width = labelwidth, format = "s",                                
                flag = "-"), sep = "")                                                              
        }                                                                                           
        else {                                                                                      
            cat("\n", formatC(" ", width = labelwidth), sep = "")                                   
        }                                                                                           
        for (j in 1:nc) {                                                                           
            cat("  ", formatC(m[i, j], width = colWidths[j]))                                       
        }                                                                                           
    }                                                                                               
    if (prop.c) {                                                                                   
        cat("\n", formatC(" ", width = labelwidth), sep = "")                                       
        for (j in 1:(nc - 1)) {                                                                     
            cat("  ", formatC(hdd * CS[j]/GT, width = colWidths[j],                                 
                digits = digits, format = "f", decimal.mark = outDec))                              
        }                                                                                           
    }                                                                                               
    cat("\n", ddashedline, "\n", sep = "")                                                          
    if (chisq) {                                                                                    
        cat(rep("\n", 2))                                                                           
        cat(gettext("Statistics for All Table Factors", domain = "R-descr"),                        
            "\n\n", sep = "")                                                                       
        cat(CST$method, "\n")                                                                       
        cat("------------------------------------------------------------\n")                       
        cat(gettext("Chi^2 =", domain = "R-descr"), CST$statistic,                                  
            "    ", gettext("d.f. =", domain = "R-descr"), CST$parameter,                           
            "    ", gettext("p =", domain = "R-descr"), CST$p.value,                                
            "\n\n")                                                                                 
        if (all(dim(t) == 2)) {                                                                     
            cat(CSTc$method, "\n")                                                                  
            cat("------------------------------------------------------------\n")                   
            cat(gettext("Chi^2 =", domain = "R-descr"), CSTc$statistic,                             
                "    ", gettext("d.f. =", domain = "R-descr"),                                      
                CSTc$parameter, "    ", gettext("p =", domain = "R-descr"),                         
                CSTc$p.value, "\n")                                                                 
        }                                                                                           
    }                                                                                               
    if (is.na(McN[1]) == FALSE) {                                                                   
        cat(rep("\n", 2))                                                                           
        cat(McN$method, "\n")                                                                       
        cat("------------------------------------------------------------\n")                       
        cat(gettext("Chi^2 =", domain = "R-descr"), McN$statistic,                                  
            "    ", gettext("d.f. =", domain = "R-descr"), McN$parameter,                           
            "    ", gettext("p =", domain = "R-descr"), McN$p.value,                                
            "\n\n")                                                                                 
        if (is.na(McNc[1]) == FALSE) {                                                              
            cat(McNc$method, "\n")                                                                  
            cat("------------------------------------------------------------\n")                   
            cat(gettext("Chi^2 =", domain = "R-descr"), McNc$statistic,                             
                "    ", gettext("d.f. =", domain = "R-descr"),                                      
                McNc$parameter, "    ", gettext("p =", domain = "R-descr"),                         
                McNc$p.value, "\n")                                                                 
        }                                                                                           
    }                                                                                               
    if (fisher) {                                                                                   
        cat(rep("\n", 2))                                                                           
        cat(gettext("Fisher's Exact Test for Count Data", domain = "R-descr"))                      
        cat("\n------------------------------------------------------------\n")                     
        if (all(dim(t) == 2)) {                                                                     
            cat(gettext("Sample estimate odds ratio:", domain = "R-descr"),                         
                FTt$estimate, "\n\n")                                                               
            cat(gettext("Alternative hypothesis: true odds ratio is not equal to 1",                
                domain = "R-descr"), "\n")                                                          
            cat(gettext("p =", domain = "R-descr"), FTt$p.value,                                    
                "\n")                                                                               
            cat(gettext("95% confidence interval:", domain = "R-descr"),                            
                FTt$conf.int, "\n\n")                                                               
            cat(gettext("Alternative hypothesis: true odds ratio is less than 1",                   
                domain = "R-descr"), "\n")                                                          
            cat(gettext("p =", domain = "R-descr"), FTl$p.value,                                    
                "\n")                                                                               
            cat(gettext("95% confidence interval:", domain = "R-descr"),                            
                FTl$conf.int, "\n\n")
            cat(gettext("Alternative hypothesis: true odds ratio is greater than 1",
                domain = "R-descr"), "\n")
            cat(gettext("p =", domain = "R-descr"), FTg$p.value,
                "\n")
            cat(gettext("95% confidence interval:", domain = "R-descr"),
                FTg$conf.int, "\n\n")
        }
        else {
            cat(gettext("Alternative hypothesis: two.sided",
                domain = "R-descr"), "\n")
            cat(gettext("p =", domain = "R-descr"), FTt$p.value,
                "\n")
        }
    }
    if (format == "SPSS") {
        if (any(dim(t) >= 2) & any(chisq, mcnemar, fisher)) {
            MinExpF = min(CST$expected)
            cat("       ", gettext("Minimum expected frequency:",
                domain = "R-descr"), MinExpF, "\n")
            NMinExpF = length(CST$expected[which(CST$expected <
                5)])
            if (NMinExpF > 0) {
                NCells = length(CST$expected)
                cat(gettext("Cells with Expected Frequency < 5:",
                  domain = "R-descr"), " ", NMinExpF, " ", gettext("of",
                  domain = "R-descr"), NCells, " (", 100 * NMinExpF/NCells,
                  "%)\n", sep = "")
            }
            cat("\n")
        }
    }
    return(invisible(x))
}

## print.freqtable <- function (x, digits = 4, na.print = "", ...) {
##     cat(x$label, "\n")
##     x2 <- x$freqtable
##     print(x2, digits = digits, na.print = na.print, ...)
##     return(invisible(x))
## }

ascii.freqtable <- function (x, digits = 4, na.print = "", include.rownames = TRUE, include.colnames = TRUE, caption = x$label, caption.level = "s", ...) {
  res <- x$freqtable
  ascii(res, include.rownames = include.rownames, include.colnames = include.colnames, caption = caption, caption.level = caption.level, digits = digits, na.print = na.print, ...)
}
