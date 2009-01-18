setMethod(
  "show",
  "R2asciidocDataFrame",
  function (object){
    x                 <- object@x
    include.rownames  <- object@include.rownames
    include.colnames  <- object@include.colnames
    format            <- object@format
    digits            <- object@digits
    decimal.mark      <- object@decimal.mark
    na.print          <- object@na.print

    # detection des colonnes numeriques
    numerics <- sapply(x, is.numeric)
    # adaption de certains parametres
    format <- rep(format, length.out = ncol(x))
    digits <- rep(digits, length.out = ncol(x))

    
    # transformation de toute la dataframe en en caracteres
    charac.x <- apply(format(x, trim = T), 2, as.character)
#    if (is.vector(charac.x)) charac.x <- t(charac.x) # si une seule dimension

    charac.x[charac.x == "NA"] <- "" # necessaire avant le formatage des nombres avec formatC(as.numeric(...))
    charac.x <- as.data.frame(charac.x, , stringsAsFactors = F)

    # rownames and colnames
    rnoms <- rownames(x)
    cnoms <- names(x)
    if (include.rownames) {
    charac.x <- data.frame(rnoms, charac.x, stringsAsFactors = F)
    cnoms <- c("", cnoms)

    # adaptation de certains parametres
    format <- rep(format, length.out = ncol(x)+1)
    digits <- c(0, digits)
    numerics <- c(FALSE, numerics)
    }
    if (include.colnames) {
    names(cnoms) <- names(charac.x) # for following rbind 
    charac.x <- rbind(data.frame(as.list(cnoms), stringsAsFactors = F), charac.x)
    }

    # Beautify cols
    replacement <- paste("\\1", na.print, "\\3", sep = "")
    for (i in 1:ncol(charac.x)) {
      if (numerics[i]) {
        if (include.colnames)  charac.x[2:nrow(charac.x),i] <- formatC(as.numeric(charac.x[2:nrow(charac.x),i]), format = format[i], digits = digits[i], decimal.mark = decimal.mark)
        if (!include.colnames) charac.x[,i] <- formatC(as.numeric(charac.x[,i]), format = format[i], digits = digits[i], decimal.mark = decimal.mark)
      }
      charac.x[,i] <- sub("( *)(NA)( *)", replacement, charac.x[,i])
      charac.x[,i] <- format(charac.x[,i], justify = "left")
    }

    # cat result
    for (i in 1:nrow(charac.x)) {
      cat("|", paste(charac.x[i,], collapse = "|"), sep = "")
      cat("\n")
    }
  }
)
