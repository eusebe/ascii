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
    caption           <- object@caption
    width             <- object@width
    frame             <- object@frame
    grid              <- object@grid
    valign            <- object@valign
    header            <- object@header
    footer            <- object@footer
    align             <- object@align
    col.width         <- object@col.width
    style             <- object@style
    
    # detection des colonnes numeriques
    numerics <- sapply(x, is.numeric)
    # adaption de certains parametres
    # format <- unlist(strsplit(format, "")) # No, format could be "fg" -> must be a vector
    format <- rep(format, length.out = ncol(x))
    digits <- rep(digits, length.out = ncol(x))

    # transformation de toute la dataframe en caracteres
    charac.x <- apply(format(x, trim = T), 2, as.character)
    if (is.vector(charac.x)) charac.x <- t(charac.x) # si une seule dimension

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
    charac.x <- rbind(data.frame(as.list(cnoms), stringsAsFactors = FALSE, check.names = FALSE), charac.x)
    }

    # Beautify cols (digits, format, spacing, na.print)
    replacement <- paste(na.print, "\\2", sep = "")
    for (i in 1:ncol(charac.x)) {
      if (numerics[i]) {
        charac.x[, i][charac.x[, i] == "NA"] <- "" # necessaire avant le formatage des nombres avec formatC(as.numeric(...))
        if (include.colnames)  charac.x[2:nrow(charac.x),i] <- formatC(as.numeric(charac.x[2:nrow(charac.x),i]), format = format[i], digits = digits[i], decimal.mark = decimal.mark)
        if (!include.colnames) charac.x[,i] <- formatC(as.numeric(charac.x[,i]), format = format[i], digits = digits[i], decimal.mark = decimal.mark)
      }
      charac.x[,i] <- sub("(NA)( *)", replacement, charac.x[,i])
      charac.x[,i] <- format(charac.x[,i], justify = "left")
    }

    # cat result
    rows <- apply(charac.x, 1, function(x) paste("|", paste(x, collapse = "|"), sep = ""))
    maxchars <- max(nchar(rows)) - 1
    topbot <- paste("|", paste(rep("=", maxchars), collapse = ""), sep = "")
    cat(header(caption = caption, frame = frame, grid = grid, valign = valign, header = header, footer = footer, cols = cols(ncol(charac.x), align = align, col.width = col.width, style = style), width = width))
    cat(topbot, "\n")
    cat(rows, sep = "\n")
    cat(topbot, "\n")
  }
)

setMethod(
  "show",
  "R2asciidocVector",
  function (object){
    x                 <- object@x
    include.rownames  <- object@include.rownames
    include.colnames  <- object@include.colnames
    format            <- object@format
    digits            <- object@digits
    decimal.mark      <- object@decimal.mark
    na.print          <- object@na.print
    caption           <- object@caption
    width             <- object@width
    frame             <- object@frame
    grid              <- object@grid
    valign            <- object@valign
    header            <- object@header
    footer            <- object@footer
    align             <- object@align
    col.width         <- object@col.width
    style             <- object@style

    # transformation du vecteur en caracteres
    charac.x <- as.character(x)

    if (is.numeric(x)) charac.x <- formatC(as.numeric(charac.x), format = format, digits = digits, decimal.mark = decimal.mark)
    replacement <- paste(na.print, "\\2", sep = "")
    charac.x <- sub("(NA)( *)", replacement, charac.x)
    charac.x <- format(charac.x, justify = "left")

    rows <- paste("|", paste(charac.x, collapse = "|"), sep = "")
    maxchars <- nchar(rows) - 1
    topbot <- paste("|", paste(rep("=", maxchars), collapse = ""), sep = "")

    cat(header(caption = caption, frame = frame, grid = grid, valign = valign, header = header, footer = footer, cols = cols(length(charac.x), align = align, col.width = col.width, style = style), width = width))
    cat(topbot, "\n")
    cat(rows, sep = "\n")
    cat(topbot, "\n")
  }
)

setMethod(
  "show",
  "R2asciidocList",
  function (object){
    x <- object@x
    caption <- object@caption
    
    charac.x <- vector("character", length(x))
    for (i in 1:length(x)) {
      charac.x[i] <- sub("( *)(.*)", "\\1- \\2", sub("\t", "  ", x[[i]]))
    }
    if (caption != "") cat(".", caption, "\n", sep = "")
    cat(charac.x, sep = "\n")
  }
)
