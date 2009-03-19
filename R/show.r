require(proto)

asciiDataFrame <- proto(expr = {
  new <- function(.,
    x,
    include.rownames,
    include.colnames,
    format,
    digits,
    decimal.mark,
    na.print,
    caption,
    width,
    frame,
    grid,
    valign,
    header,
    footer,
    align,
    col.width,
    style) proto(.,
    x = x,
    include.rownames = include.rownames,
    include.colnames = include.colnames,
    format = format,
    digits = digits,
    decimal.mark = decimal.mark,
    na.print = na.print,
    caption = caption,
    width = width,
    frame = frame,
    grid = grid,
    valign = valign,
    header = header,
    footer = footer,
    align = align,
    col.width = col.width,
    style = style)

  charac <- function(.) {
    # detection des colonnes numeriques
    numerics <- sapply(.$x, is.numeric)
    # adaption de certains parametres
    # format <- unlist(strsplit(format, "")) # No, format could be "fg" -> must be a vector
    format <- rep(.$format, length.out = ncol(.$x))
    digits <- rep(.$digits, length.out = ncol(.$x))

    # transformation de toute la dataframe en caracteres
    charac.x <- apply(format(.$x, trim = T), 2, as.character)
    if (is.vector(charac.x)) charac.x <- t(charac.x) # si une seule dimension

    charac.x <- as.data.frame(charac.x, , stringsAsFactors = F)

    # rownames and colnames
    rnoms <- rownames(.$x)
    cnoms <- names(.$x)
    if (.$include.rownames) {
      charac.x <- data.frame(rnoms, charac.x, stringsAsFactors = F)
      cnoms <- c("", cnoms)

      # adaptation de certains parametres
      format <- c("f", format)
      digits <- c(0, digits)
      numerics <- c(FALSE, numerics)
    }
    if (.$include.colnames) {
      names(cnoms) <- names(charac.x) # for following rbind
      charac.x <- rbind(data.frame(as.list(cnoms), stringsAsFactors = FALSE, check.names = FALSE), charac.x)
    }

    # Beautify cols (digits, format, spacing, na.print)
    replacement <- paste("\\1", .$na.print, "\\3", sep = "")
    for (i in 1:ncol(charac.x)) {
      if (numerics[i]) {
        charac.x[, i][charac.x[, i] == "NA"] <- "" # necessaire avant le formatage des nombres avec formatC(as.numeric(...))
        if (.$include.colnames)  charac.x[2:nrow(charac.x),i] <- formatC(as.numeric(charac.x[2:nrow(charac.x),i]), format = format[i], digits = digits[i], decimal.mark = .$decimal.mark)
        if (!.$include.colnames) charac.x[,i] <- formatC(as.numeric(charac.x[,i]), format = format[i], digits = digits[i], decimal.mark = .$decimal.mark)
      }
      charac.x[,i] <- sub("(^ *)(NA)( *$)", replacement, charac.x[,i])
      charac.x[,i] <- format(charac.x[,i], justify = "left")
      charac.x[,i] <- gsub("\\|", "\\\\|", charac.x[,i])
    }
    return(charac.x)
  }

  show.asciidoc <- function(.) {
    charac.x <- charac(.)
    # cat result
    rows <- apply(charac.x, 1, function(x) paste("|", paste(x, collapse = "|"), sep = ""))
    maxchars <- max(nchar(rows)) - 1
    topbot <- paste("|", paste(rep("=", maxchars), collapse = ""), sep = "")
    cat(header.asciidoc(caption = .$caption, frame = .$frame, grid = .$grid, valign = .$valign, header = .$header, footer = .$footer, cols = cols(ncol(charac.x), align = .$align, col.width = .$col.width, style = .$style), width = .$width))
    cat(topbot, "\n")
    cat(rows, sep = "\n")
    cat(topbot, "\n")
  }

  show.t2t <- function(.) {
    charac.x <- charac(.)
    # prise en compte du style
    if (.$style != "") {  
      style <- unlist(strsplit(.$style, ""))
      style <- rep(style, length.out = ncol(charac.x))
      for (i in 1:ncol(charac.x)) {
        charac.x[,i] <- beauty.t2t(charac.x[,i], style[i])
      }
    }
    # prise en compte de l'alignement
    if (.$align != "") {  
      align <- unlist(strsplit(.$align, ""))
      align <- rep(align, length.out = ncol(charac.x))
      for (i in 1:ncol(charac.x)) {
        if (length(grep("^ *$", charac.x[1, i])) == 0) {
          if (align[i] == "c") { charac.x[1, i] <- sub("^ *", " ", charac.x[1, i]) ; charac.x[1, i] <- sub(" *$", " ", charac.x[1, i]) }
          if (align[i] == "r") { charac.x[1, i] <- sub("^ *", " ", charac.x[1, i]) ; charac.x[1, i] <- sub(" *$", "", charac.x[1, i]) } 
        }
      }
    }
    # cat result
    rows <- apply(charac.x, 1, function(x) paste("| ", paste(x, collapse = " | "), sep = ""))
    if (.$header) {
      rows[1] <- paste("|", rows[1], sep = "")
    }
    if (.$footer) {
      rows[length(rows)] <- paste("|", rows[length(rows)], sep = "")
    }
    if (.$frame == "" | .$frame == "all") rows <- paste(rows, " |", sep = "")
    if (.$caption != "") cat("=====", .$caption, "=====\n", sep = "")
    cat(rows, sep = "\n")
  }

  show.textile <- function(.) {
    charac.x <- charac(.)
    # prise en compte du style
    if (.$style != "") {  
      style <- unlist(strsplit(.$style, ""))
      style <- rep(style, length.out = ncol(charac.x))
      for (i in 1:ncol(charac.x)) {
        charac.x[,i] <- beauty.textile(charac.x[,i], style[i])
      }
    }
    # prise en compte de l'alignement
    if (.$align != "") {  
      align <- unlist(strsplit(.$align, ""))
      align <- rep(align, length.out = ncol(charac.x))
      for (i in 1:ncol(charac.x)) {
        charac.x[,i] <- beauty.textile(charac.x[,i], align[i])
      }
    }
    # prise en compte des header, footer
    if (.$header) charac.x[1,] <- beauty.textile(charac.x[1,], "header") 
    if (.$footer) charac.x[nrow(charac.x),] <- beauty.textile(charac.x[nrow(charac.x),], "header")
    # cat result
    cat(header.textile(frame = .$frame))
    rows <- apply(charac.x, 1, function(x) paste("|", paste(x, collapse = "|"), "|", sep = ""))
    cat(rows, sep = "\n")
  }
})

asciiList <- proto(expr = {
  new <- function(.,
    x,
    caption) proto(.,
    x = x,
    caption = caption)

  show.asciidoc <- function(.) {
    charac.x <- vector("character", length(.$x))
    for (i in 1:length(.$x)) {
      tmp <- gsub('\t|(*COMMIT)(*FAIL)','*', .$x[[i]], perl = TRUE)
      tmp <- sub('(^\\*+)(.*)', ' \\1 \\2', tmp)
      charac.x[i] <- sub("(^[^  \\*].*)", "- \\1", tmp)
    }
    if (.$caption != "") cat(".", .$caption, "\n", sep = "")
    cat(charac.x, sep = "\n")
  }
  show.t2t <- function(.) {
    charac.x <- vector("character", length(.$x))
    for (i in 1:length(.$x)) {
      tmp <- gsub('\t|(*COMMIT)(*FAIL)','-', .$x[[i]], perl = TRUE)
      tmp <- sub('(^-+)(.*)', ' \\1 \\2', tmp)
      charac.x[i] <- sub("(^[^  \\-].*)", "- \\1", tmp)
    }
    if (.$caption != "") cat("=====", .$caption, "=====\n", sep = "")
    cat(charac.x, sep = "\n")
  }
})

asciiDataFrameList <- proto(expr = {
  new <- function(.,
    x.df,
    x.list) proto(.,
    x.df = x.df,
    x.list = x.list)

  show.asciidoc <- function(.) {
    .$x.df$show.asciidoc()
    .$x.list$show.asciidoc()
  }

  show.t2t <- function(.) {
    .$x.df$show.t2t()
    .$x.list$show.t2t()
  }
})

