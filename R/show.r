require(proto)

asciiDataFrame <- proto(expr = {
  new <- function(.,
    x,
    include.rownames,
    include.colnames,
    rownames,
    colnames,
    format,
    digits,
    decimal.mark,
    na.print,
    caption,
    caption.level,
    width,
    frame,
    grid,
    valign,
    header,
    footer,
    align,
    col.width,
    style,
    tgroup,
    n.tgroup,
    calign,
    cvalign,
    cstyle,    
    lgroup,
    n.lgroup,
    ralign,
    rvalign,
    rstyle) proto(.,
    x = x,
    include.rownames = include.rownames,
    include.colnames = include.colnames,
    rownames = rownames,
    colnames = colnames,
    format = format,
    digits = digits,
    decimal.mark = decimal.mark,
    na.print = na.print,
    caption = caption,
    caption.level = caption.level, 
    width = width,
    frame = frame,
    grid = grid,
    valign = valign,
    header = header,
    footer = footer,
    align = align,
    col.width = col.width,
    style = style,
    tgroup = tgroup,
    n.tgroup = n.tgroup,
    calign = calign,
    cvalign = cvalign,
    cstyle = cstyle, 
    lgroup = lgroup,
    n.lgroup = n.lgroup,
    ralign = ralign,
    rvalign = rvalign,
    rstyle = rstyle
)

  charac <- function(.) {
    # detection des colonnes numeriques
    numerics <- sapply(.$x, is.numeric)
    # adaption de certains parametres
    # format <- unlist(strsplit(format, "")) # No, format could be "fg" -> must be a vector
    if (!is.matrix(.$format)) format <- t(matrix(rep(.$format, length.out = ncol(.$x)), ncol(.$x), nrow(.$x), F))
    else format <- apply(t(apply(.$format, 1, rep, length = ncol(.$x))), 2, rep, length = nrow(.$x))
#    digits <- rep(.$digits, length.out = ncol(.$x))
    if (!is.matrix(.$digits)) digits <- t(matrix(rep(.$digits, length.out = ncol(.$x)), ncol(.$x), nrow(.$x), F))
    else digits <- apply(t(apply(.$digits, 1, rep, length = ncol(.$x))), 2, rep, length = nrow(.$x))
 
    # transformation de toute la dataframe en caracteres
    charac.x <- apply(format(.$x, trim = T), 2, as.character)
    if (is.vector(charac.x)) charac.x <- t(charac.x) # si une seule dimension

    charac.x <- as.data.frame(charac.x, , stringsAsFactors = F)

    # rownames and colnames
    if (!is.null(.$rownames)) {
      rownames(.$x) <- rep(.$rownames, length.out = nrow(.$x))
    }
    if (!is.null(.$colnames)) {
      colnames(.$x) <- rep(.$colnames, length.out = ncol(.$x))
    }
    
    rnoms <- rownames(.$x)
    cnoms <- names(.$x)
    if (.$include.rownames) {
      charac.x <- data.frame(rnoms, charac.x, stringsAsFactors = F)
      cnoms <- c("", cnoms)

      # adaptation de certains parametres
      format <- cbind("f", format)
      digits <- cbind(0, digits)
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
        if (.$include.colnames)  charac.x[2:nrow(charac.x),i] <- apply(as.matrix(as.numeric(charac.x[2:nrow(charac.x),i])), 2, Vectorize(formatC), digits = digits[,i], format = format[,i], decimal.mark = .$decimal.mark)
        if (!.$include.colnames)  charac.x[,i] <- apply(as.matrix(as.numeric(charac.x[,i])), 2, Vectorize(formatC), digits = digits[,i], format = format[,i], decimal.mark = .$decimal.mark)
      }
      charac.x[,i] <- sub("(^ *)(NA)( *$)", replacement, charac.x[,i])
      charac.x[,i] <- format(charac.x[,i], justify = "left")
      charac.x[,i] <- gsub("\\|", "\\\\|", charac.x[,i])
    }
    return(charac.x)
  }

  show.asciidoc <- function(.) {
    charac.x <- charac(.)
    if (.$align != "") {  
      align <- unlist(strsplit(.$align, ""))
      align <- rep(align, length.out = ncol)
    } else align = .$align

    if (.$valign != "") {  
      valign <- rep(.$valign, length.out = ncol)
    } else valign = .$valign
    
    if (.$style != "") {
      style <- unlist(strsplit(.$style, ""))
      style <- rep(style, length.out = ncol(charac.x))
    } else style = .$style
    
    rows <- apply(charac.x, 1, function(x) paste(paste("", Vectorize(cells)(align = align, valign = valign, style = style), "| ", x, sep = ""), collapse = " "))
    
    if (!is.null(.$lgroup)) {
      lgroup <- .$lgroup
      n.lgroup <- .$n.lgroup
      if (is.null(n.lgroup))
        lgroup <- lgroup[1]
      
      n.lgroup <- rep(n.lgroup, length.out = length(lgroup))
      if (sum(n.lgroup) != nrow(charac.x)) {
        if (is.null(n.lgroup)) {
          n.lgroup <- nrow(charac.x)
        } else {
          n.lgroup[length(n.lgroup)] <- n.lgroup[length(n.lgroup)] + nrow(charac.x) - sum(n.lgroup)
        }
      }
      pos.lgroup <- c(1, 1+cumsum(n.lgroup))[1:length(n.lgroup)]
      rows[pos.lgroup] <- paste(paste(cells(span = paste(".", n.lgroup, "+", sep = ""), align = .$ralign, valign = .$rvalign, style = .$rstyle), lgroup, sep = "| "), rows[pos.lgroup], sep = " ")
    }
    
    if (sum(.$col.width) > length(.$col.width)) {
      col.width <- paste(rep(.$col.width, length.out = ncol(charac.x) + !is.null(.$lgroup)), collapse = ",")
    } else col.width <- ""
    
    maxchars <- max(nchar(rows)) - 1

    topbot <- paste("|", paste(rep("=", maxchars), collapse = ""), sep = "")
    cat(header.asciidoc(caption = .$caption, caption.level = .$caption.level, frame = .$frame, grid = .$grid, valign = .$valign, header = .$header, footer = .$footer, cols = col.width, width = .$width))
    
    cat(topbot, "\n")
    if (!is.null(.$tgroup)) {
      tgroup <- .$tgroup
      n.tgroup <- .$n.tgroup
      if (is.null(n.tgroup))
        tgroup <- tgroup[1]
      n.tgroup <- rep(n.tgroup, length.out = length(.$tgroup))
      if (is.null(n.tgroup))
        n.tgroup <- 0
      if (!is.null(.$lgroup)) {
        tgroup <- c("", tgroup)
        n.tgroup <- c(1, n.tgroup)
      }
      if (sum(n.tgroup) != ncol(charac.x)) {
        n.tgroup[length(n.tgroup)] <- n.tgroup[length(n.tgroup)] + ncol(charac.x) - sum(n.tgroup) + !is.null(.$lgroup)
      }
      cat(paste(cells(span = paste(n.tgroup, "+", sep = ""), align = .$calign, valign = .$cvalign, style = .$cstyle), tgroup, sep = "| "), "\n")
    }
    cat(rows, sep = "\n")
    cat(topbot, "\n")
  }

  show.sphinx <- function(.) { # Manque la classe pour les listes et les mixtes
    # L'alignement et footer ne sont pas gérés par sphinx

    charac.x <- charac(.)
    nrowx <- nrow(charac.x)
    ncolx <- ncol(charac.x)

    if (.$style != "") {  
      style <- unlist(strsplit(.$style, ""))
      style <- rep(style, length.out = ncolx)
      for (i in 1:ncolx) {
        charac.x[,i] <- beauty.sphinx(charac.x[,i], style[i])
      }
    }

    tgroup <- .$tgroup
    lgroup <- .$lgroup
    if (.$cstyle != "")
      tgroup <- beauty.sphinx(tgroup, .$cstyle)
    if (.$rstyle != "")
      lgroup <- beauty.sphinx(lgroup, .$rstyle)

    
    ncharcell <- nchar(charac.x[1,]) + 2
    
    if (!is.null(tgroup)) {
      n.tgroup <- .$n.tgroup
      if (is.null(n.tgroup))
        tgroup <- tgroup[1]
      n.tgroup <- rep(n.tgroup, length.out = length(tgroup))
      if (sum(n.tgroup) != ncol(charac.x)) {
        if (is.null(n.tgroup)) {
          n.tgroup <- ncol(charac.x)# + !is.null(lgroup)
        } else {
          n.tgroup[length(n.tgroup)] <- n.tgroup[length(n.tgroup)] + ncol(charac.x) - sum(n.tgroup)# + !is.null(lgroup)
        }
      }
      newtgroup <- NULL
      for (i in 1:length(tgroup))
        newtgroup <- c(newtgroup, tgroup[i], rep("", n.tgroup[i] - 1))

      names(newtgroup) <- names(charac.x) # for following rbind
      charac.x <- rbind(data.frame(as.list(newtgroup), stringsAsFactors = FALSE, check.names = FALSE), charac.x)
      charac.x <- format(charac.x, justify = "left")
      newtgroup <- as.character(charac.x[1,])
      charac.x <- charac.x[-1,]
      
      ccell <- cbind(cumsum(n.tgroup) - n.tgroup + 1, cumsum(n.tgroup))
      crows <- "|"
      for (i in 1:length(tgroup))
        crows <- paste(crows, paste(paste(newtgroup[unique(ccell[i,1]:ccell[i,2])], collapse = "   "), "|", collapse = " "), sep = " ")
      
      ncharcell <- nchar(charac.x[1,]) + 2

      cncharcell <- apply(cbind(cumsum(n.tgroup) - n.tgroup + 1, cumsum(n.tgroup), n.tgroup - 1), 1, function(x) sum(ncharcell[unique(x[1:2])[1]:unique(x[1:2])[length(unique(x[1:2]))]] ) + x[3])
      cinterrows <- paste("+", paste(sapply(cncharcell, function(x) paste(rep("-", x), collapse = "")), collapse = "+"), "+", sep = "")
    }

    rows <- apply(charac.x, 1, function(x) paste("|", paste(paste(x, " |", sep = ""), collapse = " ")))
    
  interrows <- rep(paste("+", paste(sapply(ncharcell, function(x) paste(rep("-", x), collapse = "")), collapse = "+"), "+", sep = ""), nrowx+1)
    if (!is.null(tgroup)) {
      interrows <- c(cinterrows, interrows)
      rows <- c(crows, rows)
    }

    if (!is.null(lgroup)) {
      n.lgroup <- .$n.lgroup
      if (is.null(n.lgroup))
        lgroup <- lgroup[1]
      n.lgroup <- rep(n.lgroup, length.out = length(lgroup))
      if (is.null(n.lgroup))
        n.lgroup <- 0
      if (!is.null(tgroup)) {
        lgroup <- c("", lgroup)
        n.lgroup <- c(1, n.lgroup)
      }
      if (sum(n.lgroup) != nrow(charac.x)) {
        n.lgroup[length(n.lgroup)] <- n.lgroup[length(n.lgroup)] + nrow(charac.x) - sum(n.lgroup) + !is.null(tgroup)
      }
      newlgroup <- rep("", sum(n.lgroup))
      rcell <- cumsum(c(1, n.lgroup[-length(n.lgroup)]))
      newlgroup[rcell] <- lgroup
      rrows <- paste("| ", format(newlgroup), " ", sep = "")
      rncharcell <- max(nchar(newlgroup)) + 2
      rinterrows <- paste("+", paste(sapply(rncharcell, function(x) paste(rep("-", x), collapse = "")), collapse = "+"), sep = "")
      norinterrows <- gsub("-", " ", rinterrows)
      
      rows <- paste(rrows, rows, sep = "")
      tmp <- rinterrows
      for (i in n.lgroup) {
        if (i == 1)
          rinterrows <- c(rinterrows, tmp)
        if (i > 1)
          rinterrows <- c(rinterrows, rep(norinterrows, i-1), tmp)
      }
      interrows <- paste(rinterrows, interrows, sep = "")
    }
    if (.$header)
      interrows[2] <- gsub("-", "=", interrows[2])

    cat(header.sphinx(caption = .$caption, caption.level = .$caption.level))    
    cat(c(rbind(interrows[-length(interrows)], rows), interrows[length(interrows)]), sep = "\n")
  }

  show.org <- function(.) {
    charac.x <- charac(.)
    ncolx <- ncol(charac.x)
    
    if (.$style != "") {  
      style <- unlist(strsplit(.$style, ""))
      style <- rep(style, length.out = ncolx)
      for (i in 1:ncolx) {
        charac.x[,i] <- beauty.org(charac.x[,i], style[i])
      }
    }
    
    ncharcell <- nchar(charac.x[1,]) + 2
    
    rows <- apply(charac.x, 1, function(x) paste("|", paste(paste(x, " |", sep = ""), collapse = " ")))
    
    interrow <- paste("|", paste(sapply(ncharcell, function(x) paste(rep("-", x), collapse = "")), collapse = "+"), "+", sep = "")
    
    if (.$header)
      rows <- c(rows[1], interrow, rows[-1])

    cat(header.org(caption = .$caption, caption.level = .$caption.level))    
    cat(interrow, "\n", sep = "")
    cat(rows, sep = "\n")
    cat(interrow, "\n", sep = "")
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

    # prise en compte de tgroup
    if (!is.null(.$tgroup)) {
      n.tgroup <- .$n.tgroup
      n.tgroup <- rep(n.tgroup, length.out = length(.$tgroup))
      if (sum(n.tgroup) != ncol(charac.x)) {
        if (is.null(n.tgroup)) {
          n.tgroup <- ncol(charac.x)
        } else {
          n.tgroup[length(n.tgroup)] <- n.tgroup[length(n.tgroup)] + ncol(charac.x) - sum(n.tgroup)
        }
      }
      tgroup <- .$tgroup
      if (.$cstyle != "") {
        cstyle <- rep(unlist(strsplit(.$cstyle, "")), length.out = length(tgroup))
        tgroup <- Vectorize(beauty.t2t)(tgroup, cstyle)
      }
      if (.$calign != "") {
        calign <- rep(unlist(strsplit(.$calign, "")), length.out = length(tgroup))
        for (i in 1:length(tgroup)) {
          if (calign[i] == "c")
            tgroup[i] <- paste(" ", tgroup[i], " ", collapse = "")
          if (calign[i] == "r")
            tgroup[i] <- paste(" ", tgroup[i], collapse = "")
        }
      }
      newtgroup <- paste("|", paste(paste(tgroup, lapply(n.tgroup, function(x) paste(rep("|", time = x), collapse = ""))), collapse = " "))
    }
    
    # prise en compte de l'alignement
    if (.$align != "") {  
      align <- unlist(strsplit(.$align, ""))
      align <- rep(align, length.out = ncol(charac.x))
      for (i in 1:ncol(charac.x)) {
        for (j in 1:nrow(charac.x)) {
          if (length(grep("^ *$", charac.x[j, i])) == 0) {
            if (align[i] == "c") { charac.x[j, i] <- sub("^ *", " ", charac.x[j, i]) ; charac.x[j, i] <- sub(" *$", " ", charac.x[j, i]) }
            if (align[i] == "r") { charac.x[j, i] <- sub("^ *", " ", charac.x[j, i]) ; charac.x[j, i] <- sub(" *$", "", charac.x[j, i]) } 
          }
        }
      }
    }
    rows <- apply(charac.x, 1, function(x) paste("| ", paste(x, collapse = " | "), sep = ""))
    # cat result
    if (!is.null(.$tgroup)) {
      if (.$header)
        cat("|", sep = "")
      cat(newtgroup, "\n")
    }
    if (.$header & is.null(.$tgroup)) {
      rows[1] <- paste("|", rows[1], sep = "")
    }
    if (.$footer) {
      rows[length(rows)] <- paste("|", rows[length(rows)], sep = "")
    }
    if (.$frame == "" | .$frame == "all") rows <- paste(rows, " |", sep = "")
    cat(header.t2t(caption = .$caption, caption.level = .$caption.level))
    cat(rows, sep = "\n")
  }

#   show.textile <- function(.) {
#     charac.x <- charac(.)
#     # prise en compte du style
#     if (.$style != "") {  
#       style <- unlist(strsplit(.$style, ""))
#       style <- rep(style, length.out = ncol(charac.x))
#       for (i in 1:ncol(charac.x)) {
#         charac.x[,i] <- beauty.textile(charac.x[,i], style[i])
#       }
#     }
#     # prise en compte de l'alignement
#     if (.$align != "") {  
#       align <- unlist(strsplit(.$align, ""))
#       align <- rep(align, length.out = ncol(charac.x))
#       for (i in 1:ncol(charac.x)) {
#         charac.x[,i] <- beauty.textile(charac.x[,i], align[i])
#       }
#     }
#     # prise en compte des header, footer
#     if (.$header) charac.x[1,] <- beauty.textile(charac.x[1,], "header") 
#     if (.$footer) charac.x[nrow(charac.x),] <- beauty.textile(charac.x[nrow(charac.x),], "header")
#     # cat result
#     cat(header.textile(frame = .$frame))
#     rows <- apply(charac.x, 1, function(x) paste("|", paste(x, collapse = "|"), "|", sep = ""))
#     cat(rows, sep = "\n")
#   }
})

asciiList <- proto(expr = {
  new <- function(.,
    x,
    caption, 
    caption.level, 
    list.type) proto(.,
    x = x,
    caption = caption, 
    caption.level = caption.level, 
    list.type = list.type)

  show.asciidoc <- function(.) {
    if (.$list.type == "bullet") mark <- rep("*", length(.$x))
    if (.$list.type == "number") mark <- rep(".", length(.$x))
    if (.$list.type == "none")   mark <- rep("", length(.$x))
    if (.$list.type == "label") {
      if (is.null(names(.$x))) {
        namesx <- paste("[[", 1:length(.$x), "]]", sep = "")
      } else {
        namesx <- names(.$x)
      }
      mark <- paste(namesx, "::\n  ", sep = "")
    }
    
    charac.x <- vector("character", length(.$x))
    for (i in 1:length(.$x)) {
      if (is.null(.$x[[i]])) next
      tmp <- .$x[[i]]
      if (.$list.type == "label") tmp <- sub("^\t*", "", tmp)
      tmp <- sub("(^.*)", paste(mark[i], "\\1", sep = ""), gsub('\t|(*COMMIT)(*FAIL)', mark[i], tmp, perl = TRUE))
      charac.x[i] <- sub(paste('(^\\', mark[i], '+)(.*)', sep = ""), '\\1 \\2', tmp)
    }
    cat(header.asciidoc(caption = .$caption, caption.level = .$caption.level))
    cat(charac.x, sep = "\n")
  }

  show.sphinx <- function(.) {
    if (.$list.type == "bullet") mark <- rep("*", length(.$x))
    if (.$list.type == "number") mark <- rep("#.", length(.$x))
    if (.$list.type == "none")  mark <- rep("", length(.$x))
    if (.$list.type == "label") {
      if (is.null(names(.$x))) {
        namesx <- paste("[[", 1:length(.$x), "]]", sep = "")
      } else {
        namesx <- names(.$x)
      }
      mark <- paste(namesx, "\n ", sep = "")
    }
    y <- gsub("(^\t*)(.*)", "\\1", .$x)
    z <- NULL
    for (i in 2:length(y))
      z <- c(z, ifelse(y[i] != y[i-1], i-1, NA))

    cat(header.sphinx(caption = .$caption, caption.level = .$caption.level), sep = "\n")

    for (i in 1:length(.$x)) {
      tmp <- .$x[[i]]
      if (.$list.type == "label") tmp <- sub("^\t*", "", tmp)
      tmp <- gsub('\t|(*COMMIT)(*FAIL)', "  ", tmp, perl = TRUE)
      tmp <- sub("(^ *)", paste("\\1", mark[i], " ", sep = ""), tmp)
      cat(tmp, "\n")
      if (i %in% z)
        cat("\n")
    }
  }

  show.org <- function(.) {
    indent.mark <- "  "
    if (.$list.type == "bullet") mark <- rep("-", length(.$x))
    if (.$list.type == "number") mark <- paste(seq(1, length(.$x), 1), ".", sep = "")
    if (.$list.type == "none")  { mark <- rep("", length(.$x)); indent.mark = ""}
    if (.$list.type == "label") {
      if (is.null(names(.$x))) {
        namesx <- paste("[ [ ", 1:length(.$x), " ] ]", sep = "")
      } else {
        namesx <- names(.$x)
      }
      mark <- paste("- ", namesx, " ::", sep = "")
      indent.mark = "  "
    }
    
    charac.x <- vector("character", length(.$x))
    for (i in 1:length(.$x)) {
      tmp <- .$x[[i]]
      tmp <- gsub('\t|(*COMMIT)(*FAIL)', indent.mark, tmp, perl = TRUE)
      charac.x[i] <- sub("(^ *)", paste("\\1", mark[i], " ", sep = ""), tmp)
    }
    cat(header.org(caption = .$caption, caption.level = .$caption.level))
    cat(charac.x, sep = "\n")
  }

  show.t2t <- function(.) {
    indent.mark <- " "
    if (.$list.type == "bullet") mark <- rep("-", length(.$x))
    if (.$list.type == "number") mark <- rep("+", length(.$x))
    if (.$list.type == "none")  { mark <- rep("", length(.$x)); indent.mark = "" }
    if (.$list.type == "label") {
      if (is.null(names(.$x))) {
        namesx <- paste("[[", 1:length(.$x), "]]", sep = "")
      } else {
        namesx <- names(.$x)
      }
      mark <- paste(": ", namesx, "\n", sep = "")
      indent.mark = ""
    }
    
    charac.x <- vector("character", length(.$x))
    for (i in 1:length(.$x)) {
      tmp <- .$x[[i]]
      if (.$list.type == "label") tmp <- sub("^\t*", "", tmp)
      tmp <- gsub('\t|(*COMMIT)(*FAIL)', indent.mark, tmp, perl = TRUE)
      charac.x[i] <- sub("(^ *)", paste("\\1", mark[i], indent.mark, sep = ""), tmp)
    }
    cat(header.t2t(caption = .$caption, caption.level = .$caption.level))
    cat(charac.x, sep = "\n")
  }
})

asciiMixed <- proto(expr = {
  new <- function(.,
    ...) {
    args <- list(...)
    noms <- as.character(as.list(substitute(list(...)))[-1])
    if (is.null(noms)) noms <- paste("obj", 1:length(args), sep = "")
    names(args) <- noms 
    as.proto(args)
  }

  show.asciidoc <- function(.) {
    args <- rev(as.list(.))
    for (i in seq_along(args)) {
      if (is.null(args[[i]])) next
      print(args[[i]], type = "asciidoc") 
      if (i != length(args)) cat("\n") 
    }
  }
  
  show.sphinx <- function(.) {
    args <- rev(as.list(.))
    for (i in seq_along(args)) {
      if (is.null(args[[i]])) next
      print(args[[i]], type = "sphinx") 
      if (i != length(args)) cat("\n") 
    }
  }

  show.org <- function(.) {
    args <- rev(as.list(.))
    for (i in seq_along(args)) {
      if (is.null(args[[i]])) next
      print(args[[i]], type = "org") 
      if (i != length(args)) cat("\n") 
    }
  }

  show.t2t <- function(.) {
    args <- rev(as.list(.))
    for (i in seq_along(args)) {
      if (is.null(args[[i]])) next
      print(args[[i]], type = "t2t")
      if (i != length(args)) cat("\n") 
    }
  }
})

