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
    cgroup,
    n.cgroup,
    calign,
    cvalign,
    cstyle,    
    rgroup,
    n.rgroup,
    ralign,
    rvalign,
    rstyle) proto(.,
    x = x,
    include.rownames = include.rownames,
    include.colnames = include.colnames,
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
    cgroup = cgroup,
    n.cgroup = n.cgroup,
    calign = calign,
    cvalign = cvalign,
    cstyle = cstyle, 
    rgroup = rgroup,
    n.rgroup = n.rgroup,
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
        if (.$include.colnames)  charac.x[2:nrow(charac.x),i] <- apply(as.matrix(as.numeric(charac.x[2:nrow(charac.x),i])), 2, Vectorize(formatC), digits = digits[,i], format = format[,i])
        if (!.$include.colnames)  charac.x[,i] <- apply(as.matrix(as.numeric(charac.x[,i])), 2, Vectorize(formatC), digits = digits[,i], format = format[,i])
      }
      charac.x[,i] <- sub("(^ *)(NA)( *$)", replacement, charac.x[,i])
      charac.x[,i] <- format(charac.x[,i], justify = "left")
      charac.x[,i] <- gsub("\\|", "\\\\|", charac.x[,i])
    }
    return(charac.x)
  }

  show.asciidoc <- function(.) {
    charac.x <- charac(.)
#    rows <- apply(charac.x, 1, function(x) paste("|", paste(x, collapse = "|"), sep = ""))
    if (.$align != "") {  
      align <- unlist(strsplit(.$align, ""))
      align <- rep(align, length.out = ncol)
      align[align == "l"] <- "<"
      align[align == "c"] <- "^"
      align[align == "r"] <- ">"
    } else align = .$align

    if (.$valign != "") {  
      valign <- unlist(strsplit(.$valign, ""))
      valign <- rep(valign, length.out = ncol)
      valign[valign == "top"] <- "<"
      valign[valign == "middle"] <- "^"
      valign[valign == "bottom"] <- ">"
    } else valign = .$valign
    
    if (.$style != "") {
      style <- unlist(strsplit(.$style, ""))
      style <- rep(style, length.out = ncol(charac.x))
    } else style = .$style
    
    rows <- apply(charac.x, 1, function(x) paste(paste("", Vectorize(cells)(align = align, valign = valign, style = style), "| ", x, sep = ""), collapse = " "))
    
    if (!is.null(.$rgroup)) {
      pos.rgroup <- c(1, 1+cumsum(.$n.rgroup))[1:length(.$n.rgroup)]
      rows[pos.rgroup] <- paste(paste(cells(span = paste(".", .$n.rgroup, "+", sep = ""), align = .$ralign, valign = .$rvalign, style = .$rstyle), .$rgroup, sep = "| "), rows[pos.rgroup], sep = " ")
    }
    
    if (sum(.$col.width) > length(.$col.width)) {
      col.width <- paste(rep(.$col.width, length.out = ncol(charac.x) + !is.null(.$rgroup)), collapse = ",")
    } else col.width <- ""
    
    maxchars <- max(nchar(rows)) - 1

    topbot <- paste("|", paste(rep("=", maxchars), collapse = ""), sep = "")
    cat(header.asciidoc(caption = .$caption, caption.level = .$caption.level, frame = .$frame, grid = .$grid, valign = .$valign, header = .$header, footer = .$footer, cols = col.width, width = .$width))
    
    cat(topbot, "\n")
    if (!is.null(.$cgroup)) {
      cat(paste(cells(span = paste(.$n.cgroup, "+", sep = ""), align = .$calign, valign = .$cvalign, style = .$cstyle), .$cgroup, sep = "| "), "\n")
    }
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
    if (.$list.type == "bullet") mark <- "*"
    if (.$list.type == "number") mark <- "."
    if (.$list.type == "none")   mark <- ""
    charac.x <- vector("character", length(.$x))
    for (i in 1:length(.$x)) {
      if (is.null(.$x[[i]])) next 
      tmp <- sub("(^.*)", paste(mark, "\\1", sep = ""), gsub('\t|(*COMMIT)(*FAIL)', mark, .$x[[i]], perl = TRUE))
      charac.x[i] <- sub(paste('(^\\', mark, '+)(.*)', sep = ""), '\\1 \\2', tmp)
    }
    cat(header.asciidoc(caption = .$caption, caption.level = .$caption.level))
    cat(charac.x, sep = "\n")
  }
  show.t2t <- function(.) {
    indent.mark <- " "
    if (.$list.type == "bullet") mark <- "-"
    if (.$list.type == "number") mark <- "+"
    if (.$list.type == "none")  { mark <- ""; indent.mark = "" }
    charac.x <- vector("character", length(.$x))
    for (i in 1:length(.$x)) {
      tmp <- gsub('\t|(*COMMIT)(*FAIL)', indent.mark, .$x[[i]], perl = TRUE)
      charac.x[i] <- sub("(^ *)", paste("\\1", mark, indent.mark, sep = ""), tmp)
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
  show.t2t <- function(.) {
    args <- rev(as.list(.))
    for (i in seq_along(args)) {
      if (is.null(args[[i]])) next
      print(args[[i]], type = "t2t")
      if (i != length(args)) cat("\n") 
    }
  }
})

