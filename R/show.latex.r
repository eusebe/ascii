beauty.latex <- function(x, beauti = c("e", "m", "s")) {
  x[is.na(x)] <- "NA"
  if (beauti %in% c("s", "h")) {
    y <- as.logical((regexpr("^ *$", x)+1)/2) # bold seulement si != de ""
    if (length(x[!y]) != 0) x[!y] <- sub("(^ *)([:alpha]*)", "\\1\\\\textbf{\\2", sub("([:alpha:]*)( *$)", "\\1}\\2", x[!y]))
  }
  if (beauti == "e") {
    y <- as.logical((regexpr("^ *$", x)+1)/2) # it seulement si != de ""
    if (length(x[!y]) != 0) x[!y] <-sub("(^ *)([:alpha]*)", "\\1\\\\emph{\\2", sub("([:alpha:]*)( *$)", "\\1}\\2", x[!y]))
  }
  if (beauti == "m") {
    y <- as.logical((regexpr("^ *$", x)+1)/2) # it seulement si != de ""
    if (length(x[!y]) != 0) x[!y] <-sub("(^ *)([:alpha]*)", "\\1\\\\texttt{\\2", sub("([:alpha:]*)( *$)", "\\1}\\2", x[!y]))
  }
  return(x)
}

header.latex <- function (caption = NULL, align = NULL, frame = NULL, lgroup = NULL, rgroup = NULL, lalign = "c", ralign = "c") {
  res <- "\\begin{table}\n\\begin{tabular}{"

  lalign <- paste(rep(lalign, length(lgroup)), "|", sep = "", collapse = "")
  ralign <- paste("|", rep(ralign, length(rgroup)), sep = "", collapse = "")
  
  if (!is.null(align)) {
    align <- paste(lalign, paste(align, collapse = ""), ralign, sep = "")
  }
  if (!is.null(frame)) {
    if (frame %in% c("all", "sides"))
      align <- paste("|", align, "|", sep = "")
  }
  if (!is.null(caption))
    caption <- paste("\\caption{", caption, "}\n", sep = "")
  
  res <- paste(res, align, "}\n", caption, sep = "")
  return(res)
}

escape.latex <- function(x) {
  xx <- gsub("\\&", "\\\\&", x)
  xx
}

show.latex.table <- function(x, include.rownames = FALSE, include.colnames = FALSE, rownames = NULL, colnames = NULL, format = "f", digits = 2, decimal.mark = ".", na.print = "", caption = NULL, caption.level = NULL, width = 0, frame = NULL, grid = NULL, valign = NULL, header = FALSE, footer = FALSE, align = NULL, col.width = 1, style = NULL, lgroup = NULL, n.lgroup = NULL, lalign = "c", lvalign = "middle", lstyle = "h", rgroup = NULL, n.rgroup = NULL, ralign = "c", rvalign = "middle", rstyle = "h", tgroup = NULL, n.tgroup = NULL, talign = "c", tvalign = "middle", tstyle = "h", bgroup = NULL, n.bgroup = NULL, balign = "c", bvalign = "middle", bstyle = "h", ...) {

  x <- escape.latex(tocharac(x, include.rownames, include.colnames, rownames, colnames, format, digits, decimal.mark, na.print))
  nrowx <- nrow(x)
  ncolx <- ncol(x)
  col.width <- rep(col.width, length.out = ncolx)
  
  line_separator <- FALSE
  vsep <- cbind("", expand("&", nrow(x), ncol(x)-1))

  if (header + footer > nrowx) {
    header <- nrowx
    footer <- FALSE
  }
  if (is.logical(header) & header)
    header <- 1
  if (header > 0) {
    header <- 1:min(c(header, nrowx))
  }
  if (is.logical(footer) & footer)
    footer <- 1
  if (footer > 0) {
    footer <- nrow(x):(nrow(x)+1-min(c(footer, nrowx))) + length(tgroup)
  }

  endstyle <- ""
  if (!is.null(style) | sum(c(header, footer)) > 0) {
    if (is.null(style))
      style <- ""
    style <- expand(style, nrowx, ncolx)
    endstyle <- expand("", nrowx, ncolx)
    style[unique(c(header, footer)), ] <- "h"
    style[!(style %in% c("s", "e", "m", "h"))] <- ""
    endstyle[style %in% c("s", "h")] <- "}"
    style[style %in% c("s", "h")] <- "\\textbf{"
    endstyle[style == "e"] <- "}"
    style[style == "e"] <- "\\emph{"
    endstyle[style == "m"] <- "}"
    style[style == "m"] <- "\\textt{"
  } else {
    style <- ""
  }
  x <- paste.matrix(style, x, endstyle, sep = "")

  results <- print.character.matrix(x, line_separator = line_separator, vsep = vsep, print = FALSE)
  results <- paste(results, "\\\\")
    
  # groups
  if (!is.null(lgroup)) {
    if (!is.list(lgroup))
      lgroup <- list(lgroup)
    n.lgroup <- groups(lgroup, n.lgroup, nrowx-include.colnames)[[2]]
  }
  if (!is.null(rgroup)) {
    if (!is.list(rgroup))
      rgroup <- list(rgroup)
    n.rgroup <- groups(rgroup, n.rgroup, nrowx-include.colnames)[[2]]
  }
  if (!is.null(tgroup)) {
    if (!is.list(tgroup))
      tgroup <- list(tgroup)
    n.tgroup <- groups(tgroup, n.tgroup, ncolx-include.rownames)[[2]]
  }
  if (!is.null(bgroup)) {
    if (!is.list(bgroup))
      bgroup <- list(bgroup)
    n.bgroup <- groups(bgroup, n.bgroup, ncolx-include.rownames)[[2]]
  }
  
  if (!is.null(lgroup)) {
    for (i in 1:length(lgroup)) {  
      pos.lgroup <- ngroups(lgroup[[i]], n.lgroup[[i]], n = nrowx)
      results <- paste(" &", results)
      results[pos.lgroup[, 2]+include.colnames] <- paste(paste("\\multirow{", pos.lgroup[, 3], "}{*}{", beauty.latex(pos.lgroup[, 1], lstyle), "}", sep = ""), results[pos.lgroup[, 2]+include.colnames])
      clinepos <- pos.lgroup[, 2]+pos.lgroup[, 3]+include.colnames-1
      clinepos <- clinepos[clinepos != nrowx]
      results[clinepos] <- paste(results[clinepos], paste("\\cline{", i, "-", i, "}", sep = ""), sep = "")
    }
  }
  
  if (!is.null(rgroup)) {
    for (i in 1:length(rgroup)) {
      pos.rgroup <- ngroups(rgroup[[i]], n.rgroup[[i]], n = nrowx)
      results <- paste(results, "& ")
      results[pos.rgroup[, 2]+include.colnames] <- paste(results[pos.rgroup[, 2]+include.colnames], paste("\\multirow{", pos.rgroup[, 3], "}{*}{", beauty.latex(pos.rgroup[, 1], lstyle), "}", sep = ""))
    }
  }

  if (!is.null(tgroup)) {
    for (i in 1:length(tgroup)) {
      pos.tgroup <- ngroups(tgroup[[i]], n.tgroup[[i]], n = ncolx)
      results <- c(paste("\\multicolumn{", pos.tgroup[, 3], "}{", talign, "}{", beauty.latex(pos.tgroup[, 1], tstyle), "}", sep = "", collapse = " & "), results)
    }
  }
  
  if (!is.null(bgroup)) {
    for (i in 1:length(bgroup)) {
      pos.bgroup <- ngroups(bgroup[[i]], n.bgroup[[i]], n = ncolx)
      results <- c(results, paste("\\multicolumn{", pos.bgroup[, 3], "}{", balign, "}{", beauty.latex(pos.bgroup[, 1], bstyle), "}", sep = "", collapse = " & "))
    }
  }
  
  results <- c("\\hline", results, "\\hline")

  if (is.null(align)) {
    align = "l"
  }
  align <- rep(align, length = ncolx)
  head <- header.latex(caption = caption, align = align, frame = frame, lgroup = lgroup, rgroup = rgroup, lalign = lalign, ralign = ralign)

  cat(head)
  cat(results, sep = "\n")
  cat("\\end{tabular}\n\\end{table}\n")
}

show.latex.list <- function(x, caption = NULL, caption.level = NULL, list.type = "bullet", ...) {
  
  if (list.type == "bullet") mark <- rep("\\*", length(x))
  if (list.type == "number") mark <- rep("\\.", length(x))
  if (list.type == "none")   mark <- rep("", length(x))
  if (list.type == "label") {
    if (is.null(names(x))) {
      namesx <- paste("[[", 1:length(x), "]]", sep = "")
    } else {
      namesx <- names(x)
    }
    mark <- paste(namesx, "::\n  ", sep = "")
  }
  
  charac.x <- vector("character", length(x))
  for (i in 1:length(x)) {
    if (is.null(x[[i]])) next
    tmp <- x[[i]]
    if (list.type == "label") tmp <- sub("^\t*", "", tmp)
    tmp <- sub("(^.*)", paste(mark[i], "\\1", sep = ""), gsub('\t|(*COMMIT)(*FAIL)', mark[i], tmp, perl = TRUE))
    if (list.type != "none")
      tmp <- sub(paste('(^', mark[i], '+)(.*)', sep = ""), '\\1 \\2', tmp)
    charac.x[i] <- tmp
  }
  cat(header.latex(caption = caption, caption.level = caption.level))
  cat(charac.x, sep = "\n")
}
