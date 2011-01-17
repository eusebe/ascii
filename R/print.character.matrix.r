##' @keywords internal
##' @param x x
##' @param n n
div <- function(x, n = 2) {
  xx <- floor(x/n)
  c(xx, x - xx)
}

##' @keywords internal
##' @param x x
##' @param times times
rep.char <- function(x, times = 1) {
  paste(rep(x, times), collapse = "")
}

##' @keywords internal
##' @param x x
##' @param vsep vsep
##' @param before_vsep before_vsep
##' @param after_vsep after_vsep
##' @param hsep hsep
##' @param csep csep
##' @param before_cell_content before_cell_content
##' @param after_cell_content after_cell_content
##' @param line_separator line_separator
##' @param line_separator_pos line_separator_pos
##' @param justify justify
##' @param right_alignment right_alignment
##' @param print print
print.character.matrix <- function(x, vsep = "|", before_vsep = "", after_vsep = "", hsep = "-", csep = "+", before_cell_content = " ", after_cell_content = " ", line_separator = TRUE, line_separator_pos = NULL, justify = c("left", "right", "centre", "none"), right_alignment = FALSE, print = TRUE) {

  justify <- justify[1]
  
  # after et before cell_content
  x <- paste.matrix(before_cell_content, x, after_cell_content, sep = "", transpose.vector = TRUE)
  x <- apply(x, 2, function(xx) as.character(format(xx, trim = TRUE, justify = justify)))
  if (is.vector(x))
    x <- t(x)
  
  # dim
  nrowx <- nrow(x)
  ncolx <- ncol(x)

  # vseps
  if (is.null(dim(vsep))) {
    vseps <- expand(vsep, nrowx, ncolx+1, drop = FALSE)
  } else {
    vseps <- expand(vsep, nrowx, ncolx+1, what = "", drop = FALSE)
  }
  before_vseps <- expand(before_vsep, nrow(vseps), ncol(vseps), what = "")
  after_vseps <- expand(after_vsep, nrow(vseps), ncol(vseps), what = "")
  final_vseps <- paste.matrix(before_vseps, vseps, after_vseps, sep = "")
  
  # nchar
  ncharvseps <- nchar(vseps)
  ncharx <- nchar(x)
  ncharbefore <- nchar(before_vseps)
  ncharafter <- nchar(after_vseps)
  nchartot <- ncharafter[, 1:(ncol(ncharafter)-1)] + ncharx + ncharbefore[, -1]
  nchartotmax <- apply(nchartot, 2, max)

  # x alignment
  addtox <- matrix(nchartotmax, nrow(nchartot), ncol(nchartot), T) - nchartot
  if (justify == "left") {
    addtoxleft <- ""
    addtoxright <- apply(addtox, 1:2, function(x) paste(rep(" ", x), collapse = ""))
  }
  if (justify == "centre" | justify == "none") {
    addtox <- apply(addtox, 1:2, div)
    addtoxleft <- apply(addtox[, , 1], 1:2, function(x) paste(rep(" ", x), collapse = ""))
    addtoxright <- apply(addtox[, , 2], 1:2, function(x) paste(rep(" ", x), collapse = ""))
  }
  if (justify == "right") {
    addtoxleft <- apply(addtox, 1:2, function(x) paste(rep(" ", x), collapse = ""))
    addtoxright <- ""
  }
  x <- paste.matrix(addtoxleft, x, addtoxright, sep = "")

  # rows
  lines <- interleave.matrix(final_vseps, x, byrow = FALSE)
  lines <- matrix(lines[!is.na(lines)], nrow = nrowx)
  lines <- paste.matrix(lines, sep = "", collapse = "")

  # row separators
  row_lines <- NULL
  if (line_separator) {
    hseps <- expand(hsep, nrowx+1, ncolx)
    hseps <- t(apply(hseps, 1, function(x) Vectorize(rep.char, c("x", "times"))(x, nchartotmax)))
    if (ncolx == 1)
      hseps <- t(hseps)
    cseps <- expand(csep, nrowx+1, ncolx+1)
    row_lines <- paste.matrix(interleave.matrix(cseps, hseps, byrow = FALSE), collapse = "", byrow = FALSE)
    
    if (!is.null(line_separator_pos)) {
      line_separator_pos <- sort(unique(ifelse(line_separator_pos >= 0, line_separator_pos + 1, nrowx + 1 + line_separator_pos + 1)))
      line_separator_pos <- line_separator_pos[line_separator_pos <= nrowx + 1]
      row_lines[-line_separator_pos] <- NA
    }
  }

  # all together
  results <- interleave(row_lines, lines)
  results <- results[!is.na(results)]
  
  # right alignment of the table
  if (right_alignment) {
    ncharres <- nchar(results)
    nchardiff <- sapply(mapply(rep, times = max(ncharres) - ncharres, x = " "), paste, collapse = "")
    results <- paste(nchardiff, results, sep = "")
  }

  # cat
  if (print)
    cat(results, sep = "\n")
  invisible(results)
}
