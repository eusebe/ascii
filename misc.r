# generate column specifiers
cols <- function(ncol, align = "l", col.width = 1, style = "d", multiplier = 1) {

  if (multiplier > 1) {
    ncol <- 1
    multiplier <- paste(multiplier, "*", sep = "")
  }
  else {
    multiplier <- ""
  }
  
  align <- unlist(strsplit(align, ""))
  style <- unlist(strsplit(style, ""))

  align <- rep(align, length.out = ncol)
  col.width <- rep(col.width, length.out = ncol)
  style <- rep(style, length.out = ncol)
  
  align[align == "l"] <- "<"
  align[align == "c"] <- "^"
  align[align == "r"] <- ">"

  res <- paste(multiplier, align, col.width, style, collapse = ",", sep = "")
  return(res)
}
#~ cols(ncol = 3, align = "llrclr")
#~ cols(ncol = 3, align = "llrclr", multiplier = 3)

# generate headers
header <- function(caption = "", frame = "", grid = "", valign = "", options = "", cols = "", width = "") {

  if (frame != "") frame <- paste('frame="', frame, '"', sep = "")
  if (grid != "") grid <- paste('grid="', grid, '"', sep = "")
  if (valign != "") valign <- paste('valign="', valign, '"', sep = "")
  if (options != "") options <- paste('options="', options, '"', sep = "")
  if (cols != "") cols <- paste('cols="', cols, '"', sep = "")
  if (width != "") width <- paste('width="', width, '"', sep = "")

  listarg <- c(frame, grid, valign, options, cols, width)
  listarg <- listarg[listarg != ""]

  if (length(listarg) != 0) {
    res <- paste("[", paste(listarg, collapse = ","), "]\n", sep = "")
  }
  else res <- ""
  if (caption != "") res <- paste(".", caption, "\n", res, sep = "")
  return(res)
}
#~ cat(header())
#~ cat(header(frame = "none"))
#~ cat(header(frame = "none", cols = cols(ncol = 3, align = "llrclr")))
#~ cat(header(caption = "A title", frame = "none", cols = cols(ncol = 3, align = "llrclr")))
#~ cat(header(caption = "A title"))

