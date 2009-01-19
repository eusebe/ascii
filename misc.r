# generate column specifiers
cols <- function(ncol, align = "", col.width = 1, style = "") {

  if (align != "") {  
    align <- unlist(strsplit(align, ""))
    align <- rep(align, length.out = ncol)
    align[align == "l"] <- "<"
    align[align == "c"] <- "^"
    align[align == "r"] <- ">"
  }
  
  if (style != "") {
    style <- unlist(strsplit(style, ""))
    style <- rep(style, length.out = ncol)
  }
  if (col.width > 1) {
    col.width <- rep(col.width, length.out = ncol)
  }
  else col.width <- ""

  res <- paste(align, col.width, style, collapse = ",", sep = "")
  return(res)
}
#~ cols()
#~ cols(ncol = 3, align = "llrclr")

# generate headers
header <- function(caption = "", frame = "", grid = "", valign = "", header = FALSE, footer = FALSE, cols = "", width = 0) {

  if (frame != "") frame <- paste('frame="', frame, '"', sep = "")
  if (grid != "") grid <- paste('grid="', grid, '"', sep = "")
  if (valign != "") valign <- paste('valign="', valign, '"', sep = "")
  if (cols != "") cols <- paste('cols="', cols, '"', sep = "")
  if (width != 0) {
    width <- paste('width="', width, '%"', sep = "")
  }
  else width <- ""
  
  if (header | footer) {
    options <- 'options="'
    if (header & footer) options <- paste(options, 'header,footer', '"', sep = "")
    else if (header)     options <- paste(options, 'header', '"', sep = "")
    else if (footer)     options <- paste(options, 'footer', '"', sep = "")
  }
  else {options <- ""}
  
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
#~ cat(header(caption = "A title", header = T, footer = T))
#~ cat(header(caption = "A title", width = 30))
