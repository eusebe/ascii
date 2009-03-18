# generate column specifiers for asciidoc
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
  if (sum(col.width) > length(col.width)) {
    col.width <- rep(col.width, length.out = ncol)
  }
  else col.width <- ""

  res <- paste(align, col.width, style, collapse = ",", sep = "")
  return(res)
}
#~ cols()
#~ cols(ncol = 3, align = "llrclr")

# generate headers for asciidoc
header.asciidoc <- function(caption = "", frame = "", grid = "", valign = "", header = FALSE, footer = FALSE, cols = "", width = 0) {

  if (frame != "") frame <- paste('frame="', switch(frame, topbot = "topbot", sides = "sides", all = "all", none = "none"), '"', sep = "")
  if (grid != "") grid <- paste('grid="', switch(grid, all = "all", rows = "rows", cols = "cols", none = "none"), '"', sep = "")
  if (valign != "") valign <- paste('valign="', switch(valign, top = "top", bottom = "bottom", middle = "middle"), '"', sep = "")
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
#~ cat(header.asciidoc())
#~ cat(header.asciidoc(frame = "none"))
#~ cat(header.asciidoc(frame = "none", cols = cols(ncol = 3, align = "llrclr")))
#~ cat(header.asciidoc(caption = "A title", frame = "none", cols = cols(ncol = 3, align = "llrclr")))
#~ cat(header.asciidoc(caption = "A title"))
#~ cat(header.asciidoc(caption = "A title", header = T, footer = T))
#~ cat(header.asciidoc(caption = "A title", width = 30))

# generate headers for textile
header.textile <- function(frame = "") {

  if (frame != "") frame <- switch(frame, topbot = "border-top:1px solid black;border-bottom:1px solid black", sides = "border-left:1px solid black;border-right:1px solid black", all = "border:1px solid black", none = "")

  listarg <- frame
  listarg <- listarg[listarg != ""]

  if (length(listarg) != 0) {
    res <- paste("table{", paste(listarg, collapse = ";"), "}\n", sep = "")
  }
  else res <- ""
  return(res)
}

# beautify for t2t
beauty.t2t <- function(x, beauti = c("e", "m", "s")) {
  if (beauti == "s") {
    y <- as.logical((regexpr("^ *$", x)+1)/2) | as.logical((regexpr("\\*\\*.*\\*\\*", x)+1)/2) # bold seulement si != de "" et si pas de bold
    if (length(x[!y]) != 0) x[!y] <- sub("(^ *)([:alpha]*)", "\\1\\*\\*\\2", sub("([:alpha:]*)( *$)", "\\1\\*\\*\\2", x[!y]))
  }
  if (beauti == "e") {
    y <- as.logical((regexpr("^ *$", x)+1)/2) | as.logical((regexpr("//.*//", x)+1)/2) # it seulement si != de "" et si pas de it
    if (length(x[!y]) != 0) x[!y] <-sub("(^ *)([:alpha]*)", "\\1//\\2", sub("([:alpha:]*)( *$)", "\\1//\\2", x[!y])) 
  }
  if (beauti == "m") {
    y <- as.logical((regexpr("^ *$", x)+1)/2) | as.logical((regexpr("``.*``", x)+1)/2) # it seulement si != de "" et si pas de mono
    if (length(x[!y]) != 0) x[!y] <-sub("(^ *)([:alpha]*)", "\\1``\\2", sub("([:alpha:]*)( *$)", "\\1``\\2", x[!y])) 
  }
  return(x)
}

# beautify for textile
beauty.textile <- function(x, beauti = c("e", "m", "s", "header", "r", "c")) {
  if (beauti == "s") {
    y <- as.logical((regexpr("^ *$", x)+1)/2) | as.logical((regexpr("\\*.*\\*", x)+1)/2) # bold seulement si != de "" et si pas de bold
    if (length(x[!y]) != 0) x[!y] <- sub("(^ *)([:alpha]*)", "\\1\\*\\2", sub("([:alpha:]*)( *$)", "\\1\\*\\2", x[!y]))
  }
  if (beauti == "e") {
    y <- as.logical((regexpr("^ *$", x)+1)/2) | as.logical((regexpr("/.*/", x)+1)/2) # it seulement si != de "" et si pas de it
    if (length(x[!y]) != 0) x[!y] <-sub("(^ *)([:alpha]*)", "\\1/\\2", sub("([:alpha:]*)( *$)", "\\1/\\2", x[!y])) 
  }
  if (beauti == "m") {
    y <- as.logical((regexpr("^ *$", x)+1)/2) | as.logical((regexpr("<code>.*</code>", x)+1)/2) # it seulement si != de "" et si pas de mono
    if (length(x[!y]) != 0) x[!y] <-sub("(^ *)([:alpha]*)", "\\1<code>\\2", sub("([:alpha:]*)( *$)", "\\1</code>\\2", x[!y])) 
  }
  if (beauti == "header") {
    y <- as.logical((regexpr("^ *$", x)+1)/2) | as.logical((regexpr("_\\..*", x)+1)/2) # it seulement si != de "" et si pas de titre
    if (length(x[!y]) != 0) x[!y] <- paste("_. ", x[!y], sep = "") 
  }
  if (beauti == "r") {
    y <- as.logical((regexpr("^ *$", x)+1)/2) | as.logical((regexpr(">\\..*", x)+1)/2) # it seulement si != de "" et si pas de r
    if (length(x[!y]) != 0) x[!y] <- paste(">. ", x[!y], sep = "") 
  }
  if (beauti == "c") {
    y <- as.logical((regexpr("^ *$", x)+1)/2) | as.logical((regexpr("=\\..*", x)+1)/2) # it seulement si != de "" et si pas de r
    if (length(x[!y]) != 0) x[!y] <- paste("=. ", x[!y], sep = "") 
  }
  if (beauti == "l") {
    y <- as.logical((regexpr("^ *$", x)+1)/2) | as.logical((regexpr("<\\..*", x)+1)/2) # it seulement si != de "" et si pas de r
    if (length(x[!y]) != 0) x[!y] <- paste("<. ", x[!y], sep = "") 
  }
  return(x)
}
beauty.textile(1:4, "header")

