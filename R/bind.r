asciiCbind <- proto(expr = {
  new <- function(.,
    x,
    y,
    caption, 
    caption.level,
    frame,
    grid,
    col.width,
    width) proto(.,
    x = x,
    caption = caption, 
    caption.level = caption.level,
    frame = frame,
    grid = grid,
    col.width = col.width,
    width = width)

  show.asciidoc <- function(.) {
    xx <- .$x
    yy <- .$y
    
    xx$caption <- NULL
    yy$caption <- NULL
    xx$frame <- NULL
    yy$frame <- NULL
    xx$grid <- NULL
    yy$grid <- NULL
    xx$col.width <- 1
    yy$col.width <- 1
    xx$width <- 0
    yy$width <- 0

    xxx <- capture.output(xx$show.asciidoc())
    yyy <- capture.output(yy$show.asciidoc())
    substring(xxx[1], nchar(xxx[1]), nchar(xxx[1])) <- ""
    substring(xxx[length(xxx)], nchar(xxx[length(xxx)]), nchar(xxx[length(xxx)])) <- ""
    substring(yyy[1], 1, 1) <- "="
    substring(yyy[length(yyy)], 1, 1) <- "="
    yyy[c(-1, -length(yyy))] <- paste("", yyy[c(-1, -length(yyy))])
    
    cat(header.asciidoc(caption = .$caption, caption.level = .$caption.level, frame = .$frame, grid = .$grid, col.width = .$col.width, width = .$width))
    cat(paste(xxx, yyy, sep = ""), sep = "\n")
  }

  show.rest <- function(.) {
    xx <- .$x
    yy <- .$y
    
    xx$caption <- NULL
    yy$caption <- NULL
    xx$caption.level <- NULL
    yy$caption.level <- NULL

    xxx <- capture.output(xx$show.rest())[-1]
    yyy <- capture.output(yy$show.rest())[-1]
    substring(yyy, 1, 1) <- ""
    cat(header.rest(caption = .$caption, caption.level = .$caption.level), sep = "\n")
    cat(paste(xxx, yyy, sep = ""), sep = "\n")
  }

  show.org <- function(.) {

  }

  show.t2t <- function(.) {

  }

  show.textile <- function(.) {

  }
})
