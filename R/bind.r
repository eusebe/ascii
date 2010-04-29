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

  test.nrow <- function(xx, yy) {

    if (!is.null(xx$tgroup)) {
      if (!is.list(xx$tgroup))
        xx$tgroup <- list(xx$tgroup)
    }
    if (!is.null(xx$bgroup)) {
      if (!is.list(xx$bgroup))
        xx$bgroup <- list(xx$bgroup)
    }
    if (!is.null(yy$tgroup)) {
      if (!is.list(yy$tgroup))
        yy$tgroup <- list(yy$tgroup)
    }
    if (!is.null(yy$bgroup)) {
      if (!is.list(yy$bgroup))
        yy$bgroup <- list(yy$bgroup)
    }

    nrowx <- nrow(xx$x) + length(xx$tgroup) + length(xx$bgroup) + xx$include.colnames
    nrowy <- nrow(yy$x) + length(yy$tgroup) + length(yy$bgroup) + yy$include.colnames
    if (nrowx != nrowy)
      stop("x and y must have same number of rows", call. = FALSE)
  }
  
  show.asciidoc <- function(.) {
    test.nrow(.$x, .$y)

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
    test.nrow(.$x, .$y)

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
    test.nrow(.$x, .$y)
    
    xx <- .$x
    yy <- .$y
    
    xx$caption <- NULL
    yy$caption <- NULL
    xx$caption.level <- NULL
    yy$caption.level <- NULL

    xxx <- capture.output(xx$show.org())
    yyy <- capture.output(yy$show.org())
    xxx <- sub("\\|$", "", xxx)
    yyy <- sub("^(\\|)(-+)", "\\+\\2", yyy)
    
    cat(header.org(caption = .$caption, caption.level = .$caption.level), sep = "\n")
    cat(paste(xxx, yyy, sep = ""), sep = "\n")    
  }

  show.t2t <- function(.) {
    test.nrow(.$x, .$y)

    xx <- .$x
    yy <- .$y
    
    xx$caption <- NULL
    yy$caption <- NULL
    xx$caption.level <- NULL
    yy$caption.level <- NULL
    xx$frame <- TRUE
    
    xxx <- capture.output(xx$show.t2t())
    yyy <- capture.output(yy$show.t2t())

    yyy <- sub("^\\|+", "", yyy)
    cat(header.t2t(caption = .$caption, caption.level = .$caption.level))
    cat(paste(xxx, yyy, sep = ""), sep = "\n")
  }

  show.textile <- function(.) {
    test.nrow(.$x, .$y)
    
    xx <- .$x
    yy <- .$y
    
    xx$caption <- NULL
    yy$caption <- NULL
    xx$frame <- NULL
    yy$frame <- NULL
    xx$width <- 0
    yy$width <- 0

    xxx <- capture.output(xx$show.textile())
    yyy <- capture.output(yy$show.textile())
    xxx <- sub("\\|$", "", xxx)
    cat(header.textile(caption = .$caption, caption.level = .$caption.level, frame = .$frame, width = .$width))
    cat(paste(xxx, yyy, sep = ""), sep = "\n")
  }
})

asciiRbind <- proto(expr = {
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

  test.ncol <- function(xx, yy) {

    if (!is.null(xx$lgroup)) {
      if (!is.list(xx$lgroup))
        xx$lgroup <- list(xx$lgroup)
    }
    if (!is.null(xx$rgroup)) {
      if (!is.list(xx$rgroup))
        xx$rgroup <- list(xx$rgroup)
    }
    if (!is.null(yy$lgroup)) {
      if (!is.list(yy$lgroup))
        yy$lgroup <- list(yy$lgroup)
    }
    if (!is.null(yy$rgroup)) {
      if (!is.list(yy$rgroup))
        yy$rgroup <- list(yy$rgroup)
    }

    ncolx <- ncol(xx$x) + length(xx$lgroup) + length(xx$rgroup) + xx$include.rownames
    ncoly <- ncol(yy$x) + length(yy$lgroup) + length(yy$rgroup) + yy$include.rownames
    if (ncolx != ncoly)
      stop("x and y must have same number of cols", call. = FALSE)
  }
  
  show.asciidoc <- function(.) {
    test.ncol(.$x, .$y)

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
    xxx <- xxx[-length(xxx)]
    yyy <- capture.output(yy$show.asciidoc())
    yyy <- yyy[-1]

    xy <- c(xxx, yyy)
    cat(header.asciidoc(caption = .$caption, caption.level = .$caption.level, frame = .$frame, grid = .$grid, col.width = .$col.width, width = .$width))
    cat(xy, sep = "\n")
  }

  show.rest <- function(.) {
    test.ncol(.$x, .$y)

    xx <- .$x
    yy <- .$y
    
    xx$caption <- NULL
    yy$caption <- NULL
    xx$caption.level <- NULL
    yy$caption.level <- NULL

    xxx <- capture.output(xx$show.rest())
    xxx <- xxx[-length(xxx)]
    yyy <- capture.output(yy$show.rest())
    yyy <- yyy[-1]

    xy <- c(xxx, yyy)
    
    cat(header.rest(caption = .$caption, caption.level = .$caption.level), sep = "\n")
    cat(xy, sep = "\n")
    warning("show.rest() currently produces a non valid markup", call. = FALSE)
  }

  show.org <- function(.) {
    test.ncol(.$x, .$y)

    xx <- .$x
    yy <- .$y
    
    xx$caption <- NULL
    yy$caption <- NULL
    xx$caption.level <- NULL
    yy$caption.level <- NULL

    xxx <- capture.output(xx$show.org())
    yyy <- capture.output(yy$show.org())

    xy <- c(xxx, yyy)
    
    cat(header.org(caption = .$caption, caption.level = .$caption.level), sep = "\n")
    cat(xy, sep = "\n")
    warning("show.org() currently produces a non valid markup", call. = FALSE)
  }

  show.t2t <- function(.) {
    test.ncol(.$x, .$y)

    xx <- .$x
    yy <- .$y
    
    xx$caption <- NULL
    yy$caption <- NULL
    xx$caption.level <- NULL
    yy$caption.level <- NULL

    xxx <- capture.output(xx$show.t2t())
    yyy <- capture.output(yy$show.t2t())

    xy <- c(xxx, yyy)
    
    cat(header.t2t(caption = .$caption, caption.level = .$caption.level), sep = "\n")
    cat(xy, sep = "\n")
  }

  show.textile <- function(.) {
    test.ncol(.$x, .$y)

    xx <- .$x
    yy <- .$y
        
    xx$caption <- NULL
    yy$caption <- NULL
    xx$frame <- NULL
    yy$frame <- NULL
    xx$width <- 0
    yy$width <- 0

    xxx <- capture.output(xx$show.textile())
    yyy <- capture.output(yy$show.textile())

    xy <- c(xxx, yyy)
    
    cat(header.textile(caption = .$caption, caption.level = .$caption.level, frame = .$frame, width = .$width))
    cat(xy, sep = "\n")
  }
})

rbind.ascii <- function(x, y, caption = NULL, caption.level = NULL, frame = NULL, grid = NULL, col.width = 1, width = 0) {
  results <- asciiRbind$new(x, y, caption = caption, caption.level = caption.level, frame = frame, grid = grid, col.width = col.width, width = width)
  class(results) <- c("ascii", "proto", "environment")
  results
}

cbind.ascii <- function(x, y, caption = NULL, caption.level = NULL, frame = NULL, grid = NULL, col.width = 1, width = 0) {
  results <- asciiCbind$new(x, y, caption = caption, caption.level = caption.level, frame = frame, grid = grid, col.width = col.width, width = width)
  class(results) <- c("ascii", "proto", "environment")
  results
}
