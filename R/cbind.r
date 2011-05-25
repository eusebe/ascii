require(proto)

asciiCbind <- proto(expr = {
  new <- function(.,
    ...,
    caption, 
    caption.level,
    frame,
    grid,
    col.width,
    width) proto(.,
    args = list(...),
    caption = caption, 
    caption.level = caption.level,
    frame = frame,
    grid = grid,
    col.width = col.width,
    width = width)

  test.nrow <- function(args) {

    nrow.args <- sapply(args, function(x) {
      if (!is.null(x$tgroup)) {
        if (!is.list(x$tgroup))
          x$tgroup <- list(x$tgroup)
      }
      if (!is.null(x$bgroup)) {
        if (!is.list(x$bgroup))
          x$bgroup <- list(x$bgroup)
      }
      nrow(x$x) + length(x$tgroup) + length(x$bgroup) + x$include.colnames
    })

    if (length(unique(nrow.args)) != 1)
      stop("x and y must have same number of rows", call. = FALSE)
  }

  show.asciidoc <- function(.) {
    test.nrow(.$args)

    args.output <- lapply(.$args, function(x) {
      x$caption <- NULL
      x$caption.level <- NULL
      x$frame <- NULL
      x$grid <- NULL
      x$col.width <- 1
      x$width <- 0
      capture.output(x$show.asciidoc())
    })

    args.output[-length(args.output)] <- lapply(args.output[-length(args.output)], function(x) {
      substring(x[1], nchar(x[1]), nchar(x[1])) <- ""
      substring(x[length(x)], nchar(x[length(x)]), nchar(x[length(x)])) <- ""
      x
    })

    args.output[-1] <- lapply(args.output[-1], function(x) {
      substring(x[1], 1, 1) <- "="
      substring(x[length(x)], 1, 1) <- "="
      x[c(-1, -length(x))] <- paste("", x[c(-1, -length(x))])
      x
    })
    
    cat(header.asciidoc(caption = .$caption, caption.level = .$caption.level, frame = .$frame, grid = .$grid, col.width = .$col.width, width = .$width))
    cat(do.call("paste", c(args.output, list(sep = ""))), sep = "\n")
  }

  show.rest <- function(.) {
    test.nrow(.$args)

    args.output <- lapply(.$args, function(x) {
      x$caption <- NULL
      x$caption.level <- NULL
      capture.output(x$show.rest())[-1]
    })

    args.output[-1] <- lapply(args.output[-1], function(x) {
      substring(x, 1, 1) <- ""
      x
    })
    
    cat(header.rest(caption = .$caption, caption.level = .$caption.level), sep = "\n")
    cat(do.call("paste", c(args.output, list(sep = ""))), sep = "\n")
  }

  show.org <- function(.) {
    test.nrow(.$args)

    args.output <- lapply(.$args, function(x) {
      x$caption <- NULL
      x$caption.level <- NULL
      capture.output(x$show.org())
    })

    args.output[-length(args.output)] <- lapply(args.output[-length(args.output)], function(x) {
      x <- sub("\\|$", "", x)
      x
    })

    args.output[-1] <- lapply(args.output[-1], function(x) {
      x <- sub("^(\\|)(-+)", "\\+\\2", x)
      x
    })
    
    cat(header.org(caption = .$caption, caption.level = .$caption.level), sep = "\n")
    cat(do.call("paste", c(args.output, list(sep = ""))), sep = "\n")
  }

  show.t2t <- function(.) {
    test.nrow(.$args)

    .$args[-length(.$args)] <- lapply(.$args[-length(.$args)], function(x) {
      x$frame <- TRUE
      x
    })

    args.output <- lapply(.$args, function(x) {
      x$caption <- NULL
      x$caption.level <- NULL
      capture.output(x$show.t2t())
    })

    args.output[-1] <- lapply(args.output[-1], function(x) {
      x <- sub("^\\|+", "", x)
      x
    })
    
    cat(header.t2t(caption = .$caption, caption.level = .$caption.level))
    cat(do.call("paste", c(args.output, list(sep = ""))), sep = "\n")
  }

  show.textile <- function(.) {
    test.nrow(.$args)

    args.output <- lapply(.$args, function(x) {
      x$caption <- NULL
      x$caption.level <- NULL
      x$frame <- NULL
      x$width <- 0
      capture.output(x$show.textile())
    })

    args.output[-length(args.output)] <- lapply(args.output[-length(args.output)], function(x) {
      x <- sub("\\|$", "", x)
      x
    })

    cat(header.textile(caption = .$caption, caption.level = .$caption.level, frame = .$frame, width = .$width))
    cat(do.call("paste", c(args.output, list(sep = ""))), sep = "\n")
  }

  show.pandoc <- function(.) {
    test.nrow(.$args)

    args.output <- lapply(.$args, function(x) {
      x$caption <- NULL
      x$caption.level <- NULL
      capture.output(x$show.pandoc())
    })

    args.output[-length(args.output)] <- lapply(args.output[-length(args.output)], function(x) {
      x <- paste(x, " ", sep = "")
      x
    })

    cat(header.pandoc(caption = .$caption, caption.level = .$caption.level))
    cat(do.call("paste", c(args.output, list(sep = ""))), sep = "\n")
  }
})

##' Cbind two ascii objects
##'
##' This function binds cols of two ascii table.
##' @title Cbind two ascii objects
##' @param ... ascii objects
##' @param caption see \code{?ascii}
##' @param caption.level see \code{?ascii}
##' @param frame see \code{?ascii}
##' @param grid see \code{?ascii}
##' @param col.width see \code{?ascii}
##' @param width see \code{?ascii}
##' @return An ascii object.
##' @export
##' @author David Hajage
cbind.ascii <- function(..., caption = NULL, caption.level = NULL, frame = NULL, grid = NULL, col.width = 1, width = 0) {
  results <- asciiCbind$new(..., caption = caption, caption.level = caption.level, frame = frame, grid = grid, col.width = col.width, width = width)
  class(results) <- c("ascii", "proto", "environment")
  results
}
