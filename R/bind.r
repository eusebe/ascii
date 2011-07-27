## asciiRbind <- setRefClass("asciiRbind",
##                           fields = c("x",
##                             "y",
##                             "caption", 
##                             "caption.level",
##                             "frame",
##                             "grid",
##                             "col.width",
##                             "width"),
##                           methods = list(
##                             test.ncol <- function(xx, yy) {
                              
##                               if (!is.null(xx$lgroup)) {
##                                 if (!is.list(xx$lgroup))
##                                   xx$lgroup <- list(xx$lgroup)
##                               }
##                               if (!is.null(xx$rgroup)) {
##                                 if (!is.list(xx$rgroup))
##                                   xx$rgroup <- list(xx$rgroup)
##                               }
##                               if (!is.null(yy$lgroup)) {
##                                 if (!is.list(yy$lgroup))
##                                   yy$lgroup <- list(yy$lgroup)
##                               }
##                               if (!is.null(yy$rgroup)) {
##                                 if (!is.list(yy$rgroup))
##                                   yy$rgroup <- list(yy$rgroup)
##                               }
                              
##                               ncolx <- ncol(xx$x) + length(xx$lgroup) + length(xx$rgroup) + xx$include.rownames
##                               ncoly <- ncol(yy$x) + length(yy$lgroup) + length(yy$rgroup) + yy$include.rownames
##                               if (ncolx != ncoly)
##                                 stop("x and y must have same number of cols", call. = FALSE)
##                             },
  
##                             show.asciidoc <- function(.) {
##                               test.ncol(.self$x, .self$y)
                              
##                               xx <- .self$x
##                               yy <- .self$y
                              
##                               xx$caption <- NULL
##                               yy$caption <- NULL
##                               xx$frame <- NULL
##                               yy$frame <- NULL
##                               xx$grid <- NULL
##                               yy$grid <- NULL
##                               xx$col.width <- 1
##                               yy$col.width <- 1
##                               xx$width <- 0
##                               yy$width <- 0
                              
##                               xxx <- capture.output(xx$show.asciidoc())
##                               xxx <- xxx[-length(xxx)]
##                               yyy <- capture.output(yy$show.asciidoc())
##                               yyy <- yyy[-1]
                              
##                               xy <- c(xxx, yyy)
##                               cat(header.asciidoc(caption = .self$caption, caption.level = .self$caption.level, frame = .self$frame, grid = .self$grid, col.width = .self$col.width, width = .self$width))
##                               cat(xy, sep = "\n")
##                             },

##                             show.rest <- function(.) {
##                               test.ncol(.self$x, .self$y)
                              
##                               xx <- .self$x
##                               yy <- .self$y
                              
##                               xx$caption <- NULL
##                               yy$caption <- NULL
##                               xx$caption.level <- NULL
##                               yy$caption.level <- NULL
                              
##                               xxx <- capture.output(xx$show.rest())
##                               xxx <- xxx[-length(xxx)]
##                               yyy <- capture.output(yy$show.rest())
##                               yyy <- yyy[-1]
                              
##                               xy <- c(xxx, yyy)
                              
##                               cat(header.rest(caption = .self$caption, caption.level = .self$caption.level), sep = "\n")
##                               cat(xy, sep = "\n")
##                               warning("show.rest() currently produces a non valid markup", call. = FALSE)
##                             },

##                             show.org <- function(.) {
##                               test.ncol(.self$x, .self$y)
                              
##                               xx <- .self$x
##                               yy <- .self$y
                              
##                               xx$caption <- NULL
##                               yy$caption <- NULL
##                               xx$caption.level <- NULL
##                               yy$caption.level <- NULL
                              
##                               xxx <- capture.output(xx$show.org())
##                               yyy <- capture.output(yy$show.org())
                              
##                               xy <- c(xxx, yyy)
                              
##                               cat(header.org(caption = .self$caption, caption.level = .self$caption.level), sep = "\n")
##                               cat(xy, sep = "\n")
##                               warning("show.org() currently produces a non valid markup", call. = FALSE)
##                             },

##                             show.t2t <- function(.) {
##                               test.ncol(.self$x, .self$y)
                              
##                               xx <- .self$x
##                               yy <- .self$y
                              
##                               xx$caption <- NULL
##                               yy$caption <- NULL
##                               xx$caption.level <- NULL
##                               yy$caption.level <- NULL
                              
##                               xxx <- capture.output(xx$show.t2t())
##                               yyy <- capture.output(yy$show.t2t())
                              
##                               xy <- c(xxx, yyy)
                              
##                               cat(header.t2t(caption = .self$caption, caption.level = .self$caption.level), sep = "\n")
##                               cat(xy, sep = "\n")
##                             },

##                             show.textile <- function(.) {
##                               test.ncol(.self$x, .self$y)
                              
##                               xx <- .self$x
##                               yy <- .self$y
                              
##                               xx$caption <- NULL
##                               yy$caption <- NULL
##                               xx$frame <- NULL
##                               yy$frame <- NULL
##                               xx$width <- 0
##                               yy$width <- 0
                              
##                               xxx <- capture.output(xx$show.textile())
##                               yyy <- capture.output(yy$show.textile())
                              
##                               xy <- c(xxx, yyy)
                              
##                               cat(header.textile(caption = .self$caption, caption.level = .self$caption.level, frame = .self$frame, width = .self$width))
##                               cat(xy, sep = "\n")
##                             }
##                             )
##                           )

## ##' Rbind two ascii objects
## ##'
## ##' This function binds rows of two ascii table.
## ##' @title Rbind two ascii objects
## ##' @param x an ascii table
## ##' @param y another ascii table
## ##' @param caption see \code{?ascii}
## ##' @param caption.level see \code{?ascii}
## ##' @param frame see \code{?ascii}
## ##' @param grid see \code{?ascii}
## ##' @param col.width see \code{?ascii}
## ##' @param width see \code{?ascii}
## ##' @return An ascii object.
## ##' @export
## ##' @author David Hajage
## rbind.ascii <- function(x, y, caption = NULL, caption.level = NULL, frame = NULL, grid = NULL, col.width = 1, width = 0) {
##   results <- asciiRbind$new(x, y, caption = caption, caption.level = caption.level, frame = frame, grid = grid, col.width = col.width, width = width)
##   results
## }
