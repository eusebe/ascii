require(proto)

asciiTable <- proto(expr = {
  new <- function(.,
                  x,
                  include.rownames,
                  include.colnames,
                  rownames,
                  colnames,
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
                  tgroup,
                  n.tgroup,
                  talign,
                  tvalign,
                  tstyle,
                  bgroup,
                  n.bgroup,
                  balign,
                  bvalign,
                  bstyle,
                  lgroup,
                  n.lgroup,
                  lalign,
                  lvalign,
                  lstyle,
                  rgroup,
                  n.rgroup,
                  ralign,
                  rvalign,
                  rstyle) proto(.,
                                x = x,
                                include.rownames = include.rownames,
                                include.colnames = include.colnames,
                                rownames = rownames,
                                colnames = colnames,
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
                                tgroup = tgroup,
                                n.tgroup = n.tgroup,
                                talign = talign,
                                tvalign = tvalign,
                                tstyle = tstyle, 
                                bgroup = bgroup,
                                n.bgroup = n.bgroup,
                                balign = balign,
                                bvalign = bvalign,
                                bstyle = bstyle, 
                                lgroup = lgroup,
                                n.lgroup = n.lgroup,
                                lalign = lalign,
                                lvalign = lvalign,
                                lstyle = lstyle,
                                rgroup = rgroup,
                                n.rgroup = n.rgroup,
                                ralign = ralign,
                                rvalign = rvalign,
                                rstyle = rstyle
                                )
  
  show.asciidoc <- function(.) {
    show.asciidoc.table(x = .$x, include.rownames = .$include.rownames, include.colnames = .$include.colnames, rownames = .$rownames, colnames = .$colnames, format = .$format, digits = .$digits, decimal.mark = .$decimal.mark, na.print = .$na.print, caption = .$caption, caption.level = .$caption.level, width = .$width, frame = .$frame, grid = .$grid, valign = .$valign, header = .$header, footer = .$footer, align = .$align, col.width = .$col.width, style = .$style, lgroup = .$lgroup, n.lgroup = .$n.lgroup, lalign = .$lalign, lvalign = .$lvalign, lstyle = .$lstyle, rgroup = .$rgroup, n.rgroup = .$n.rgroup, ralign = .$ralign, rvalign = .$rvalign, rstyle = .$rstyle, tgroup = .$tgroup, n.tgroup = .$n.tgroup, talign = .$talign, tvalign = .$tvalign, tstyle = .$tstyle, bgroup = .$bgroup, n.bgroup = .$n.bgroup, balign = .$balign, bvalign = .$bvalign, bstyle = .$bstyle)
  }

  show.rest <- function(.) {
    show.rest.table(x = .$x, include.rownames = .$include.rownames, include.colnames = .$include.colnames, rownames = .$rownames, colnames = .$colnames, format = .$format, digits = .$digits, decimal.mark = .$decimal.mark, na.print = .$na.print, caption = .$caption, caption.level = .$caption.level, width = .$width, frame = .$frame, grid = .$grid, valign = .$valign, header = .$header, footer = .$footer, align = .$align, col.width = .$col.width, style = .$style, lgroup = .$lgroup, n.lgroup = .$n.lgroup, lalign = .$lalign, lvalign = .$lvalign, lstyle = .$lstyle, rgroup = .$rgroup, n.rgroup = .$n.rgroup, ralign = .$ralign, rvalign = .$rvalign, rstyle = .$rstyle, tgroup = .$tgroup, n.tgroup = .$n.tgroup, talign = .$talign, tvalign = .$tvalign, tstyle = .$tstyle, bgroup = .$bgroup, n.bgroup = .$n.bgroup, balign = .$balign, bvalign = .$bvalign, bstyle = .$bstyle)
  }

  show.org <- function(.) {
    show.org.table(x = .$x, include.rownames = .$include.rownames, include.colnames = .$include.colnames, rownames = .$rownames, colnames = .$colnames, format = .$format, digits = .$digits, decimal.mark = .$decimal.mark, na.print = .$na.print, caption = .$caption, caption.level = .$caption.level, width = .$width, frame = .$frame, grid = .$grid, valign = .$valign, header = .$header, footer = .$footer, align = .$align, col.width = .$col.width, style = .$style, lgroup = .$lgroup, n.lgroup = .$n.lgroup, lalign = .$lalign, lvalign = .$lvalign, lstyle = .$lstyle, rgroup = .$rgroup, n.rgroup = .$n.rgroup, ralign = .$ralign, rvalign = .$rvalign, rstyle = .$rstyle, tgroup = .$tgroup, n.tgroup = .$n.tgroup, talign = .$talign, tvalign = .$tvalign, tstyle = .$tstyle, bgroup = .$bgroup, n.bgroup = .$n.bgroup, balign = .$balign, bvalign = .$bvalign, bstyle = .$bstyle)
  }
  
  show.t2t <- function(.) {
    show.t2t.table(x = .$x, include.rownames = .$include.rownames, include.colnames = .$include.colnames, rownames = .$rownames, colnames = .$colnames, format = .$format, digits = .$digits, decimal.mark = .$decimal.mark, na.print = .$na.print, caption = .$caption, caption.level = .$caption.level, width = .$width, frame = .$frame, grid = .$grid, valign = .$valign, header = .$header, footer = .$footer, align = .$align, col.width = .$col.width, style = .$style, lgroup = .$lgroup, n.lgroup = .$n.lgroup, lalign = .$lalign, lvalign = .$lvalign, lstyle = .$lstyle, rgroup = .$rgroup, n.rgroup = .$n.rgroup, ralign = .$ralign, rvalign = .$rvalign, rstyle = .$rstyle, tgroup = .$tgroup, n.tgroup = .$n.tgroup, talign = .$talign, tvalign = .$tvalign, tstyle = .$tstyle, bgroup = .$bgroup, n.bgroup = .$n.bgroup, balign = .$balign, bvalign = .$bvalign, bstyle = .$bstyle)
  }

  show.textile <- function(.) {
    show.textile.table(x = .$x, include.rownames = .$include.rownames, include.colnames = .$include.colnames, rownames = .$rownames, colnames = .$colnames, format = .$format, digits = .$digits, decimal.mark = .$decimal.mark, na.print = .$na.print, caption = .$caption, caption.level = .$caption.level, width = .$width, frame = .$frame, grid = .$grid, valign = .$valign, header = .$header, footer = .$footer, align = .$align, col.width = .$col.width, style = .$style, lgroup = .$lgroup, n.lgroup = .$n.lgroup, lalign = .$lalign, lvalign = .$lvalign, lstyle = .$lstyle, rgroup = .$rgroup, n.rgroup = .$n.rgroup, ralign = .$ralign, rvalign = .$rvalign, rstyle = .$rstyle, tgroup = .$tgroup, n.tgroup = .$n.tgroup, talign = .$talign, tvalign = .$tvalign, tstyle = .$tstyle, bgroup = .$bgroup, n.bgroup = .$n.bgroup, balign = .$balign, bvalign = .$bvalign, bstyle = .$bstyle)
  }

  show.pandoc <- function(.) {
    show.pandoc.table(x = .$x, include.rownames = .$include.rownames, include.colnames = .$include.colnames, rownames = .$rownames, colnames = .$colnames, format = .$format, digits = .$digits, decimal.mark = .$decimal.mark, na.print = .$na.print, caption = .$caption, caption.level = .$caption.level, width = .$width, frame = .$frame, grid = .$grid, valign = .$valign, header = .$header, footer = .$footer, align = .$align, col.width = .$col.width, style = .$style, lgroup = .$lgroup, n.lgroup = .$n.lgroup, lalign = .$lalign, lvalign = .$lvalign, lstyle = .$lstyle, rgroup = .$rgroup, n.rgroup = .$n.rgroup, ralign = .$ralign, rvalign = .$rvalign, rstyle = .$rstyle, tgroup = .$tgroup, n.tgroup = .$n.tgroup, talign = .$talign, tvalign = .$tvalign, tstyle = .$tstyle, bgroup = .$bgroup, n.bgroup = .$n.bgroup, balign = .$balign, bvalign = .$bvalign, bstyle = .$bstyle)
  }
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
    show.asciidoc.list(x = .$x, caption = .$caption, caption.level = .$caption.level, list.type = .$list.type)
  }

  show.rest <- function(.) {
    show.rest.list(x = .$x, caption = .$caption, caption.level = .$caption.level, list.type = .$list.type)
  }

  show.org <- function(.) {
    show.org.list(x = .$x, caption = .$caption, caption.level = .$caption.level, list.type = .$list.type)
  }

  show.t2t <- function(.) {
    show.t2t.list(x = .$x, caption = .$caption, caption.level = .$caption.level, list.type = .$list.type)
  }

  show.textile <- function(.) {
    show.textile.list(x = .$x, caption = .$caption, caption.level = .$caption.level, list.type = .$list.type)
  }

  show.pandoc <- function(.) {
    show.pandoc.list(x = .$x, caption = .$caption, caption.level = .$caption.level, list.type = .$list.type)
  }
})

asciiMixed <- proto(expr = {
  new <- function(., ...) {
    args <- list(...)
    noms <- as.character(as.list(substitute(list(...)))[-1])
    if (is.null(noms))
      noms <- paste("obj", 1:length(args), sep = "")
    else
      noms <- paste(paste("obj", 1:length(args), sep = ""), noms, sep = ": ")
    names(args) <- noms 
    as.proto(args)
  }

  show.asciidoc <- function(.) {
    args <- as.list(.)
    args <- args[sort(names(args))]
    for (i in seq_along(args)) {
      if (is.null(args[[i]])) next
      print(args[[i]], type = "asciidoc") 
      if (i != length(args)) cat("\n") 
    }
  }
  
  show.rest <- function(.) {
    args <- rev(as.list(.))
    args <- args[sort(names(args))]
    for (i in seq_along(args)) {
      if (is.null(args[[i]])) next
      print(args[[i]], type = "rest") 
      if (i != length(args)) cat("\n") 
    }
  }

  show.org <- function(.) {
    args <- rev(as.list(.))
    args <- args[sort(names(args))]
    for (i in seq_along(args)) {
      if (is.null(args[[i]])) next
      print(args[[i]], type = "org") 
      if (i != length(args)) cat("\n") 
    }
  }

  show.t2t <- function(.) {
    args <- rev(as.list(.))
    args <- args[sort(names(args))]
    for (i in seq_along(args)) {
      if (is.null(args[[i]])) next
      print(args[[i]], type = "t2t")
      if (i != length(args)) cat("\n") 
    }
  }

  show.textile <- function(.) {
    args <- rev(as.list(.))
    args <- args[sort(names(args))]
    for (i in seq_along(args)) {
      if (is.null(args[[i]])) next
      print(args[[i]], type = "textile")
      if (i != length(args)) cat("\n") 
    }
  }

  show.pandoc <- function(.) {
    args <- rev(as.list(.))
    args <- args[sort(names(args))]
    for (i in seq_along(args)) {
      if (is.null(args[[i]])) next
      print(args[[i]], type = "pandoc")
      if (i != length(args)) cat("\n") 
    }
  }
})
