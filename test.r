include.rownames = FALSE
include.colnames = TRUE
format = "f"
digits = 2
decimal.mark = "."
na.print = ""
    caption           <- ""
    width             <- 0
    frame             <- ""
    grid              <- ""
    valign            <- ""
    header            <- T
    footer            <- F
    align             <- ""
    col.width         <- 1
    style             <- ""
source("load.r")

x <- data.frame(a = 1:10, bbbbbb = rnorm(10), c = factor(LETTERS[1:10]), d = LETTERS[11:20])
x$d <- as.character(x$d)

x[1,1] <- NA
x[3,3] <- NA

ascii(x)
ascii(x, format = "e", na.print = "Donnée manquante")


ascii(1:4)
ascii(c(1:3, NA), na.print = "toto")
ascii(letters[1:4])
ascii(c(letters[1:3], NA))
ascii(1:4, include.rownames = T)
ascii(1:4)

x <- as.data.frame(t(1:4))

library(survival)
x = summary(aml)
