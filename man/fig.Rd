\name{fig}
\alias{fig}
\alias{graph}
\title{Insert figure}
\usage{
fig(file = NULL, graph = NULL, format = NULL, ...)
}
\arguments{
  \item{file}{character string (}

  \item{graph}{a recordedplot, a lattice plot, a ggplot, or
  an expression producing a plot (optional if the file
  already exists)}

  \item{format}{jpg, png or pdf (or guessed with the file
  name)}

  \item{...}{additional arguments (passed to format
  options)}
}
\value{
A fig object
}
\description{
\code{graph} can be used with \code{export} function to insert an R graphic.
}
\author{
David Hajage
}

