# set caption
setGeneric("asciiCaption<-", function(object, value){standardGeneric("asciiCaption<-")})
setReplaceMethod(
  f = "asciiCaption", 
  signature = "R2asciidocDataFrame", 
  definition = function(object, value) {
    object@caption <- value
    return(object)
  }
)

setReplaceMethod(
  f = "asciiCaption", 
  signature = "R2asciidocVector",
  definition = function(object, value) {
    object@caption <- value
    return(object)
  }
)

setReplaceMethod(
  f = "asciiCaption", 
  signature = "R2asciidocList",
  definition = function(object, value) {
    object@caption <- value
    return(object)
  }
)

# Is this usefull ?
# It would be nice to have same system as ggplot2 : ascii(res) + ascii_caption("A title")
# Then, I should define a class asciiCaption, and define a method for +
