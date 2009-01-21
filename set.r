# set caption
setGeneric("caption<-", function(object, value){standardGeneric("caption<-")})
setReplaceMethod(
  f = "caption", 
  signature = "R2asciidocDataFrame", 
  definition = function(object, value) {
    object@caption <- value
    return(object)
  }
)

setReplaceMethod(
  f = "caption", 
  signature = "R2asciidocVector",
  definition = function(object, value) {
    object@caption <- value
    return(object)
  }
)

setReplaceMethod(
  f = "caption", 
  signature = "R2asciidocList",
  definition = function(object, value) {
    object@caption <- value
    return(object)
  }
)
