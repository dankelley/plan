#' Base Class for plan Objects
#' @slot data A list containing variable contents.
#' @family classes provided by plan
#setClass("plan", representation(data="list"), prototype=list(data=list()))
setClass("plan", slots=c(data="list"))
setMethod("initialize", "plan",
    function(.Object) {
        .Object@data <- list()
        .Object
    })

#' Extract Something From a plan Object
#'
#' @description Extract something from a plan object, avoiding using the "slot" notation.
#'
#' @param x A [plan-class] object.
#' @param i The item to extract.
#' @param j Optional additional information on the `i` item.
#' @param ... Optional additional information (ignored).
setMethod(f="[[",
          signature(x="plan", i="ANY", j="ANY"),
          definition=function(x, i, j, ...) {
              if (length(i) != 1L)
                  stop("length of 'i' must be 1")
              if (i == "data") {
                  return(x@data)
              } else {
                  return(x@data[[i]])
              }
          })

#' @title Replace Parts of a plan Object
#'
#' @description Replace something within a plan object, avoiding using the "slot" notation.
#'
#' @param x A [plan-class] object.
#' @param i The item to replace.
#' @param j Optional additional information on the `i` item.
#' @param ... Optional additional information (ignored).
#' @param value The value to be placed into `x`, somewhere.
setMethod(f="[[<-",
          signature(x="plan", i="ANY", j="ANY"),
          function(x, i, j, ..., value) { # FIXME: use j for e.g. times
              ## message("in base [[<-")
              ## message("value: ", paste(value, collapse=" "))
              ## metadata must match exactly but data can be partially matched
              if (length(i) != 1L)
                  stop("length of 'i' must be 1")
              if (i %in% names(x@data)) {
                  x@data[[i]] <- value
                  return(x)
              } else {
                  warning("there is no item named \"", i, "\" in this ", class(x), " object", call.=FALSE)
              }
          })


