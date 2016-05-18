#' Base Class for plan Objects
#' @slot data A list containing variable contents.
#' @family classes provided by \code{plan}
setClass("plan", representation(data="list"), prototype=list(data=list()))


#' Extract Something From a plan Object
#'
#' @param x A \code{plan} object, i.e. one inheriting from \code{\link{plan-class}}.
#' @param i The item to extract.
#' @param j Optional additional information on the \code{i} item.
#' @param ... Optional additional information (ignored).
setMethod(f="[[",
          signature(x="plan", i="ANY", j="ANY"),
          definition=function(x, i, j, ...) {
              if (i == "data") {
                  return(x@data)
              } else {
                  return(x@data[[i]])
              }
          })


