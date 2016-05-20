check.tokens <- function(tokens, expected)
{
    nt <- length(tokens)
    ne <- length(expected)
    if (nt != ne) stop("wrong number of words on line; got", nt, "but need", ne)
    for (i in 1:nt) {
        if (tokens[i] != expected[i]) stop("expecting word", expected[i], "but got", tokens[i])
    }
}





#' Trim leading/trailing whitespace from character strings
#' 
#' Trim leading and trailing whitespace from character strings.  Used by
#' \code{\link{read.gantt}} and \code{\link{read.burndown}}.
#' 
#' 
#' @param x a character string, or vector of character strings.
#' @return As \code{x}, but with leading and trailing space removed
#' @author Dan Kelley
#' @keywords misc
#' @examples
#' 
#' library(plan)
#' x <- c("  hellow there", "ba bye   ", " buddy   ")
#' print(x)
#' print(trim.whitespace(x))
#' 
trim.whitespace <- function(x)
{
    x <- gsub("^[ \t]*", "", x)
    x <- gsub("[ \t]*$", "", x)
}

