#' check tokens
#' @param tokens the tokens
#' @param expected as expected
#' @export
check.tokens <- function(tokens, expected)
{
    nt <- length(tokens)
    ne <- length(expected)
    if (nt != ne) stop("wrong number of words on line; got", nt, "but need", ne)
    for (i in 1:nt) {
        if (tokens[i] != expected[i]) stop("expecting word", expected[i], "but got", tokens[i])
    }
}

