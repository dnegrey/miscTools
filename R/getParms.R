#' @title Get a list of parameters called in a function
#' @description \code{getParms()} produces a named list of character values
#' containing the parameters supplied to a function
#' @param which if negative then the number of frames to go back; otherwise the
#' frame number
#' @return A list of length 1 character vectors containing the parameters that
#' where supplied to a function. The names of the list are the arguments to
#' the function.
#' @examples
#' foo <- function(x, y){getParms()}
#' foo(iris, 1983)
#' @seealso \code{\link{match.call}, \link{sys.parent}}
#' @export
getParms <- function(which = -1) {
    x <- as.list(match.call(definition = sys.function(which),
                            call = sys.call(which)))
    x <- x[names(x) != ""]
    nx <- names(x)
    x <- as.list(as.character(x))
    names(x) <- nx
    return(x)
}
