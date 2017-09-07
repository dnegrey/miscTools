#' @title Determine if an object is a string
#' @description \code{isString()} determines whether an object is a string. A 
#' string is defined as a length one character vector.
#' @param ... one or more objects to evaluate
#' @return A logical vector indicating whether the input objects are strings.
#' @examples
#' isString("abc", 123, c("def", "ghi"))
#' @export
isString <- function(...) {
    # store input in list
    x <- list(...)
    # character class
    cc <- unlist(lapply(x, class)) == "character"
    # length one
    l1 <- unlist(lapply(x, length)) == 1
    # return logical
    return(cc & l1)
}
