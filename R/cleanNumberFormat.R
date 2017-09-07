#' @title Convert a numeric vector to character with pretty formatting
#' @description \code{cleanNumberFormat} applies commonly used numeric 
#' formatting conventions to numeric vectors and returns them as character 
#' vectors
#' @param x numeric vector to format
#' @param type string describing the format type; valid values are 
#' \code{"int"}, \code{"dlr"} and \code{"pct"}
#' @param digits non-negative integer value indicating the number of decimal 
#' places to be used
#' @return A character vector of formatted numbers.
#' @examples
#' x <- rnorm(5)
#' print(x)
#' cleanNumberFormat(x, "pct", 1)
#' cleanNumberFormat(x*10000, "int")
#' cleanNumberFormat(x*10000, "dlr", digits = 2)
#' @seealso \code{\link{round}, \link{format}, \link{trimws}}
#' @export
cleanNumberFormat <- function(x, type, digits = 0) {
    # check parameters
    if (!is.numeric(x)) {
        stop("non-numeric argument supplied for x")
    } else if (!isString(type)) {
        stop("non-string argument supplied for type")
    } else if (!(
        is.numeric(digits) & 
        length(digits) == 1 & 
        all(digits >= 0) & 
        all(as.integer(digits) == digits)
    )) {
        stop("non-integer argument supplied for digits")
    }
    # proceed
    if (type %in% c("int", "dlr")) {
        y <- format(round(x, digits), nsmall = digits, big.mark = ",")
    } else if (type == "pct") {
        y <- format(100*round(x, digits + 2), nsmall = digits, big.mark = ",")
    } else {
        y <- x
    }
    y <- trimws(y)
    if (type == "dlr") {
        y <- paste0("$", y)
    } else if (type == "pct") {
        y <- paste0(y, "%")
    }
    # return
    return(y)
}
