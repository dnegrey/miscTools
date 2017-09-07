#' @title Calculate a coefficient of determination (R-squared)
#' @description \code{nrsq()} calculates the coefficient of determination, or 
#' R-squared value, between an independent variable \code{x} and a dependent 
#' variable \code{y}. Note that when \code{na.rm = FALSE} (default), missing 
#' values of \code{x} will be replaced with \code{1} and a flag identifying the 
#' missings will be included as an additive term in the model. Otherwise, only 
#' non-missing records are fit.
#' @param x numeric vector (independent variable)
#' @param y numeric vector (dependent variable)
#' @param na.rm logical value indicating whether missing values of \code{x} 
#' (and their corresponding \code{y} values) should be removed
#' @return A numeric value with class "\code{mt_nrsq}" that indicates the 
#' proportion of the variance in the dependent variable that is predictable 
#' from the independent variable.
#' @examples 
#' # basic example without missing values
#' nrsq(mtcars$hp, mtcars$mpg)
#' 
#' # add some missing values
#' x <- ifelse(runif(length(mtcars$hp)) < 0.30, NA, mtcars$hp)
#' # include missings with adjustment
#' nrsq(x, mtcars$mpg, na.rm = FALSE)
#' # exclude missings entirely
#' nrsq(x, mtcars$mpg, na.rm = TRUE)
#' 
#' # evaluate a whole data frame of predictors
#' temp <- lapply(mtcars[c("disp", "hp", "wt", "qsec")], nrsq, y = mtcars$mpg)
#' data.frame(
#'     VarName = names(temp),
#'     RSquared = as.numeric(unlist(temp)),
#'     stringsAsFactors = FALSE
#' )
#' @seealso \code{\link{lm}}, \code{\link{summary.lm}}
#' @importFrom stats lm summary.lm
#' @export
nrsq <- function(x, y, na.rm = FALSE) {
    # validate x and y
    if (!(
        is.numeric(x)
        && is.numeric(y)
        && length(y) == length(x)
    )) {
        stop("x and y must be numeric vectors with the same length")
    } else if (any(is.na(y))) {
        stop("y is the dependent variable, and may not have missing values")
    }
    # validate na.rm
    if (!(
        is.logical(na.rm)
        && length(na.rm) == 1
    )) {
        stop("na.rm must be a single logical value")
    }
    # flag missing values of x
    miss <- is.na(x)
    # remove NA if specified, otherwise replace
    if (na.rm) {
        x <- x[!miss]
        y <- y[!miss]
        z <- lm(y ~ x)
    } else {
        x <- ifelse(miss, 1, x)
        z <- lm(y ~ x + miss)
    }
    # fetch R-squared
    z <- summary(z)$r.squared
    # return R-squared
    class(z) <- c("mt_nrsq", class(z))
    return(z)
}