#' @title Generate a univariate summary table
#' @description \code{univariateSummary()} produces a univariate summary table, 
#' evaluating \code{x} as the independent variable and \code{y} as the dependent 
#' variable. It uses the S3 generic function \code{\link{bin}} to bin the values of 
#' \code{x}, then per bin, applies \code{FUN} to \code{y}. The vector \code{x} can include 
#' missing values but \code{y} cannot. When \code{ytype = 1}, \code{y} must 
#' contain only unique (coercable) values of 0 or 1. When \code{ytype = 2}, 
#' \code{y} must have at least 3 distinct values.
#' @param x character, factor, logical or numeric vector (independent variable)
#' @param y logical, integer or numeric vector (dependent variable)
#' @param ytype integer value of 1 (binary y) or 2 (continuous y)
#' @param FUN function to be applied to \code{y}
#' @param ... further arguments passed to \code{\link{bin}}
#' @return A data frame with class "\code{mt_univariateSummary}" containing the 
#' following columns:
#' \itemize{
#'   \item \code{xbin}: binned values of x (character)
#'   \item \code{Freq}: frequency of observations (integer)
#'   \item \code{Percent}: relative frequency of observations (numeric)
#'   \item \code{yagg}: aggregated summary statistic of y (numeric)
#' }
#' @examples 
#' # character x, continuous y
#' z <- univariateSummary(iris$Species, iris$Sepal.Length, ytype = 2)
#' print(z)
#' class(z)
#' 
#' # numeric x, logical y, additional arguments to bin()
#' z <- univariateSummary(mtcars$hp, mtcars$mpg > 15, numBins = 2)
#' print(z)
#' class(z)
#' @seealso \code{\link{bin}}
#' @importFrom dplyr %>% group_by summarise n mutate arrange select
#' @export
univariateSummary <- function(x,
                              y = rep(0, length(x)),
                              ytype = 1,
                              FUN = mean,
                              ...) {
    # validate x
    if (!(
        (inherits(x, c("character", "factor", "logical")) | is.numeric(x))
        && length(x) == length(y)
    ))
    # validate y
    if (!(
        inherits(y, c("logical", "integer", "numeric"))
        && all(!is.na(y))
    )) {
        stop("y must be logical, integer or numeric with no missing values")
    }
    # validate ytype
    if (!(
        is.numeric(ytype)
        && length(ytype) == 1
        && ytype %in% 1:2
    )) {
        stop("ytype must be either 1 (binary y) or 2 (continuous y)")
    }
    # validate y given ytype
    if (ytype == 1) {
        if (!(
            all(unique(as.numeric(y)) %in% 0:1)
        )) {
            stop("y must have only unique (coercable) values 0 or 1")
        }
    } else {
        if (!(
            length(unique(y)) >= 3
        )) {
            stop("y must have at least 3 unique values")
        }
    }
    # validate FUN
    if (!is.function(FUN)) {stop("FUN must be a function")}
    # binning method
    xbin <- bin(x, ...)
    # coerce y to numeric
    y <- as.numeric(y)
    # combine x, xbin and y
    z <- data.frame(
        x = x,
        xbin = xbin,
        y = y,
        stringsAsFactors = FALSE
    )
    # aggregate
    z <- yagg(x, z, FUN)
    class(z) <- c("mt_univariateSummary", class(z))
    return(z)
}
yagg <- function(x, z, FUN) {
    UseMethod("yagg")
}
yagg.character <- function(x, z, FUN) {
    nx <- nrow(z)
    z %>% 
        group_by(xbin) %>% 
        summarise(Freq = n(),
                  yagg = FUN(y)) %>% 
        data.frame() %>% 
        mutate(Percent = Freq/nx) %>% 
        arrange(xbin) %>% 
        select(xbin, Freq, Percent, yagg)
}
yagg.factor <- function(x, z, FUN) {
    yagg.character(x, z, FUN)
}
yagg.logical <- function(x, z, FUN) {
    yagg.character(x, z, FUN)
}
yagg.numeric <- function(x, z, FUN) {
    nx <- nrow(z)
    z %>% 
        group_by(xbin) %>% 
        summarise(Freq = n(),
                  MinX = min(x),
                  yagg = FUN(y)) %>% 
        data.frame() %>% 
        mutate(Percent = Freq/nx) %>% 
        arrange(MinX) %>% 
        select(xbin, Freq, Percent, yagg)
}
