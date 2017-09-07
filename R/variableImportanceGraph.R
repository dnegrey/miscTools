#' @title Produce a variable importance graph
#' @description \code{variableImportanceGraph} produces a variable importance graph
#' @param x character vector
#' @param y numeric vector
#' @param sumYcheck logical value; check that \code{sum(y) == 1}?
#' @param barColor character string; fill color for bars (valid color)
#' @param lineColor character string; line color (valid color)
#' @param lineWidth non-negative integer value indicating line width (use 0 to omit)
#' @return A horizontal \code{\link{barGraph}}, where \code{x} contains the names 
#' of the variables in a model and \code{y} contains the model contribution (decimals).
#' @examples 
#' x <- c(
#'     "Days Since Last Transaction",
#'     "Average Order Value",
#'     "Signed Up Online",
#'     "Number of Transactions",
#'     "Tenure",
#'     "Multi-Category Flag"
#' )
#' y <- c(.18, .4, .04, .25, .12, .01)
#' variableImportanceGraph(x, y)
#' @seealso \code{\link{barGraph}, \link{plot_ly}}
#' @export 
variableImportanceGraph <- function(x, y,
                                    sumYcheck = TRUE,
                                    barColor = "rgba(0, 55, 82, 0.6)",
                                    lineColor = "#003752",
                                    lineWidth = 2) {
    # validate x
    if (!(
        is.character(x) 
        && all(!is.na(x))
    )) {stop("x must be a character vector with no missing values")}
    # validate y
    if (!(
        is.numeric(y) 
        && length(y) == length(x) 
        && all(!is.na(y)) 
        && all(y > 0) 
        && all(y < 1)
    )) {stop("y must be a numeric vector of real numbers in (0, 1), with the same length as x")}
    # validate sumYcheck
    if (!(
        is.logical(sumYcheck) 
        && length(sumYcheck) == 1
    )) {stop("sumYcheck must be a length one logical vector")}
    if (sumYcheck) {
        if (sum(y) != 1) {
            stop("sum(y) != 1")
        }
    }
    # assign other parameters
    mainTitle <- ""
    yLabel <- "<b>Model Contribution</b>"
    yType <- "pct"
    yDigits <- 1
    orderBy <- "y"
    orderDesc <- TRUE
    horizontal <- TRUE
    z <- barGraph(
        x = x,
        y = y,
        mainTitle = mainTitle,
        yLabel = yLabel,
        yType = yType,
        yDigits = yDigits,
        barColor = barColor,
        lineColor = lineColor,
        lineWidth = lineWidth,
        orderBy = orderBy,
        orderDesc = orderDesc,
        horizontal = horizontal
    )
    # return object
    class(z) <- c(class(z), "mt_variableImportanceGraph")
    return(z)
}