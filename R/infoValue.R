#' @title Extract the information value from a weight of evidence table
#' @description \code{infoValue} returns the total information value from a 
#' \code{miscTools} weight of evidence table (data frame with the "mt_woe" 
#' class, i.e. the result of running \code{miscTools::woe()})
#' @param x mt_woe data frame
#' @return A numeric value with class "\code{mt_infoValue}" representing the 
#' total information value from the input weight of evidence table.
#' @examples 
#' x <- woe(mtcars$hp, mtcars$mpg > 20)
#' print(x)
#' class(x)
#' y <- infoValue(x)
#' print(y)
#' class(y)
#' @seealso \code{\link{woe}}
#' @export
infoValue <- function(x) {
    UseMethod("infoValue")
}
#' @export
infoValue.mt_woe <- function(x) {
    y <- x[x$Level == "*** TOTAL ***", "InformationValue"]
    class(y) <- c("mt_infoValue", class(y))
    return(y)
}
