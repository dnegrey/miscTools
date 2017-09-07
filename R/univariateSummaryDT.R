#' @title Produce a univariate summary datatable
#' @description \code{univariateSummaryDT()} produces a nicely formatted 
#' \code{DT::datatable} representation of a \code{mt_univariateSummary} object.
#' @param data \code{mt_univariateSummary} data frame
#' @param yLabel character string; \code{y} variable label
#' @param yType character string; \code{y} variable format type; valid 
#' values are \code{"int"}, \code{"dlr"} and \code{"pct"}
#' @param yDigits non-negative integer value indicating the number of decimal 
#' places to show for values of the \code{y} variable
#' @return A \code{datatable} representation of a \code{mt_univariateSummary} object.
#' @examples 
#' x <- univariateSummary(rnorm(5000), round(runif(5000)))
#' univariateSummaryDT(x, "Random Binary Rate", "pct", 1)
#' @seealso \code{\link{univariateSummary}}, \code{\link{datatable}}
#' @importFrom DT datatable
#' @export
univariateSummaryDT <- function(data,
                                yLabel,
                                yType,
                                yDigits) {
    UseMethod("univariateSummaryDT")
}
#' @export
univariateSummaryDT.mt_univariateSummary <- function(data,
                                                     yLabel,
                                                     yType,
                                                     yDigits) {
    # validate yLabel
    if (!(
        is.character(yLabel)
        && length(yLabel) == 1
    )) {stop("yLabel must be a length one character vector")}
    # validate yType
    if (!(
        is.character(yType)
        && length(yType) == 1
        && yType %in% c("int", "dlr", "pct")
    )) {stop("yType must be 'int', 'dlr' or 'pct'")}
    # validate yDigits
    if (!(
        is.numeric(yDigits)
        && length(yDigits) == 1
        && yDigits == as.integer(yDigits)
        && yDigits >= 0
    )) {stop("yDigits must be a non-negative integer")}
    # make adjustments as needed
    tmpdata <- split(data, is.na(data$xbin))
    x <- rbind(tmpdata$`TRUE`, tmpdata$`FALSE`)
    x$Freq <- cleanNumberFormat(x$Freq, "int", 0)
    x$Percent <- cleanNumberFormat(x$Percent, "pct", 1)
    x$yagg <- cleanNumberFormat(x$yagg, yType, yDigits)
    names(x) <- c(
        "Bin",
        "Frequency",
        "Percentage",
        yLabel
    )
    z <- datatable(
        data = x,
        escape = FALSE,
        extensions = c("Buttons"),
        options = list(
            dom = 'Bt',
            buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
            pageLength = nrow(x),
            ordering = FALSE,
            columnDefs = list(
                list(class = 'dt-center', targets = 0:3)
            )
        ),
        rownames = FALSE
    )
    # return object
    class(z) <- c(class(z), "mt_univariateSummaryDT")
    return(z)
}
