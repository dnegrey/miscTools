#' @title Produce a bar graph
#' @description \code{barGraph} produces a simple bar graph
#' @param x character, factor, numeric, logical or Date vector
#' @param y numeric vector
#' @param mainTitle character string; graph title
#' @param xLabel character string; x-axis label
#' @param yLabel character string; y-axis label
#' @param yType character string; y-axis format type; valid 
#' values are \code{"int"}, \code{"dlr"} and \code{"pct"}
#' @param yDigits non-negative integer value indicating the number of decimal 
#' places to show when hovering over the bars
#' @param barColor character string; fill color for bars (valid color)
#' @param lineColor character string; line color (valid color)
#' @param lineWidth non-negative integer value indicating line width (use 0 to omit)
#' @param orderBy a value of \code{NULL} (do not order), \code{"x"} (order by x) or 
#' \code{"y"} (order by y)
#' @param orderDesc logical value; when ordering by \code{x} or \code{y}, order descending?
#' @param horizontal logical value; create a horizontal bar graph?
#' @return A \code{\link{plot_ly}} bar graph using \code{x} for the bar labels 
#' and \code{y} for the heights of the bars
#' @examples 
#' barGraph(c("giraffes", "orangutans", "monkeys"), c(20, 14, 23))
#' @seealso \code{\link{plot_ly}}
#' @importFrom dplyr arrange desc select
#' @importFrom plotly plot_ly %>% add_trace layout
#' @export 
barGraph <- function(x, y,
                     mainTitle = "",
                     xLabel = "",
                     yLabel = "",
                     yType = "int",
                     yDigits = 0,
                     barColor = "#009DDC",
                     lineColor = barColor,
                     lineWidth = 0,
                     orderBy = NULL,
                     orderDesc = FALSE,
                     horizontal = FALSE) {
    # validate x
    if (!(
        is.character(x) 
        || is.factor(x) 
        || is.numeric(x) 
        || is.logical(x) 
        || class(x) == "Date"
    )) {stop("x must be a character, factor, numeric, logical or Date vector")}
    # validate y
    if (!(
        is.numeric(y) 
        && length(y) == length(x)
    )) {stop("y must be a numeric vector with the same length as x")}
    # validate mainTitle
    if (!(
        is.character(mainTitle)
        && length(mainTitle) == 1
    )) {stop("mainTitle must be a length one character vector")}
    # validate xLabel
    if (!(
        is.character(xLabel)
        && length(xLabel) == 1
    )) {stop("xLabel must be a length one character vector")}
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
    # validate barColor
    if (!(
        is.character(barColor)
        && length(barColor) == 1
    )) {stop("barColor must be a length one character vector")}
    # validate lineColor
    if (!(
        is.character(lineColor)
        && length(lineColor) == 1
    )) {stop("lineColor must be a length one character vector")}
    # validate lineWidth
    if (!(
        is.numeric(lineWidth)
        && length(lineWidth) == 1
        && lineWidth == as.integer(lineWidth)
        && lineWidth >= 0
    )) {stop("lineWidth must be a non-negative integer")}
    # validate orderBy
    if (!(
        is.null(orderBy) 
        || identical(orderBy, "x") 
        || identical(orderBy, "y")
    )) {stop("orderBy must be null or 'x' or 'y'")}
    # validate orderDesc
    if (!(
        is.logical(orderDesc)
        && length(orderDesc) == 1
    )) {stop("orderDesc must be a length one logical vector")}
    # validate horizontal
    if (!(
        is.logical(horizontal)
        && length(horizontal) == 1
    )) {stop("horizontal must be a length one logical vector")}
    # combine x and y
    z <- data.frame(
        x = x,
        y = y,
        stringsAsFactors = FALSE
    )
    # force x to character
    z$x <- as.character(z$x)
    # order if necessary
    if (!is.null(orderBy)) {
        z$zt <- z[, orderBy]
        if (orderDesc) {
            if (horizontal) {
                z <- arrange(z, zt)
            } else {
                z <- arrange(z, desc(zt))
            }
        } else {
            if (horizontal) {
                z <- arrange(z, desc(zt))
            } else {
                z <- arrange(z, zt)
            }
        }
        z <- select(z, -zt)
    }
    # x as factor for plotly
    z$x <- factor(z$x, levels = z$x)
    # axis definitions
    xa <- list(
        title = xLabel
    )
    mp <- 1
    ya <- list(
        title = yLabel,
        showgrid = TRUE,
        rangemode = "tozero"
    )
    if (yType == "dlr") {
        ya$tickprefix = "$"
    } else if (yType == "pct") {
        mp <- 100
        ya$ticksuffix = "%"
    }
    # adjust for orientation
    orientation <- "v"
    z$zt <- z[, "y"]
    z$y <- z$y*mp
    ml <- list()
    if (horizontal) {
        names(z) <- c("y", "x", "zt")
        orientation <- "h"
        temp <- list(xa = xa,
                     ya = ya)
        xa <- temp$ya
        ya <- temp$xa
        ya$title <- ""
        ml$l <- ceiling(max(nchar(as.character(z$y)))*250/32)
    }
    # produce plot
    p <- plot_ly(z) %>% 
        add_trace(x = ~x,
                  y = ~y,
                  type = "bar",
                  marker = list(color = barColor,
                                line = list(color = lineColor,
                                            width = lineWidth)),
                  hoverinfo = "text",
                  orientation = orientation,
                  text = ~cleanNumberFormat(zt, yType, yDigits)) %>% 
        layout(title = mainTitle,
               xaxis = xa,
               yaxis = ya,
               margin = ml)
    # return object
    return(p)
}