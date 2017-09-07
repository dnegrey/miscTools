#' @title Produce a univariate graph
#' @description \code{univariateGraph()} produces a univariate graph based on 
#' the contents of the input data frame, which must be a mt_\code{\link{univariateSummary}} 
#' object.
#' @param data \code{mt_univariateSummary} data frame
#' @param xLabel character string; \code{x} variable label
#' @param yLabel character string; \code{y} variable label
#' @param yType character string; \code{y} variable format type; valid 
#' values are \code{"int"}, \code{"dlr"} and \code{"pct"}
#' @param yDigits non-negative integer value indicating the number of decimal 
#' places to show for values of the \code{y} variable
#' @param yRangeMode character string; \code{"tozero"} (y-axis starts at 0) or 
#' \code{"auto"} (y-axis extremes determined by data)
#' @param barColor character string; fill color for bars (valid color)
#' @param lineColor character string; line color (valid color)
#' @return A \code{\link{plot_ly}} visualization of a 
#' mt_\code{\link{univariateSummary}} object, showing the distribution of a 
#' binned variable \code{x} overlayed with an aggregated summary statistic of a 
#' variable \code{y}.
#' @examples 
#' # numeric x logical y
#' x <- c(rnorm(5000, 100, 10), rep(as.numeric(NA), 500))
#' y <- runif(5500)
#' z <- univariateSummary(x, y < 0.50)
#' univariateGraph(z, "Random Normal", "Random Uniform", "pct", 1)
#' 
#' # numeric x continuous y
#' z <- univariateSummary(x, y*100, 2, numBins = 15)
#' univariateGraph(z, "Random Normal", "Random Uniform", "dlr", 0)
#' 
#' # character x continuous y
#' z <- univariateSummary(iris$Species, iris$Sepal.Length, 2)
#' univariateGraph(z, "Species", "Sepal Length", "int", 2)
#' @seealso \code{\link{univariateSummary}}, \code{\link{plot_ly}}
#' @importFrom plotly plot_ly %>% add_trace layout
#' @export
univariateGraph <- function(data, 
                            xLabel,
                            yLabel,
                            yType,
                            yDigits,
                            yRangeMode = "tozero",
                            barColor = "#BDDFF7",
                            lineColor = "#000000") {
    UseMethod("univariateGraph")
}
#' @export
univariateGraph.mt_univariateSummary <- function(data, 
                                                 xLabel,
                                                 yLabel,
                                                 yType,
                                                 yDigits,
                                                 yRangeMode = "tozero",
                                                 barColor = "#BDDFF7",
                                                 lineColor = "#000000") {
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
    # validate yRangeMode
    if (!(
        is.character(yRangeMode)
        && length(yRangeMode) == 1
        && yRangeMode %in% c("tozero", "auto")
    )) {stop("yRangeMode must be 'tozero' or 'auto'")}
    # validate barColor
    if (!(
        is.character(barColor)
        && length(barColor) == 1
    )) {stop("barColor must be a length one character vector (valid color)")}
    # validate lineColor
    if (!(
        is.character(lineColor)
        && length(lineColor) == 1
    )) {stop("lineColor must be a length one character vector (valid color)")}
    # make adjustments as needed
    tmpdata <- split(data, is.na(data$xbin))
    data <- rbind(tmpdata$`TRUE`, tmpdata$`FALSE`)
    data$xbin <- ifelse(is.na(data$xbin), "NA", data$xbin)
    data$xbin <- factor(data$xbin, levels = data$xbin)
    data$Percent <- data$Percent*100
    if (yRangeMode == "auto") {yRangeMode == "normal"}
    mp <- 1
    ya <- list(
        side = "right",
        overlaying = "y",
        title = paste0("<b>", yLabel, "</b>"),
        showgrid = FALSE,
        rangemode = yRangeMode
    )
    if (yType == "dlr") {
        ya$tickprefix = "$"
    } else if (yType == "pct") {
        mp <- 100
        ya$ticksuffix = "%"
    }
    data$yagg <- data$yagg*mp
    # create plot
    z <- plot_ly(data) %>% 
        add_trace(x = ~xbin,
                  y = ~Percent,
                  type = "bar",
                  name = "Relative Frequency",
                  marker = list(color = barColor),
                  hoverinfo = "text",
                  text = ~cleanNumberFormat(Percent/100, "pct", 1)) %>% 
        add_trace(x = ~xbin,
                  y = ~yagg,
                  type = "scatter",
                  mode = "lines",
                  name = yLabel,
                  yaxis = "y2",
                  line = list(color = lineColor),
                  hoverinfo = "text",
                  text = ~cleanNumberFormat(yagg/mp, yType, yDigits)) %>% 
        layout(xaxis = list(title = paste0("<b>", xLabel, "</b>")),
               yaxis = list(
                   side = "left",
                   title = "<b>Relative Frequency</b>",
                   ticksuffix = "%",
                   showgrid = FALSE,
                   rangemode = "tozero"
               ),
               yaxis2 = ya,
               legend = list(xanchor = "center", yanchor = "top",
                             x = 0.10, y = 1.15,
                             orientation = "h"),
               margin = list(r = 100))
    # return object
    class(z) <- c(class(z), "mt_univariateGraph")
    return(z)
}
