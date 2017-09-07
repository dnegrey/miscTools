#' @title Produce a relative histogram
#' @description \code{relativeHistogram()} produces a relative histogram based on 
#' the contents of the input data frame, which must be a mt_\code{\link{univariateSummary}} 
#' object.
#' @param data \code{mt_univariateSummary} data frame
#' @param xLabel character string; \code{x} variable label
#' @param barColor character string; fill color for bars (valid color)
#' @return A \code{\link{plot_ly}} visualization of a 
#' mt_\code{\link{univariateSummary}} object, showing the distribution of a 
#' binned variable \code{x}.
#' @examples 
#' # numeric x with missings
#' x <- c(rnorm(5000, 100, 10), rep(as.numeric(NA), 500))
#' z <- univariateSummary(x, numBins = 20)
#' relativeHistogram(z, "Random Normal")
#' 
#' # logical x
#' x <- round(runif(20))
#' z <- univariateSummary(x)
#' relativeHistogram(z, "Random Binary", "orange")
#' 
#' # character x
#' relativeHistogram(univariateSummary(iris$Species), "Species", "#77BF30")
#' @seealso \code{\link{univariateSummary}}, \code{\link{plot_ly}}
#' @importFrom plotly plot_ly %>% add_trace layout
#' @export
relativeHistogram <- function(data,
                              xLabel,
                              barColor = "#BDDFF7") {
    UseMethod("relativeHistogram")
}
#' @export
relativeHistogram.mt_univariateSummary <- function(data,
                                                   xLabel,
                                                   barColor = "#BDDFF7") {
    # validate xLabel
    if (!(
        is.character(xLabel)
        && length(xLabel) == 1
    )) {stop("xLabel must be a length one character vector")}
    # validate barColor
    if (!(
        is.character(barColor)
        && length(barColor) == 1
    )) {stop("barColor must be a length one character vector (valid color)")}
    # make adjustments as needed
    tmpdata <- split(data, is.na(data$xbin))
    data <- rbind(tmpdata$`TRUE`, tmpdata$`FALSE`)
    data$xbin <- ifelse(is.na(data$xbin), "NA", data$xbin)
    data$xbin <- factor(data$xbin, levels = data$xbin)
    data$Percent <- data$Percent*100
    # create plot
    z <- plot_ly(data) %>% 
        add_trace(x = ~xbin,
                  y = ~Percent,
                  type = "bar",
                  name = "Relative Frequency",
                  marker = list(color = barColor),
                  hoverinfo = "text",
                  text = ~cleanNumberFormat(Percent/100, "pct", 1)) %>% 
        layout(xaxis = list(title = paste0("<b>", xLabel, "</b>")),
               yaxis = list(
                   side = "left",
                   title = "<b>Relative Frequency</b>",
                   ticksuffix = "%",
                   showgrid = TRUE,
                   rangemode = "tozero"
               ))
    # return object
    class(z) <- c(class(z), "mt_relativeHistogram")
    return(z)
}
