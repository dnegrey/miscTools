#' @title Produce a quantile bar graph based on gains chart(s)
#' @description \code{gainsChartBarGraph} produces a bar graph based on a 
#' mt_\code{\link{gainsChart}} object. When two such objects are supplied, the 
#' function produces a combined bar graph, where the first object (\code{xb}) is 
#' labeled as "Build" and the second object (\code{xv}) is labeled as "Validate".
#' @param xb \code{mt_gainsChart} object
#' @param xv \code{mt_gainsChart} object
#' @param cb character string; fill color for \code{xb} bars (valid color)
#' @param cv character string; fill color for \code{xv} bars (valid color)
#' @param xLabel character string; x-axis label
#' @param yLabel character string; y-axis label
#' @param yType character string; y-axis format type; valid 
#' values are \code{"int"}, \code{"dlr"} and \code{"pct"}
#' @param yDigits non-negative integer value indicating the number of decimal 
#' places to show when hovering over the bars
#' @return A \code{\link{plot_ly}} visualization of a 
#' mt_\code{\link{gainsChart}} object, showing the average Y value (\code{avgY} 
#' for "binary"/"continuous"; \code{perFreqYC} for "combined") by quantile (
#' \code{yhatBin}).
#' @examples 
#' # pull in sample scored data frame
#' x <- modelSampleScored
#' # filter to validation subset
#' x <- split(x, x$ValidateFlag)
#' # gains chart binary
#' y <- lapply(x, function(z){gainsChart(z$TargetFlag, z$pTargetFlag)})
#' # bar graph
#' gainsChartBarGraph(y[[1]], y[[2]])
#' # gains chart continuous
#' y <- lapply(x, function(z){
#'     gainsChart(z[z$TargetFlag, "TargetValue"], z[z$TargetFlag, "pTargetValue"])
#' })
#' # bar graph
#' gainsChartBarGraph(y[[1]], y[[2]])
#' # gains chart combined
#' y <- lapply(x, function(z){
#'     gainsChart(z$TargetFlag, z$pTargetFlag*z$pTargetValue, z$TargetValue)
#' })
#' # bar graph
#' gainsChartBarGraph(y[[1]], y[[2]])
#' @seealso \code{\link{gainsChart}, \link{plot_ly}}
#' @importFrom plotly plot_ly %>% add_trace layout
#' @export 
gainsChartBarGraph <- function(xb, xv = NULL,
                               cb = "#009DDC",
                               cv = "#77BF30",
                               xLabel = "Model Quantile",
                               yLabel = "Mean Outcome",
                               yType = ifelse("binary" %in% class(xb), "pct", "dlr"),
                               yDigits = ifelse("binary" %in% class(xb), 1, 0)) {
    # validate cb
    if (!(
        is.character(cb)
        && length(cb) == 1
    )) {stop("cb must be a length one character vector")}
    # validate cv
    if (!(
        is.character(cv)
        && length(cv) == 1
    )) {stop("cv must be a length one character vector")}
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
    # validate xb and xv
    if (!("mt_gainsChart" %in% class(xb))) {
        stop("xb must be a mt_gainsChart object")
    }
    xbc <- class(xb)
    cat(paste0("xb has class: ", '"', paste(xbc, collapse = '" "'), '"'), sep = "\n")
    yt <- xbc[xbc %in% c("binary", "continuous", "combined")][1]
    if (length(yt) != 1) {
        stop("xb class must include one of the following: binary, continuous or combined")
    } 
    cat(sprintf("xb is based on the following outcome type: %s", yt), sep = "\n")
    yn <- "avgY"
    if (yt == "combined") {yn <- "perFreqYC"}
    y <- data.frame(
        qbin = xb$yhatBin,
        build = xb[, yn],
        stringsAsFactors = FALSE
    )
    valid <- FALSE
    if (!is.null(xv)) {
        cat("xv is not null, validating xv ...", sep = "\n")
        if (!identical(class(xb), class(xv))) {
            stop("xv does not have the same class as xb")
        }
        if (!identical(xb$yhatBin, xv$yhatBin)) {
            stop("xv does not have the same yhatBin vector as xb")
        }
        cat("xv has been validated", sep = "\n")
        y$validate <- xv[, yn]
        valid <- TRUE
    }
    # remove total row
    y <- y[toupper(y$qbin) != "TOTAL", ]
    # qbin as factor
    y$qbin <- factor(y$qbin, levels = y$qbin)
    # make adjustments as needed
    mp <- 1
    ya <- list(
        side = "left",
        title = paste0("<b>", yLabel, "</b>"),
        showgrid = TRUE,
        rangemode = "tozero"
    )
    if (yType == "dlr") {
        ya$tickprefix = "$"
    } else if (yType == "pct") {
        mp <- 100
        ya$ticksuffix = "%"
    }
    y$build <- y$build*mp
    if (valid) {
        y$validate <- y$validate*mp
    }
    # graph
    z <- plot_ly(y) %>% 
        add_trace(x = ~qbin,
                  y = ~build,
                  type = "bar",
                  name = "Build",
                  marker = list(color = cb),
                  hoverinfo = "text",
                  text = ~cleanNumberFormat(build/mp, yType, yDigits))
    if (valid) {
        z <- z %>% 
            add_trace(x = ~qbin,
                      y = ~validate,
                      type = "bar",
                      name = "Validate",
                      marker = list(color = cv),
                      hoverinfo = "text",
                      text = ~cleanNumberFormat(validate/mp, yType, yDigits))
    }
    z <- z %>% 
        layout(xaxis = list(title = paste0("<b>", xLabel, "</b>")),
               yaxis = ya)
    # return object
    class(z) <- c(class(z), "mt_gainsChartBarGraph", yt)
    return(z)
}
