#' @title Produce a cumulative performance graph based on gains chart(s)
#' @description \code{gainsChartPerfCurve} produces a cumulative performance graph based on a 
#' mt_\code{\link{gainsChart}} object. When two such objects are supplied, the 
#' function produces a combined graph, where the first object (\code{xb}) is 
#' labeled as "Build" and the second object (\code{xv}) is labeled as "Validate".
#' @param xb \code{mt_gainsChart} object
#' @param xv \code{mt_gainsChart} object
#' @param cb character string; line color for \code{xb} (valid color)
#' @param cv character string; line color for \code{xv} (valid color)
#' @param cr character string; random/reference line color (valid color)
#' @param xLabel character string; x-axis label
#' @param yLabel character string; y-axis label
#' @return A \code{\link{plot_ly}} visualization of a 
#' mt_\code{\link{gainsChart}} object, showing the cumulative percent of total Y 
#' (\code{cumPctY} for "binary"/"continuous"; \code{cumPctYC} for "combined") by quantile (
#' \code{yhatBin}) percent.
#' @examples 
#' # pull in sample scored data frame
#' x <- modelSampleScored
#' # filter to validation subset
#' x <- split(x, x$ValidateFlag)
#' # gains chart binary
#' y <- lapply(x, function(z){gainsChart(z$TargetFlag, z$pTargetFlag)})
#' # cumulative performance graph
#' gainsChartPerfCurve(y[[1]], y[[2]])
#' # gains chart continuous
#' y <- lapply(x, function(z){
#'     gainsChart(z[z$TargetFlag, "TargetValue"], z[z$TargetFlag, "pTargetValue"])
#' })
#' # cumulative performance graph
#' gainsChartPerfCurve(y[[1]], y[[2]])
#' # gains chart combined
#' y <- lapply(x, function(z){
#'     gainsChart(z$TargetFlag, z$pTargetFlag*z$pTargetValue, z$TargetValue)
#' })
#' # cumulative performance graph
#' gainsChartPerfCurve(y[[1]], y[[2]])
#' @seealso \code{\link{gainsChart}, \link{plot_ly}}
#' @importFrom plotly plot_ly %>% add_trace layout
#' @export 
gainsChartPerfCurve <- function(xb, xv = NULL,
                                cb = "#009DDC",
                                cv = "#77BF30",
                                cr = "#666666",
                                xLabel = "Cumulative Total Percent",
                                yLabel = "Cumulative Outcome Percent") {
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
    # validate cr
    if (!(
        is.character(cr)
        && length(cr) == 1
    )) {stop("cr must be a length one character vector")}
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
    yn <- "cumPctY"
    if (yt == "combined") {yn <- "cumPctYC"}
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
    # add 0 row
    if (valid) {
        y <- rbind(
            data.frame(qbin = "0", build = 0, validate = 0,
                       stringsAsFactors = FALSE),
            y
        )
    } else {
        y <- rbind(
            data.frame(qbin = "0", build = 0,
                       stringsAsFactors = FALSE),
            y
        )
    }
    # add random
    y$random <- 100*as.integer(y$qbin)/(nrow(y) - 1)
    # qbin as factor
    y$qbin <- cleanNumberFormat(as.integer(y$qbin)/(nrow(y) - 1), "pct", 0)
    y$qbin <- factor(y$qbin, levels = y$qbin)
    # make adjustments as needed
    y$build <- y$build*100
    if (valid) {
        y$validate <- y$validate*100
    }
    ya <- list(
        side = "left",
        title = paste0("<b>", yLabel, "</b>"),
        showgrid = TRUE,
        range = c(-1, 101),
        ticksuffix = "%",
        tick0 = 0,
        dtick = 10
    )
    xa <- list(
        title = paste0("<b>", xLabel, "</b>")
    )
    # graph
    z <- plot_ly(y) %>% 
        add_trace(x = ~qbin,
                  y = ~build,
                  type = "scatter",
                  mode = "lines+markers",
                  name = ifelse(valid, "Build", "Model"),
                  line = list(color = cb),
                  marker = list(color = cb),
                  hoverinfo = "text",
                  text = ~cleanNumberFormat(build/100, "pct", 1))
    if (valid) {
        z <- z %>% 
            add_trace(x = ~qbin,
                      y = ~validate,
                      type = "scatter",
                      mode = "lines+markers",
                      name = "Validate",
                      line = list(color = cv),
                      marker = list(color = cv),
                      hoverinfo = "text",
                      text = ~cleanNumberFormat(validate/100, "pct", 1))
    }
    z <- z %>% 
        add_trace(x = ~qbin,
                  y = ~random,
                  type = "scatter",
                  mode = "lines+markers",
                  name = "Random",
                  line = list(color = cr),
                  marker = list(color = cr),
                  hoverinfo = "text",
                  text = ~cleanNumberFormat(random/100, "pct", 1)) %>% 
        layout(xaxis = xa,
               yaxis = ya)
    # return object
    class(z) <- c(class(z), "mt_gainsChartPerfCurve", yt)
    return(z)
}
