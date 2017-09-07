#' @title Evaluate the performance of a model
#' @description \code{modelPerformance} is a wrapper function that runs various 
#' model evaluation techniques and returns the results in a convenient list object. Parameters 
#' prefaced with a function dot (i.e. \code{gainsChartDT.}) are only applicable to the use 
#' of that function.
#' @param y logical, integer or numeric vector (dependent variable)
#' @param yhat numeric vector (predicted values of \code{y})
#' @param y2 logical, integer or numeric vector (other dependent variable for combined models)
#' @param v logical, integer or numeric vector of binary values (distinguishes validate (TRUE) vs. build (FALSE) observations)
#' @param numBins integer value >= 2; number of desired bins
#' @param gainsChartDT.scoreFormatType character string; format type for score field(s); valid values 
#' are "int", "dlr" and "pct"
#' @param gainsChartDT.scoreFormatDigits non-negative integer value; number of decimal places 
#' for score field(s)
#' @param gainsChartDT.contSumFormatType character string; format type for continuous sum field(s); valid values 
#' are "int", "dlr" and "pct"
#' @param gainsChartDT.contSumFormatDigits non-negative integer value; number of decimal places 
#' for continuous sum field(s)
#' @param gainsChartDT.contAvgFormatType character string; format type for continuous average (mean) field(s); valid values 
#' are "int", "dlr" and "pct"
#' @param gainsChartDT.contAvgFormatDigits non-negative integer value; number of decimal places 
#' for continuous average (mean) field(s)
#' @param gainsChartDT.KScolor character string; text color for max KS value (valid color)
#' @param gainsChartBarGraph.cb character string; fill color for build bars (valid color)
#' @param gainsChartBarGraph.cv character string; fill color for validate bars (valid color)
#' @param gainsChartBarGraph.xLabel character string; x-axis label
#' @param gainsChartBarGraph.yLabel character string; y-axis label
#' @param gainsChartBarGraph.yType character string; y-axis format type; valid 
#' values are \code{"int"}, \code{"dlr"} and \code{"pct"}
#' @param gainsChartBarGraph.yDigits non-negative integer value indicating the number of decimal 
#' places to show when hovering over the bars
#' @param gainsChartPerfCurve.cb character string; line color for build (valid color)
#' @param gainsChartPerfCurve.cv character string; line color for validate (valid color)
#' @param gainsChartPerfCurve.cr character string; random/reference line color (valid color)
#' @param gainsChartPerfCurve.xLabel character string; x-axis label
#' @param gainsChartPerfCurve.yLabel character string; y-axis label
#' @param variableImportanceGraph.x character vector
#' @param variableImportanceGraph.y numeric vector
#' @param variableImportanceGraph.sumYcheck logical value; check that \code{sum(y) == 1}?
#' @param variableImportanceGraph.barColor character string; fill color for bars (valid color)
#' @param variableImportanceGraph.lineColor character string; line color (valid color)
#' @param variableImportanceGraph.lineWidth non-negative integer value indicating line width (use 0 to omit)
#' @return A named list with class \code{mt_modelPerformance} containing the following 
#' objects. Note that when \code{v} is supplied, the data will be split into build 
#' and validate sets where each set will be evaluated. When \code{v} is not supplied, 
#' the data is assumed to just be the "build" set.
#' \itemize{
#'   \item \code{build}: a list containing the following objects:
#'   \itemize{
#'     \item \code{gc}: \code{gainsChart()} object based on build set
#'     \item \code{gcDT}: \code{gainsChartDT()} object based on build set
#'     \item \code{gcBG}: \code{gainsChartBarGraph()} object based on build set
#'     \item \code{gcPC}: \code{gainsChartPerfCurve()} object based on build set
#'   }
#'   \item \code{validate}: (only returned when \code{v} is supplied) same type of list as \code{build} but based on validate set
#'   \item \code{both}: (only returned when \code{v} is supplied) a list containing the following objects:
#'   \itemize{
#'     \item \code{gcBG}: \code{gainsChartBarGraph()} object based on build and validate sets
#'     \item \code{gcPC}: \code{gainsChartPerfCurve()} object based on build and validate sets
#'   }
#'   \item \code{varImp}: (only returned when \code{variableImportanceGraph.*} parms are supplied) \code{variableImportanceGraph()} object
#' }
#' @examples
#' # pull in sample scored data frame
#' x <- modelSampleScored
#' head(x)
#' # binary target
#' z <- modelPerformance(
#'     y = x$TargetFlag,
#'     yhat = x$pTargetFlag,
#'     v = x$ValidateFlag
#' )
#' class(z)
#' names(z)
#' names(z$build)
#' names(z$validate)
#' names(z$both)
#' z$validate$gcDT
#' z$both$gcBG
#' z$both$gcPC
#' # continuous target
#' z <- modelPerformance(
#'     y = x[x$TargetFlag, ]$TargetValue,
#'     yhat = x[x$TargetFlag, ]$pTargetValue,
#'     v = x[x$TargetFlag, ]$ValidateFlag,
#'     gainsChartBarGraph.yType = "dlr",
#'     gainsChartBarGraph.yDigits = 0
#' )
#' class(z)
#' names(z)
#' names(z$build)
#' names(z$validate)
#' names(z$both)
#' z$validate$gcDT
#' z$both$gcBG
#' z$both$gcPC
#' # combined target (with arbitrary variable importance parms)
#' z <- modelPerformance(
#'     y = x$TargetFlag,
#'     yhat = x$pTargetFlag*x$pTargetValue,
#'     y2 = x$TargetValue,
#'     v = x$ValidateFlag,
#'     variableImportanceGraph.x = c(
#'         "Days Since Last Transaction",
#'         "Average Order Value",
#'         "Signed Up Online",
#'         "Number of Transactions",
#'         "Tenure",
#'         "Multi-Category Flag"
#'     ),
#'     variableImportanceGraph.y = c(.18, .4, .04, .25, .12, .01)
#' )
#' class(z)
#' names(z)
#' names(z$build)
#' names(z$validate)
#' names(z$both)
#' z$validate$gcDT
#' z$both$gcBG
#' z$both$gcPC
#' z$varImp
#' @seealso \code{\link{gainsChart}, \link{gainsChartDT}, \link{gainsChartBarGraph}, 
#' \link{gainsChartPerfCurve}, \link{variableImportanceGraph}}
#' @export 
modelPerformance <- function(y, yhat, y2 = NULL, v = NULL, numBins = 10,
                             gainsChartDT.scoreFormatType = "int",
                             gainsChartDT.scoreFormatDigits = 5,
                             gainsChartDT.contSumFormatType = "dlr",
                             gainsChartDT.contSumFormatDigits = 0,
                             gainsChartDT.contAvgFormatType = "dlr",
                             gainsChartDT.contAvgFormatDigits = 0,
                             gainsChartDT.KScolor = "#FF3005",
                             gainsChartBarGraph.cb = "#009DDC",
                             gainsChartBarGraph.cv = "#77BF30",
                             gainsChartBarGraph.xLabel = "Model Quantile",
                             gainsChartBarGraph.yLabel = "Mean Outcome",
                             gainsChartBarGraph.yType = ifelse(is.null(y2), "pct", "dlr"),
                             gainsChartBarGraph.yDigits = ifelse(is.null(y2), 1, 0),
                             gainsChartPerfCurve.cb = "#009DDC",
                             gainsChartPerfCurve.cv = "#77BF30",
                             gainsChartPerfCurve.cr = "#666666",
                             gainsChartPerfCurve.xLabel = "Cumulative Total Percent",
                             gainsChartPerfCurve.yLabel = "Cumulative Outcome Percent",
                             variableImportanceGraph.x = NULL,
                             variableImportanceGraph.y = NULL,
                             variableImportanceGraph.sumYcheck = TRUE,
                             variableImportanceGraph.barColor = "rgba(0, 55, 82, 0.6)",
                             variableImportanceGraph.lineColor = "#003752",
                             variableImportanceGraph.lineWidth = 2) {
    # leverage parm validation of gainsChart
    gcTest <- try(gainsChart(y, yhat, y2, numBins), silent = TRUE)
    if (inherits(gcTest, "try-error")) {
        stop("at least one invalid parameter found in: y, yhat, y2, numBins")
    }
    # combine vectors into a data frame for ease
    x <- data.frame(y = y, yhat = yhat, v = "build", stringsAsFactors = FALSE)
    if (!is.null(y2)) {
        x$y2 <- y2
    }
    # determine if validation is on
    if (!is.null(v)) {
        cat("v is not null; validating v ...", sep = "\n")
        if (!(
            any(class(v) %in% c("logical", "integer", "numeric"))
            && sum(is.na(v)) == 0
            && length(v) == length(y) 
            && all(0:1 %in% unique(as.numeric(v)))
        )) {
            stop("v must be a logical, integer or numeric vector the same length as y, with coercable values of 0 and 1")
        }
        cat("v has been successfully validated, turning on validation", sep = "\n")
        x$v <- ifelse(as.logical(v), "validate", "build")
    }
    x <- split(x, x$v)
    # create output list
    z <- list()
    for (i in 1:length(x)) {
        w <- x[[i]]
        z[[i]] <- list()
        names(z)[i] <- names(x)[i]
        z[[i]]$gc <- gainsChart(w$y, w$yhat, w$y2, numBins)
        z[[i]]$gcDT <- gainsChartDT(
            z[[i]]$gc,
            scoreFormatType = gainsChartDT.scoreFormatType,
            scoreFormatDigits = gainsChartDT.scoreFormatDigits,
            contSumFormatType = gainsChartDT.contSumFormatType,
            contSumFormatDigits = gainsChartDT.contSumFormatDigits,
            contAvgFormatType = gainsChartDT.contAvgFormatType,
            contAvgFormatDigits = gainsChartDT.contAvgFormatDigits,
            KScolor = gainsChartDT.KScolor
        )
        z[[i]]$gcBG <- gainsChartBarGraph(
            z[[i]]$gc,
            cb = ifelse(names(z)[i] == "build", gainsChartBarGraph.cb, gainsChartBarGraph.cv),
            xLabel = gainsChartBarGraph.xLabel,
            yLabel = gainsChartBarGraph.yLabel,
            yType = gainsChartBarGraph.yType,
            yDigits = gainsChartBarGraph.yDigits
        )
        z[[i]]$gcPC <- gainsChartPerfCurve(
            z[[i]]$gc,
            cb = ifelse(names(z)[i] == "build", gainsChartPerfCurve.cb, gainsChartPerfCurve.cv),
            cr = gainsChartPerfCurve.cr,
            xLabel = gainsChartPerfCurve.xLabel,
            yLabel = gainsChartPerfCurve.yLabel
        )
    }
    vc <- NULL
    if (length(x) == 2) {
        z$both <- list()
        z$both$gcBG <- gainsChartBarGraph(
            z$build$gc,
            z$validate$gc,
            cb = gainsChartBarGraph.cb,
            cv = gainsChartBarGraph.cv,
            xLabel = gainsChartBarGraph.xLabel,
            yLabel = gainsChartBarGraph.yLabel,
            yType = gainsChartBarGraph.yType,
            yDigits = gainsChartBarGraph.yDigits
        )
        z$both$gcPC <- gainsChartPerfCurve(
            z$build$gc,
            z$validate$gc,
            cb = gainsChartPerfCurve.cb,
            cv = gainsChartPerfCurve.cv,
            cr = gainsChartPerfCurve.cr,
            xLabel = gainsChartPerfCurve.xLabel,
            yLabel = gainsChartPerfCurve.yLabel
        )
        vc <- "HasValidate"
    }
    vic <- NULL
    if (!is.null(variableImportanceGraph.x) | !is.null(variableImportanceGraph.y)) {
        z$varImp <- variableImportanceGraph(
            x = variableImportanceGraph.x,
            y = variableImportanceGraph.y,
            sumYcheck = variableImportanceGraph.sumYcheck,
            barColor = variableImportanceGraph.barColor,
            lineColor = variableImportanceGraph.lineColor,
            lineWidth = variableImportanceGraph.lineWidth
        )
        vic <- "HasVariableImportance"
    }
    # assign new class
    nc <- c("mt_modelPerformance", class(z$build$gc)[2], vc, vic)
    class(z) <- c(nc, class(z))
    # return object
    return(z)
}
