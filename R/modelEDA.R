#' @title Exploratory data analysis on a modeling dataset
#' @description \code{modelEDA} performs standard exploratory data analysis on a 
#' modeling dataset, including variable clustering, weight of evidence (binary y), 
#' numeric R-squared (continuous y) and univariate summaries/graphs. Parameters 
#' prefaced with a function dot (i.e. \code{bin.}) are only applicable to the use 
#' of that function.
#' @param x data frame; a modeling dataset
#' @param yname character string; dependent variable column name
#' @param ytype integer value of 1 (binary y) or 2 (continuous y)
#' @param bin.numBins integer value >= 2; number of desired bins
#' @param bin.equalBinSize logical value; return equally sized (TRUE) or equally spaced (FALSE) bins
#' @param bin.minPct integer between 0 and 100 specifying a percentile to force as the max endpoint for the low (first) bin
#' @param bin.maxPct integer between 0 and 100 specifying a percentile to force as the min endpoint for the high (last) bin (must be > minPct)
#' @param nrsq.na.rm logical value indicating whether missing values of x (and their corresponding y values) should be removed
#' @param variable_cluster.n integer value >= 2; number of desired clusters
#' @param variable_cluster.na.rm logical value; should records with missing values be removed?
#' @param univariateSummary.FUN function to be applied to y
#' @param univariateGraph.yLabel character string; y variable label
#' @param univariateGraph.yType character string; y variable format type; valid values are "int", "dlr" and "pct"
#' @param univariateGraph.yDigits non-negative integer value indicating the number of decimal places to show for values of the y variable
#' @param univariateGraph.yRangeMode character string; "tozero" (y-axis starts at 0) or "auto" (y-axis extremes determined by data)
#' @param univariateGraph.barColor character string; fill color for bars (valid color)
#' @param univariateGraph.lineColor character string; line color (valid color)
#' @return A named list with class \code{mt_modelEDA} containing the following 
#' objects:
#' \itemize{
#'   \item \code{dataSummaryDT}: \code{datatable()} object summarizing data
#'   \item \code{y.relativeHistogram}: \code{relativeHistogram()} object for \code{y}
#'   \item \code{variable_cluster}: data frame with \code{variable_cluster()} results
#'   \item \code{woe} (binary \code{y}): named list of \code{woe()} tables
#'   \item \code{infoValue} (binary \code{y}): named list of \code{infoValue()} values
#'   \item \code{woeDT} (binary \code{y}): named list of \code{woeDT()} objects
#'   \item \code{nrsq} (continuous \code{y}): named list of \code{nrsq()} values
#'   \item \code{variable_cluster_plus}: data frame with \code{variable_cluster_plus()} results
#'   \item \code{clusterDT}: named list of \code{clusterDT()} objects
#'   \item \code{univariateSummary}: named list of \code{univariateSummary()} tables
#'   \item \code{univariateSummaryDT}: named list of \code{univariateSummaryDT()} objects
#'   \item \code{univariateGraph}: named list of \code{univariateGraph()} objects
#' }
#' @examples
#' # binary y 
#' x <- modelEDA(mtcars, "vs", 1)
#' names(x)
#' x$woeDT
#' 
#' # continuous y
#' x <- modelEDA(mtcars, "mpg", 2)
#' names(x)
#' x$clusterDT
#' @seealso \code{\link{woe}, \link{nrsq}, \link{variable_cluster}, \link{univariateGraph}}
#' @importFrom DT datatable
#' @importFrom dplyr filter
#' @importFrom digest digest
#' @export
modelEDA <- function(x, yname, ytype,
                     bin.numBins = 10,
                     bin.equalBinSize = FALSE,
                     bin.minPct = 0,
                     bin.maxPct = 100,
                     nrsq.na.rm = FALSE,
                     variable_cluster.n = max(floor((length(x) - 1)/10), 2),
                     variable_cluster.na.rm = TRUE,
                     univariateSummary.FUN = mean,
                     univariateGraph.yLabel = yname,
                     univariateGraph.yType = ifelse(ytype == 1, "pct", "dlr"),
                     univariateGraph.yDigits = ifelse(ytype == 1, 1, 0),
                     univariateGraph.yRangeMode = "tozero",
                     univariateGraph.barColor = "#BDDFF7",
                     univariateGraph.lineColor = "#000000") {
    # validate x
    if (!(
        class(x) == "data.frame"
    )) {stop("x must be a data frame")}
    # validate yname
    if (!(
        is.character(yname)
        && length(yname) == 1
        && yname %in% names(x)
    )) {stop("yname must be the name of a variable in x")}
    # validate ytype
    if (!(
        is.numeric(ytype)
        && length(ytype) == 1
        && ytype %in% 1:2
    )) {stop("ytype must be either 1 (binary y) or 2 (continuous y)")}
    yt <- switch(ytype, "1" = "binary", "2" = "continuous")
    # separate y from x
    y <- x[, yname]
    # temp copy of unaltered y for relative histogram
    tyrh <- y
    class(y) <- c(yt, class(y))
    check_ytype(y)
    x <- x[names(x) != yname]
    # create result object
    z <- list()
    # data summary
    ds <- data.frame(
        Measure = c(
            "Dependent Variable Type",
            "Dependent Variable Name",
            "Dependent Variable Mean",
            "Number of Observations",
            "Number of Independent Variables"
        ),
        Value = c(
            yt,
            yname,
            cleanNumberFormat(mean(y), univariateGraph.yType, univariateGraph.yDigits),
            cleanNumberFormat(nrow(x), "int", 0),
            cleanNumberFormat(length(x), "int", 0)
        ),
        stringsAsFactors = FALSE
    )
    ds$Value <- paste0("<b>", ds$Value, "</b>")
    z$dataSummaryDT <- datatable(
        data = ds,
        escape = FALSE,
        extensions = c("Buttons"),
        options = list(
            dom = 'Bt',
            buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
            ordering = FALSE,
            columnDefs = list(
                list(class = 'dt-center', targets = 0:1)
            )
        ),
        rownames = FALSE
    )
    # y relative histogram
    z$y.relativeHistogram <- relativeHistogram(
        data = univariateSummary(
            tyrh,
            numBins = bin.numBins
        ),
        xLabel = yname,
        barColor = univariateGraph.barColor
    )
    # variable clusters
    z$variable_cluster <- variable_cluster(x,
                                           n = variable_cluster.n,
                                           na.rm = variable_cluster.na.rm)
    # woe/nrsq and cluster join
    if (ytype == 1) {
        z$woe <- lapply(x, function(x){woe(x, y,
                                           numBins = bin.numBins,
                                           equalBinSize = bin.equalBinSize,
                                           minPct = bin.minPct,
                                           maxPct = bin.maxPct)})
        for (nm in names(z$woe)) {
            z$woe[[nm]]$Variable <- nm
        }
        z$infoValue <- lapply(z$woe, infoValue)
        z$woeDT <- lapply(z$woe, woeDT)
        z$variable_cluster_plus <- variable_cluster_plus(z$variable_cluster, z$infoValue)
        temp <- c("woe", "infoValue", "woeDT")
    } else {
        z$nrsq <- lapply(x, function(x){nrsq(x, y, na.rm = nrsq.na.rm)})
        names(z$nrsq) <- names(x)
        z$variable_cluster_plus <- variable_cluster_plus(z$variable_cluster, z$nrsq)
        temp <- c("nrsq")
    }
    z$clusterDT <- lapply(z$variable_cluster_plus$VarName, function(x){
        xc <- filter(z$variable_cluster_plus, VarName == x)
        xc <- xc[, "PrimaryCluster"]
        y <- filter(z$variable_cluster_plus, PrimaryCluster == xc)
        class(y) <- class(z$variable_cluster_plus)
        clusterDT(y, highlightVars = x,
                  highlightColor = univariateGraph.barColor,
                  varNameTransform = function(x){
                      paste0(
                          "<a href=", '"#var-', unlist(lapply(x, digest)), '">', x, "</a>"
                      )
                  })
    })
    names(z$clusterDT) <- z$variable_cluster_plus$VarName
    z$clusterDT <- list(
        all = clusterDT(z$variable_cluster_plus, columnFilters = TRUE,
                        varNameTransform = function(x){
                            paste0(
                                "<a href=", '"#var-', unlist(lapply(x, digest)), '">', x, "</a>"
                            )
                        }),
        vars = z$clusterDT
    )
    # univariate summary
    z$univariateSummary <- lapply(x, function(x){
        univariateSummary(
            x = x,
            y = y,
            ytype = ytype,
            FUN = univariateSummary.FUN,
            numBins = bin.numBins,
            equalBinSize = bin.equalBinSize,
            minPct = bin.minPct,
            maxPct = bin.maxPct
        )
    })
    # univariate summary DT
    z$univariateSummaryDT <- list()
    for (nm in names(z$univariateSummary)) {
        z$univariateSummaryDT[[nm]] <- univariateSummaryDT(
            data = z$univariateSummary[[nm]],
            yLabel = univariateGraph.yLabel,
            yType = univariateGraph.yType,
            yDigits = univariateGraph.yDigits
        )
    }
    # univariate graph
    z$univariateGraph <- list()
    for (nm in names(z$univariateSummary)) {
        z$univariateGraph[[nm]] <- univariateGraph(
            data = z$univariateSummary[[nm]],
            xLabel = nm,
            yLabel = univariateGraph.yLabel,
            yType = univariateGraph.yType,
            yDigits = univariateGraph.yDigits,
            yRangeMode = univariateGraph.yRangeMode,
            barColor = univariateGraph.barColor,
            lineColor = univariateGraph.lineColor
        )
    }
    # return object
    z <- z[c("dataSummaryDT", "y.relativeHistogram",
             "variable_cluster", temp, "variable_cluster_plus",
             "clusterDT", "univariateSummary",
             "univariateSummaryDT", "univariateGraph")]
    class(z) <- c("mt_modelEDA", yt, class(z))
    return(z)
}
check_ytype <- function(y) {
    UseMethod("check_ytype")
}
check_ytype.binary <- function(y) {
    # validate y
    if (!(
        any(class(y) %in% c("logical", "integer", "numeric"))
        && all(!is.na(y))
        && all(unique(as.numeric(y)) %in% 0:1)
    )) {stop("y must be coercable to numeric with unique values 0 and 1")}
}
check_ytype.continuous <- function(y) {
    # validate y
    if (!(
        is.numeric(y)
        && all(!is.na(y))
        && length(unique(y)) >= 3
    )) {stop("y must be numeric with at least 3 unique values")}
}