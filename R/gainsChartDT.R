#' @title Produce a gains chart datatable
#' @description \code{gainsChartDT} produces a nicely formatted \code{DT::datatable} 
#' representation of a \code{mt_gainsChart} object.
#' @param x \code{mt_gainsChart} object
#' @param scoreFormatType character string; format type for score field(s); valid values 
#' are "int", "dlr" and "pct"
#' @param scoreFormatDigits non-negative integer value; number of decimal places 
#' for score field(s)
#' @param contSumFormatType character string; format type for continuous sum field(s); valid values 
#' are "int", "dlr" and "pct"
#' @param contSumFormatDigits non-negative integer value; number of decimal places 
#' for continuous sum field(s)
#' @param contAvgFormatType character string; format type for continuous average (mean) field(s); valid values 
#' are "int", "dlr" and "pct"
#' @param contAvgFormatDigits non-negative integer value; number of decimal places 
#' for continuous average (mean) field(s)
#' @param KScolor character string; text color for max KS value (valid color)
#' @return A \code{datatable} representation of a \code{mt_gainsChart} object.
#' @examples 
#' # pull in sample scored data frame
#' x <- modelSampleScored
#' # filter to validation subset
#' x <- x[x$ValidateFlag, ]
#' # gains chart binary
#' y <- gainsChart(x$TargetFlag, x$pTargetFlag)
#' gainsChartDT(y)
#' # gains chart continuous
#' y <- gainsChart(x[x$TargetFlag, "TargetValue"], x[x$TargetFlag, "pTargetValue"])
#' gainsChartDT(y)
#' # gains chart combined
#' y <- gainsChart(x$TargetFlag, x$pTargetFlag*x$pTargetValue, x$TargetValue)
#' gainsChartDT(y)
#' @seealso \code{\link{gainsChart}, \link{datatable}}
#' @importFrom DT datatable %>% formatStyle styleEqual
#' @importFrom htmltools withTags tags
#' @export 
gainsChartDT <- function(x,
                         scoreFormatType = "int",
                         scoreFormatDigits = 5,
                         contSumFormatType = "dlr",
                         contSumFormatDigits = 0,
                         contAvgFormatType = "dlr",
                         contAvgFormatDigits = 0,
                         KScolor = "#FF3005") {
    # validate scoreFormatType
    if (!(
        is.character(scoreFormatType)
        && length(scoreFormatType) == 1
        && scoreFormatType %in% c("int", "dlr", "pct")
    )) {stop("scoreFormatType must be 'int', 'dlr' or 'pct'")}
    # validate scoreFormatDigits
    if (!(
        is.numeric(scoreFormatDigits)
        && length(scoreFormatDigits) == 1
        && scoreFormatDigits == as.integer(scoreFormatDigits)
        && scoreFormatDigits >= 0
    )) {stop("scoreFormatDigits must be a non-negative integer")}
    # validate contSumFormatType
    if (!(
        is.character(contSumFormatType)
        && length(contSumFormatType) == 1
        && contSumFormatType %in% c("int", "dlr", "pct")
    )) {stop("contSumFormatType must be 'int', 'dlr' or 'pct'")}
    # validate contSumFormatDigits
    if (!(
        is.numeric(contSumFormatDigits)
        && length(contSumFormatDigits) == 1
        && contSumFormatDigits == as.integer(contSumFormatDigits)
        && contSumFormatDigits >= 0
    )) {stop("contSumFormatDigits must be a non-negative integer")}
    # validate contAvgFormatType
    if (!(
        is.character(contAvgFormatType)
        && length(contAvgFormatType) == 1
        && contAvgFormatType %in% c("int", "dlr", "pct")
    )) {stop("contAvgFormatType must be 'int', 'dlr' or 'pct'")}
    # validate contAvgFormatDigits
    if (!(
        is.numeric(contAvgFormatDigits)
        && length(contAvgFormatDigits) == 1
        && contAvgFormatDigits == as.integer(contAvgFormatDigits)
        && contAvgFormatDigits >= 0
    )) {stop("contAvgFormatDigits must be a non-negative integer")}
    # parameter list
    pl <- list(
        scoreFormatType = scoreFormatType,
        scoreFormatDigits = scoreFormatDigits,
        contSumFormatType = contSumFormatType,
        contSumFormatDigits = contSumFormatDigits,
        contAvgFormatType = contAvgFormatType,
        contAvgFormatDigits = contAvgFormatDigits,
        KScolor = KScolor
    )
    UseMethod("gainsChartDT")
}
#' @export 
gainsChartDT.mt_gainsChart <- function(x, ...) {
    UseMethod("gainsChartDT.mt_gainsChart")
}
#' @export 
gainsChartDT.mt_gainsChart.binary <- function(x, ...) {
    sketch <- withTags(table(
        class = 'display',
        thead(
            tr(
                th(colspan = 3, "Score"),
                th(colspan = 2, "Frequency"),
                th(colspan = 3, "Relative Y"),
                th(colspan = 4, "Cumulative Y"),
                th(colspan = 3, "False Y"),
                th(rowspan = 2, "K-S")
            ),
            tr(
                th("Quantile"),
                th("Min"),
                th("Max"),
                th("Relative"),
                th("Cumulative"),
                th("Sum"),
                th("Mean"),
                th("Lift"),
                th("Sum"),
                th("Mean"),
                th("Lift"),
                th("Percent"),
                th("Relative", br(), "#"),
                th("Cumulative", br(), "#"),
                th("Cumulative", br(), "%")
            )
        )
    ))
    x$yhatBin <- as.character(x$yhatBin)
    x[nrow(x), "yhatBin"] <- "TOTAL"
    columnFormat <- function(z, cn, fmt, digits) {
        z[, cn] <- ifelse(
            is.na(z[, cn]),
            "",
            cleanNumberFormat(z[, cn], fmt, digits)
        )
        return(z)
    }
    x <- columnFormat(x, "minYhat", pl$scoreFormatType, pl$scoreFormatDigits)
    x <- columnFormat(x, "maxYhat", pl$scoreFormatType, pl$scoreFormatDigits)
    x <- columnFormat(x, "Freq", "int", 0)
    x <- columnFormat(x, "cumFreq", "int", 0)
    x <- columnFormat(x, "sumY", "int", 0)
    x <- columnFormat(x, "avgY", "pct", 1)
    x <- columnFormat(x, "Lift", "int", 0)
    x <- columnFormat(x, "cumSumY", "int", 0)
    x <- columnFormat(x, "cumAvgY", "pct", 1)
    x <- columnFormat(x, "cumLift", "int", 0)
    x <- columnFormat(x, "cumPctY", "pct", 1)
    x <- columnFormat(x, "nonY", "int", 0)
    x <- columnFormat(x, "cumNonY", "int", 0)
    x <- columnFormat(x, "cumPctNonY", "pct", 1)
    maxKS <- which(x$KS == max(x$KS, na.rm = TRUE))
    x <- columnFormat(x, "KS", "pct", 1)
    x[maxKS, "KS"] <- paste0(
        "<b style='color: ", pl$KScolor, ";'>",
        x[maxKS, "KS"],
        "</b>"
    )
    for (i in 1:length(x)) {
        x[nrow(x), i] <- paste0(
            "<b>",
            x[nrow(x), i],
            "</b>"
        )
    }
    y <- datatable(
        data = x,
        container = sketch,
        escape = FALSE,
        extensions = c("Buttons"),
        options = list(
            dom = 'Bt',
            buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
            pageLength = nrow(x),
            ordering = FALSE,
            columnDefs = list(
                list(class = 'dt-center', targets = 0:15)
            )
        ),
        rownames = FALSE
    ) %>% 
        formatStyle(
            c("Freq", "sumY", "cumSumY", "nonY", "KS"),
            borderLeft = "1px solid #111111"
        )
    for (i in 1:length(x)) {
        y <- y %>% 
            formatStyle(
                names(x)[i],
                borderTop = styleEqual(
                    x[nrow(x), names(x)[i]],
                    "1px solid #111111"
                )
            )
    }
    # return object
    class(y) <- c(class(y), "mt_gainsChartDT", "binary")
    return(y)
}
#' @export 
gainsChartDT.mt_gainsChart.continuous <- function(x, ...) {
    sketch <- withTags(table(
        class = 'display',
        thead(
            tr(
                th(colspan = 3, "Score"),
                th(colspan = 2, "Frequency"),
                th(colspan = 3, "Relative Y"),
                th(colspan = 4, "Cumulative Y"),
                th(rowspan = 2, "Cumulative", br(), "% Random"),
                th(rowspan = 2, "K-S")
            ),
            tr(
                th("Quantile"),
                th("Min"),
                th("Max"),
                th("Relative"),
                th("Cumulative"),
                th("Sum"),
                th("Mean"),
                th("Lift"),
                th("Sum"),
                th("Mean"),
                th("Lift"),
                th("Percent")
            )
        )
    ))
    x$yhatBin <- as.character(x$yhatBin)
    x[nrow(x), "yhatBin"] <- "TOTAL"
    columnFormat <- function(z, cn, fmt, digits) {
        z[, cn] <- ifelse(
            is.na(z[, cn]),
            "",
            cleanNumberFormat(z[, cn], fmt, digits)
        )
        return(z)
    }
    x <- columnFormat(x, "minYhat", pl$scoreFormatType, pl$scoreFormatDigits)
    x <- columnFormat(x, "maxYhat", pl$scoreFormatType, pl$scoreFormatDigits)
    x <- columnFormat(x, "Freq", "int", 0)
    x <- columnFormat(x, "cumFreq", "int", 0)
    x <- columnFormat(x, "sumY", pl$contSumFormatType, pl$contSumFormatDigits)
    x <- columnFormat(x, "avgY", pl$contAvgFormatType, pl$contAvgFormatDigits)
    x <- columnFormat(x, "Lift", "int", 0)
    x <- columnFormat(x, "cumSumY", pl$contSumFormatType, pl$contSumFormatDigits)
    x <- columnFormat(x, "cumAvgY", pl$contAvgFormatType, pl$contAvgFormatDigits)
    x <- columnFormat(x, "cumLift", "int", 0)
    x <- columnFormat(x, "cumPctY", "pct", 1)
    x <- columnFormat(x, "cumPctRandom", "pct", 1)
    maxKS <- which(x$KS == max(x$KS, na.rm = TRUE))
    x <- columnFormat(x, "KS", "pct", 1)
    x[maxKS, "KS"] <- paste0(
        "<b style='color: ", pl$KScolor, ";'>",
        x[maxKS, "KS"],
        "</b>"
    )
    for (i in 1:length(x)) {
        x[nrow(x), i] <- paste0(
            "<b>",
            x[nrow(x), i],
            "</b>"
        )
    }
    y <- datatable(
        data = x,
        container = sketch,
        escape = FALSE,
        extensions = c("Buttons"),
        options = list(
            dom = 'Bt',
            buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
            pageLength = nrow(x),
            ordering = FALSE,
            columnDefs = list(
                list(class = 'dt-center', targets = 0:13)
            )
        ),
        rownames = FALSE
    ) %>% 
        formatStyle(
            c("Freq", "sumY", "cumSumY", "cumPctRandom", "KS"),
            borderLeft = "1px solid #111111"
        )
    for (i in 1:length(x)) {
        y <- y %>% 
            formatStyle(
                names(x)[i],
                borderTop = styleEqual(
                    x[nrow(x), names(x)[i]],
                    "1px solid #111111"
                )
            )
    }
    # return object
    class(y) <- c(class(y), "mt_gainsChartDT", "continuous")
    return(y)
}
#' @export 
gainsChartDT.mt_gainsChart.combined <- function(x, ...) {
    sketch <- withTags(table(
        class = 'display',
        thead(
            tr(
                th(colspan = 3, "Score"),
                th(colspan = 2, "Frequency"),
                th(colspan = 3, "Binary Y"),
                th(colspan = 7, "Continuous Y"),
                th(rowspan = 2, "Cumulative", br(), "% Random"),
                th(rowspan = 2, "K-S")
            ),
            tr(
                th("Quantile"),
                th("Min"),
                th("Max"),
                th("Relative"),
                th("Cumulative"),
                th("Sum"),
                th("Mean"),
                th("Lift"),
                th("Sum"),
                th(style = "white-space: nowrap;", "Mean", sup("1")),
                th(style = "white-space: nowrap;", "Lift", sup("1")),
                th(style = "white-space: nowrap;", "Mean", sup("2")),
                th(style = "white-space: nowrap;", "Lift", sup("2")),
                th("Cumulative", br(), "Sum"),
                th("Cumulative", br(), "Percent")
            )
        )
    ))
    x$yhatBin <- as.character(x$yhatBin)
    x[nrow(x), "yhatBin"] <- "TOTAL"
    columnFormat <- function(z, cn, fmt, digits) {
        z[, cn] <- ifelse(
            is.na(z[, cn]),
            "",
            cleanNumberFormat(z[, cn], fmt, digits)
        )
        return(z)
    }
    x <- columnFormat(x, "minYhat", pl$scoreFormatType, pl$scoreFormatDigits)
    x <- columnFormat(x, "maxYhat", pl$scoreFormatType, pl$scoreFormatDigits)
    x <- columnFormat(x, "Freq", "int", 0)
    x <- columnFormat(x, "cumFreq", "int", 0)
    x <- columnFormat(x, "sumYB", "int", 0)
    x <- columnFormat(x, "avgYB", "pct", 1)
    x <- columnFormat(x, "LiftYB", "int", 0)
    x <- columnFormat(x, "sumYC", pl$contSumFormatType, pl$contSumFormatDigits)
    x <- columnFormat(x, "avgYC", pl$contAvgFormatType, pl$contAvgFormatDigits)
    x <- columnFormat(x, "LiftYC", "int", 0)
    x <- columnFormat(x, "perFreqYC", pl$contAvgFormatType, pl$contAvgFormatDigits)
    x <- columnFormat(x, "LiftYC2", "int", 0)
    x <- columnFormat(x, "cumSumYC", pl$contSumFormatType, pl$contSumFormatDigits)
    x <- columnFormat(x, "cumPctYC", "pct", 1)
    x <- columnFormat(x, "cumPctRandom", "pct", 1)
    maxKS <- which(x$KS == max(x$KS, na.rm = TRUE))
    x <- columnFormat(x, "KS", "pct", 1)
    x[maxKS, "KS"] <- paste0(
        "<b style='color: ", pl$KScolor, ";'>",
        x[maxKS, "KS"],
        "</b>"
    )
    for (i in 1:length(x)) {
        x[nrow(x), i] <- paste0(
            "<b>",
            x[nrow(x), i],
            "</b>"
        )
    }
    y <- datatable(
        data = x,
        caption = tags$caption(
            style = "caption-side: bottom; text-align: left;",
            withTags(
                span(
                    sup(1), "based on binary Y",
                    br(),
                    sup(2), "based on frequency"
                )
            )
        ),
        container = sketch,
        escape = FALSE,
        extensions = c("Buttons"),
        options = list(
            dom = 'Bt',
            buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
            pageLength = nrow(x),
            ordering = FALSE,
            columnDefs = list(
                list(class = 'dt-center', targets = 0:15)
            )
        ),
        rownames = FALSE
    ) %>% 
        formatStyle(
            c("Freq", "sumYB", "sumYC", "cumPctRandom", "KS"),
            borderLeft = "1px solid #111111"
        )
    for (i in 1:length(x)) {
        y <- y %>% 
            formatStyle(
                names(x)[i],
                borderTop = styleEqual(
                    x[nrow(x), names(x)[i]],
                    "1px solid #111111"
                )
            )
    }
    # return object
    class(y) <- c(class(y), "mt_gainsChartDT", "combined")
    return(y)
}