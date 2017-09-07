#' @title Produce a standard model performance report
#' @description \code{modelPerformance_Report} produces a \code{\link{flexdashboard}} 
#' report based on a \code{mt_modelPerformance} object.
#' @param x \code{mt_modelPerformance} object
#' @param projectLabel character string; descriptive header for report
#' @param outFile character string; file path/name for saving report (*.html)
#' @param tempDir character string; temporary directory path
#' @return No object is returned. The report is saved to the specified location.
#' @examples 
#' # pull in sample scored data frame
#' x <- modelSampleScored
#' str(x)
#' head(x)
#' # binary target
#' # create mt_modelPerformance object
#' # note that "v" and the "variableImportance..." parms are optional
#' # in case you do not have a validation set or do not know/care about the variable importance
#' z <- modelPerformance(
#'     y = x$TargetFlag,
#'     yhat = x$pTargetFlag,
#'     v = x$ValidateFlag
#' )
#' modelPerformance_Report(z)
#' # continuous target
#' z <- modelPerformance(
#'     y = x[x$TargetFlag, ]$TargetValue,
#'     yhat = x[x$TargetFlag, ]$pTargetValue,
#'     v = x[x$TargetFlag, ]$ValidateFlag,
#'     gainsChartBarGraph.yType = "dlr",
#'     gainsChartBarGraph.yDigits = 0
#' )
#' modelPerformance_Report(z)
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
#' modelPerformance_Report(z)
#' @seealso \code{\link{modelPerformance}, \link{flex_dashboard}}
#' @importFrom rmarkdown render
#' @importFrom flexdashboard flex_dashboard
#' @export
modelPerformance_Report <- function(x,
                                    projectLabel = "Model Performance",
                                    outFile = "./modelPerformance_Report.html",
                                    tempDir = ".tempDir") {
    # validate parameters
    if (!isString(projectLabel)) {stop("projectLabel must be a string")}
    if (!isString(outFile)) {stop("outFile must be a string")}
    if (!isString(tempDir)) {stop("tempDir must be a string")}
    y <- list(
        projectLabel = projectLabel,
        outFile = outFile,
        tempDir = tempDir
    )
    UseMethod("modelPerformance_Report")
}
#' @export
modelPerformance_Report.mt_modelPerformance <- function(x, ...) {
    # create tempDir
    if (dir.exists(y$tempDir)) {
        cat("Removing tempDir ...", sep = "\n")
        unlink(y$tempDir, TRUE)
    }
    cat("Creating tempDir ...", sep = "\n")
    dir.create(y$tempDir)
    y$tempDir <- normalizePath(y$tempDir)
    # copy template files
    cat("Copying modelPerformance_Report template files ...", sep = "\n")
    file.copy(
        from = list.files(
            system.file(
                "extdata/modelPerformance",
                package = packageName()
            ),
            full.names = TRUE
        ),
        to = y$tempDir
    )
    # derive template type
    tt <- paste(c(as.integer(inherits(x, "HasValidate")),
                  as.integer(inherits(x, "HasVariableImportance"))),
                collapse = "")
    # copy appropriate template
    file.copy(
        from = paste0(y$tempDir, "/main", tt, ".Rmd"),
        to = paste0(y$tempDir, "/main.Rmd")
    )
    # add project label
    main <- parseTemplate(
        paste0(y$tempDir, "/main.Rmd"),
        y$projectLabel,
        y$projectLabel,
        y$projectLabel
    )
    write(
        main,
        paste0(y$tempDir, "/main.Rmd")
    )
    # render report
    render(
        paste0(y$tempDir, "/main.Rmd")
    )
    # copy report
    cat("Copying report to outFile ...", sep = "\n")
    file.copy(
        paste0(y$tempDir, "/main.html"),
        y$outFile,
        overwrite = TRUE
    )
    # remove tempDir
    cat("Removing tempDir ...", sep = "\n")
    unlink(y$tempDir, TRUE)
    # return logical success
    file.exists(y$outFile)
}
