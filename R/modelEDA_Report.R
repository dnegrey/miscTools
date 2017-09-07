#' @title Produce a standard modelEDA report
#' @description \code{modelEDA_Report} produces a \code{\link{flexdashboard}} 
#' report based on a \code{mt_modelEDA} object.
#' @param x \code{mt_modelEDA} object
#' @param projectLabel character string; description header for report home page
#' @param outFile character string; file path/name for saving report (*.html)
#' @param tempDir character string; temporary directory path
#' @return No object is returned. The report is saved to the specified location.
#' @examples 
#' # binary target
#' modelEDA_Report(modelEDA(mtcars, "vs", 1))
#' 
#' # continuous target
#' modelEDA_Report(modelEDA(
#'     x = mtcars,
#'     yname = "mpg",
#'     ytype = 2,
#'     univariateGraph.yLabel = "Average MPG",
#'     univariateGraph.yType = "int",
#'     univariateGraph.yDigits = 1
#' ))
#' @seealso \code{\link{modelEDA}, \link{flex_dashboard}}
#' @importFrom rmarkdown render
#' @importFrom flexdashboard flex_dashboard
#' @importFrom digest digest
#' @export
modelEDA_Report <- function(x,
                            projectLabel = "Model EDA",
                            outFile = "./modelEDA_Report.html",
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
    UseMethod("modelEDA_Report")
}
#' @export
modelEDA_Report.mt_modelEDA <- function(x, ...) {
    # create tempDir
    if (dir.exists(y$tempDir)) {
        cat("Removing tempDir ...", sep = "\n")
        unlink(y$tempDir, TRUE)
    }
    cat("Creating tempDir ...", sep = "\n")
    dir.create(y$tempDir)
    # derive ytype
    yt <- c("binary", "continuous")[c("binary", "continuous") %in% class(x)]
    # copy template files
    cat("Copying modelEDA_Report template files ...", sep = "\n")
    file.copy(
        from = list.files(
            system.file(
                "extdata/modelEDA",
                package = packageName()
            ),
            full.names = TRUE
        ),
        to = y$tempDir
    )
    file.copy(
        from = paste0(y$tempDir, "/variable-", yt, ".Rmd"),
        to = paste0(y$tempDir, "/variable.Rmd")
    )
    # add project label
    home <- parseTemplate(
        paste0(y$tempDir, "/home.Rmd"),
        y$projectLabel
    )
    write(
        home,
        paste0(y$tempDir, "/home.Rmd")
    )
    # get variable names
    vn <- names(x$univariateSummary)
    # construct variable pages
    for (i in vn) {
        vp <- parseTemplate(
            paste0(y$tempDir, "/variable.Rmd"),
            i, paste0("var-", digest(i)), i, i, i, i
        )
        write(
            vp,
            paste0(y$tempDir, "/main.Rmd"),
            append = TRUE
        )
    }
    # render report
    render(
        paste0(y$tempDir, "/main.Rmd")
    )
    cat("Copying report to outFile ...", sep = "\n")
    file.copy(
        paste0(y$tempDir, "/main.html"),
        y$outFile,
        overwrite = TRUE
    )
    # remove tempDir
    cat("Removing tempDir ...", sep = "\n")
    unlink(y$tempDir, TRUE)
}