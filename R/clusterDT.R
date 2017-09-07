#' @title Produce a variable cluster datatable
#' @description \code{clusterDT} produces a nicely formatted \code{DT::datatable} 
#' representation of a \code{mt_variable_cluster} or \code{mt_variable_cluster_plus} 
#' object.
#' @param x \code{mt_variable_cluster} or \code{mt_variable_cluster_plus} object
#' @param columnFilters logical value; include column filters?
#' @param highlightVars character vector; \code{x$VarName} values used to highlight rows
#' @param highlightColor character string; highlight color (valid color)
#' @param varNameTransform function used to transform \code{x$VarName}; should 
#' take only one parameter and return a character vector of the same length. 
#' Example (bold effect): \code{varNameTransform = function(x){paste0("<b>", x, "</b>")}}
#' @return A \code{datatable} representation of a \code{mt_variable_cluster} or 
#' \code{mt_variable_cluster_plus} object.
#' @examples 
#' # regular variable cluster
#' x <- variable_cluster(mtcars, 3)
#' clusterDT(x)
#' 
#' # variable cluster plus info value
#' x <- variable_cluster(mtcars[names(mtcars) != "vs"], 3)
#' y <- lapply(mtcars[names(mtcars) != "vs"], woe, y = mtcars$vs)
#' y <- lapply(y, infoValue)
#' z <- variable_cluster_plus(x, y)
#' clusterDT(z)
#' 
#' # variable cluster plus R-squared
#' x <- variable_cluster(mtcars[names(mtcars) != "mpg"], 3)
#' y <- lapply(mtcars[names(mtcars) != "mpg"], nrsq, y = mtcars$mpg)
#' z <- variable_cluster_plus(x, y)
#' clusterDT(z)
#' @seealso \code{\link{variable_cluster}, \link{variable_cluster_plus}, \link{datatable}}
#' @importFrom DT datatable %>% formatCurrency formatStyle styleEqual
#' @export
clusterDT <- function(x, columnFilters = FALSE,
                      highlightVars = NA_character_,
                      highlightColor = "yellow",
                      varNameTransform = function(x){x}) {
    # validate parameters
    if (!(
        class(columnFilters) == "logical"
        && length(columnFilters) == 1
    )) {stop("columnFilters must be a logical value")}
    if (!(
        is.character(highlightVars)
    )) {stop("highlightVars must be a character vector")}
    if (!(
        is.character(highlightColor)
        && length(highlightColor) == 1
    )) {stop("highlightColor must be a character value")}
    if (!(
        class(varNameTransform) == "function"
    )) {stop("varNameTransform must be a function")}
    # setup
    vt <- data.frame(
        v = x$VarName,
        stringsAsFactors = FALSE
    )
    tmp <- varNameTransform(vt$v)
    if (!(
        is.character(tmp)
        && length(tmp) == length(vt$v)
    )) {stop("varNameTransform must return a character vector with the same length as x$VarName")}
    vt$vt <- tmp
    vt$hl <- vt$v %in% highlightVars
    x$VarName <- vt$vt
    if (any(!is.na(highlightVars))) {
        highlightVars <- vt[vt$hl, "vt"]
    }
    y <- list(
        x = x,
        columnFilters = columnFilters,
        highlightVars = highlightVars,
        highlightColor = highlightColor
    )
    # dispatch method
    UseMethod("clusterDT")
}
#' @export
clusterDT.mt_variable_cluster <- function(x, ...) {
    z <- clusterDT_main(NULL, NULL, y)
    class(z) <- c(class(z), "mt_clusterDT")
    return(z)
}
#' @export
clusterDT.mt_variable_cluster_plus <- function(x, ...) {
    UseMethod("clusterDT.mt_variable_cluster_plus")
}
#' @export
clusterDT.mt_variable_cluster_plus.binary <- function(x, ...) {
    z <- clusterDT_main(c("Information Value", "Information Value Rank"), 7, y)
    class(z) <- c(class(z), "mt_clusterDT", "binary")
    return(z)
}
#' @export
clusterDT.mt_variable_cluster_plus.continuous <- function(x, ...) {
    z <- clusterDT_main(c("Target R-squared", "Target R-squared Rank"), 7, y)
    class(z) <- c(class(z), "mt_clusterDT", "continuous")
    return(z)
}

clusterDT_main <- function(plusNames, plusColsRound, y) {
    datatable(
        data = y$x,
        escape = FALSE,
        filter = ifelse(y$columnFilters, "top", "none"),
        extensions = c("Buttons"),
        options = list(
            dom = 'Bt',
            buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
            pageLength = nrow(y$x),
            ordering = TRUE,
            columnDefs = list(
                list(class = 'dt-center', targets = 0:(length(y$x) - 1))
            )
        ),
        rownames = FALSE,
        colnames = c(
            c("Variable",
              "Primary Cluster",
              "Primary R-squared",
              "Nearest Cluster",
              "Nearest R-squared",
              "1 - R-squared Ratio"),
            plusNames
        )
    ) %>% 
        formatCurrency(c(3, 5:6, plusColsRound), currency = "", digits = 5) %>% 
        formatStyle(
            "VarName",
            target = "row",
            backgroundColor = styleEqual(
                y$highlightVars,
                rep(y$highlightColor, length(y$highlightVars))
            )
        )
}
