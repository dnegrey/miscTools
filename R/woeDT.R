#' @title Produce a weight of evidence (WoE) datatable
#' @description \code{mt_woeDT} produces a nicely formatted \code{DT::datatable} 
#' representation of a \code{mt_woe} object.
#' @param x \code{mt_woe} object
#' @return A \code{datatable} representation of a \code{mt_woe} object.
#' @examples 
#' woeDT(woe(iris$Species, iris$Sepal.Length > 5.75))
#' @seealso \code{\link{woe}, \link{datatable}}
#' @importFrom DT datatable
#' @export
woeDT <- function(x) {
    UseMethod("woeDT")
}
#' @export
woeDT.mt_woe <- function(x) {
    for (i in c(6:7, 14)) {
        x[, i] <- ifelse(is.na(x[, i]), "", cleanNumberFormat(x[, i], "int", 0))
    }
    for (i in c(8:9, 13)) {
        x[, i] <- ifelse(is.na(x[, i]), "", cleanNumberFormat(x[, i], "pct", 1))
    }
    for (i in c(10:12, 15)) {
        x[, i] <- ifelse(is.na(x[, i]), "", cleanNumberFormat(x[, i], "int", 5))
    }
    for (i in c(1:3)) {
        x[, i] <- ifelse(is.na(x[, i]), "", as.character(x[, i]))
    }
    x[nrow(x), "Level"] <- "TOTAL"
    for (i in 1:15) {
        x[nrow(x), i] <- paste0("<b>",
                                ifelse(is.na(x[nrow(x), i]),
                                       "",
                                       x[nrow(x), i]),
                                "</b>")
    }
    z <- datatable(
        data = x,
        escape = FALSE,
        extensions = c("Buttons"),
        options = list(
            dom = 'Bt',
            buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
            pageLength = nrow(x),
            ordering = FALSE,
            columnDefs = list(
                list(class = 'dt-center', targets = 0:14)
            )
        ),
        rownames = FALSE,
        colnames = c(
            "Variable",
            "Method",
            "Level",
            "Low Value",
            "High Value",
            "Target Count",
            "Non-Target Count",
            "Target Distribution",
            "Non-Target Distribution",
            "Target Odds",
            "Log Odds",
            "Distribution Difference",
            "Target Rate",
            "Bin Size",
            "Information Value"
        )
    )
    # return object
    class(z) <- c(class(z), "mt_woeDT")
    return(z)
}