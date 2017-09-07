#' @title Append information value / numeric R-squared to variable clusters
#' @description \code{variable_cluster_plus} appends a named list of information 
#' values or numeric R-squared values to a variable cluster data frame.
#' @param vc \code{mt_variable_cluster} object
#' @param x list; all elements must inherit either \code{mt_infoValue} or 
#' \code{mt_nrsq}; element names should be found in the \code{VarName} column in \code{vc}
#' @return A data frame with classes "\code{mt_variable_cluster_plus} and either 
#' "\code{binary}" or "\code{continuous}" (depending on whether \code{x} contains 
#' "\code{mt_infoValue}" or "\code{mt_nrsq}" objects, respectively). The returned 
#' object appends the values and ranks from \code{x} to \code{vc}.
#' @examples
#' # example with infoValue
#' vc <- variable_cluster(mtcars, 3)
#' x <- lapply(mtcars[names(mtcars) != "vs"], woe, y = mtcars$vs)
#' x <- lapply(x, infoValue)
#' variable_cluster_plus(vc, x)
#' 
#' # example with nrsq
#' x <- lapply(mtcars[names(mtcars) != "mpg"], nrsq, y = mtcars$mpg)
#' variable_cluster_plus(vc, x)
#' @seealso \code{\link{variable_cluster}, \link{woe}, \link{infoValue}, 
#' \link{nrsq}, \link{lapply}}
#' @importFrom dplyr %>% arrange desc inner_join select
#' @export
variable_cluster_plus <- function(vc, x) {
    # validate vc
    if (!(inherits(vc, "mt_variable_cluster"))){
        stop("vc must be a mt_variable_cluster object")
    }
    # validate x
    if (!(inherits(x, "list"))){
        stop("x must be a list")
    }
    check_info <- all(unlist(lapply(x, inherits, what = "mt_infoValue")))
    check_nrsq <- all(unlist(lapply(x, inherits, what = "mt_nrsq")))
    if (check_info) {
        xtype <- "info"
        xname <- "InformationValue"
        xclass <- "binary"
    } else if (check_nrsq) {
        xtype <- "nrsq"
        xname <- "RsquaredToTarget"
        xclass <- "continuous"
    } else {
        stop("all elements of x must inherit mt_infoValue or mt_nrsq")
    }
    temp <- data.frame(
        VarName = names(x),
        C2 = as.numeric(unlist(x)),
        stringsAsFactors = FALSE
    ) %>% 
        arrange(desc(C2))
    temp$C3 <- 1:nrow(temp)
    names(temp)[2] <- xname
    names(temp)[3] <-  paste0(names(temp)[2], "_Rank")
    tn <- names(temp)[3]
    y <- vc %>% 
        inner_join(temp, "VarName")
    y$TempRank <- y[, tn]
    y <- y %>% 
        arrange(
            PrimaryCluster,
            TempRank,
            OneMinusRsquaredRatio,
            desc(RsquaredToPrimaryCluster)
        ) %>% 
        select(-TempRank)
    class(y) <- c("mt_variable_cluster_plus", xclass, class(y))
    return(y)
}