#' @title Cluster a set of numeric variables
#' @description \code{variable_cluster()} performs non-hierarchical (disjoint) 
#' variable clustering on the numeric variables in a specified data frame. This 
#' approach is similar to that of the \code{varclus} procedure in SAS. Like 
#' that approach, the default behavior is to remove records with any missing 
#' values (\code{na.rm = TRUE}). Unlike that approach, however, missing values 
#' can be easily replaced with their column means (\code{na.rm = FALSE}). Note 
#' that this can greatly increase the size of the data to be clustered, which 
#' will increase the run time of the function. An example of this was timed on a 
#' data frame with about 230K observations and 200 variables where the function 
#' took 10 minutes. Future efforts should be made to optimize this or explore 
#' alternative approaches.
#' @param x data frame; contains the variables to be clustered
#' @param n integer value >= 2; number of desired clusters
#' @param na.rm logical value; should records with missing values be removed?
#' @return A data frame with class "\code{mt_variable_cluster}" containing the 
#' following columns:
#' \itemize{
#'   \item \code{VarName}: variable name (character)
#'   \item \code{PrimaryCluster}: assigned cluster (integer)
#'   \item \code{RsquaredToPrimaryCluster}: R-squared to primary cluster (numeric)
#'   \item \code{NearestCluster}: next nearest cluster (integer)
#'   \item \code{RsquaredToNearestCluster}: R-squared to nearest cluster (numeric)
#'   \item \code{OneMinusRsquaredRatio}: one minus R-squared ratio (numeric)
#' }
#' @examples 
#' variable_cluster(mtcars, 3)
#' @seealso \code{\link{kmeansvar}}
#' @importFrom ClustOfVar kmeansvar
#' @importFrom stats cor
#' @importFrom dplyr inner_join %>% mutate arrange desc
#' @export
variable_cluster <- function(x, n, na.rm = TRUE) {
    # validate x
    if (!(
        class(x) == "data.frame"
    )) {
        stop("x must be a data frame")
    }
    # validate n
    if (!(
        is.numeric(n)
        && length(n) == 1
        && n == as.integer(n)
        && n > 1
    )) {
        stop("n must be a single positive integer >= 2")
    }
    # validate na.rm
    if (!(
        is.logical(na.rm)
        && length(na.rm) == 1
    )) {
        stop("na.rm must be a single logical value")
    }
    # handle random seed
    randomState <- list(
        exist = exists(".Random.seed", envir = globalenv())
    )
    if (randomState$exist) {
        randomState$value <- get(".Random.seed", envir = globalenv())
        if (is.null(randomState$value)) {
            randomState$exist <- FALSE
            rm(".Random.seed", envir = globalenv())
        }
    }
    set.seed(seed = 1)
    # remove non-numeric columns
    nncn <- names(which(!unlist(lapply(x, is.numeric))))
    if (length(nncn) > 0) {
        cat("The following columns are non-numeric and will be removed:", sep = "\n")
        cat(nncn, sep = "\n")
        x <- x[!(names(x) %in% nncn)]
    }
    # remove all-missing columns
    amcn <- names(which(unlist(lapply(x, function(x){all(is.na(x))}))))
    if (length(amcn) > 0) {
        cat("The following columns have all missing values and will be removed:", sep = "\n")
        cat(amcn, sep = "\n")
        x <- x[!(names(x) %in% amcn)]
    }
    # remove missing rows if specified
    if (na.rm) {
        cat("na.rm is TRUE: observations with any missing values will be removed",
            sep = "\n")
        xk <- apply(x, 1, function(x){all(!is.na(x))})
        sxk <- sum(xk)
        nx <- nrow(x)
        cat(sprintf("%s of the %s (%s) records will be used",
                    cleanNumberFormat(sxk, "int", 0),
                    cleanNumberFormat(nx, "int", 0),
                    cleanNumberFormat(sxk/nx, "pct", 1)), sep = "\n")
        x <- x[xk, ]
    } else {
        cat("na.rm is FALSE: missing values will be replaced by column means",
            sep = "\n")
        x <- as.data.frame(
            lapply(as.list(x), function(x){
                x[is.na(x)] <- mean(x, na.rm = TRUE)
                return(x)
            })
        )
    }
    # identify constant columns
    ccn <- names(which(apply(x, 2, function(x){length(unique(x[!is.na(x)]))}) == 1))
    if (length(ccn) > 0) {
        cat("The following columns are constant and will be removed:", sep = "\n")
        cat(ccn, sep = "\n")
        x <- x[!(names(x) %in% ccn)]
    }
    # status on x
    cat(sprintf("x has %s eligible rows and %s eligible columns",
                nrow(x), length(x)), sep = "\n")
    # n must be <= length(x)
    if (n > length(x)) {
        stop(sprintf("n must be <= the number of elibile columns on x"))
    }
    # run variable clustering
    y <- kmeansvar(X.quanti = x, init = n)
    # construct output data frame
    z1 <- do.call(rbind, lapply(names(x), clusterStats, x = x, y = y, n = 1))
    z2 <- do.call(rbind, lapply(names(x), clusterStats, x = x, y = y, n = 2))
    names(z1)[2:3] <- c("PrimaryCluster", "RsquaredToPrimaryCluster")
    names(z2)[2:3] <- c("NearestCluster", "RsquaredToNearestCluster")
    z <- inner_join(z1, z2, "VarName") %>% 
        mutate(
            OneMinusRsquaredRatio = (1 - RsquaredToPrimaryCluster)/(1 - RsquaredToNearestCluster)
        ) %>% 
        arrange(PrimaryCluster, desc(RsquaredToPrimaryCluster), OneMinusRsquaredRatio)
    # restore random seed
    if (randomState$exist) {
        assign(".Random.seed", randomState$value, envir = globalenv())
    } else {
        rm(".Random.seed", envir = globalenv())
    }
    # assign class
    class(z) <- c("mt_variable_cluster", class(z))
    return(z)
}

clusterStats <- function(var, x, y, n) {
    z <- x[, var]
    z <- unlist(lapply(as.data.frame(y$scores), function(x){cor(x, z)^2}))
    z <- z[order(z, decreasing = TRUE)]
    z <- z[n]
    z <- data.frame(
        VarName = var,
        Cluster = as.integer(gsub("[^0-9]", "", names(z))),
        Rsquared = z,
        stringsAsFactors = FALSE
    )
    row.names(z) <- NULL
    return(z)
}
