#' @rdname textToColumns
#' @title Split character vector into data frame columns
#' @description \code{textToColumns()} splits a character vector into data 
#' frame columns based on a specified split value
#' @param x character vector, each element of which is to be split
#' @param split character value to be used as the splitter
#' @return A data frame of character vectors containing all of the split 
#' values. The number of columns will be the maximum number of splits found. 
#' Observations with less than the maximum number of splits will have 
#' \code{NA} values in columns for which they do not have a split. Column 
#' names will default to \code{V1 - Vn}, where \code{n} is the maximum number 
#' of splits found.
#' @examples 
#' x <- c("abc|123", "def|456")
#' textToColumns(x, "|")
#' @seealso \code{\link{strsplit}}
#' @export
textToColumns <- function(x, split) {
    
    # x must be character vector
    if (!(is.character(x))) {
        stop("x must be a character vector")
    }
    
    # split must be character value
    if (!(
        is.character(split) 
        && length(split) == 1
    )) {
        stop("split must be a length 1 character vector")
    }

    # split
    y <- strsplit(x, split, fixed = TRUE)
    
    # convert to list of matrices
    y <- lapply(y, rbind)
    
    # find max length
    ml <- max(unlist(lapply(y, length)))
    
    # loop to make consistent matrix layout
    z <- y
    for (i in 1:length(z)) {
        fill <- ml - length(z[[i]])
        if (fill > 0) {
            fill <- (ml - fill + 1):ml
            for (j in fill) {
                z[[i]] <- cbind(z[[i]], as.character(NA))
            }
        }
    }
    
    # stack and convert to data frame
    z <- do.call(rbind, z)
    z <- as.data.frame(z)
    
    # clean up
    names(z) <- paste0("V", 1:length(z))
    for (k in 1:length(z)) {
        z[, k] <- as.character(z[, k])
    }
    row.names(z) <- NULL
    
    # result
    return(z)

}
