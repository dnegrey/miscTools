#' @title Generate a weight of evidence (WoE) table
#' @description \code{woe()} produces a weight of evidence table, evaluating 
#' \code{x} as the independent variable and \code{y} as the dependent variable. 
#' It depends on an S3 generic function \link{bin} for binning the values of 
#' \code{x}. Character, factor and logical vectors for \code{x} are all treated 
#' as categorical. Numeric vectors are treated as continuous (numeric) as long 
#' as they have more distinct values than the number of bins (\code{numBins}) 
#' specified, otherwise they are treated as categorical. Integer vectors are 
#' handled as numeric vectors.
#' @param x character, factor, logical or numeric vector (independent variable)
#' @param y logical, integer or numeric vector that is coercable to numeric 
#' with unique values of 0 and 1 (dependent variable)
#' @param ... further arguments passed to \link{bin}
#' @return A data frame with class "\code{mt_woe}" summarizing the weight of 
#' evidence and information value calculations for an independent variable 
#' \code{x} with respect to a binary dependent variable \code{y}. 
#' @examples 
#' # factor
#' woe(iris$Species, round(runif(nrow(iris))))
#' 
#' # numeric - equally spaced vs. equally sized bins
#' x <- rnorm(1000, mean = 100, sd = 5)
#' y <- round(runif(1000))
#' woe(x, y)
#' woe(x, y, equalBinSize = TRUE)
#' 
#' # evaluate all columns in a data frame
#' wl <- lapply(mtcars, woe, y = round(runif(nrow(mtcars))))
#' names(wl)
#' wl$mpg
#' # create table of total info values
#' iv <- unlist(lapply(wl, infoValue))
#' data.frame(
#'     Variable = names(iv), 
#'     InformationValue = as.numeric(iv),
#'     stringsAsFactors = FALSE
#' )
#' @seealso \code{\link{bin}, \link{infoValue}, \link{woeDT}}
#' @importFrom dplyr %>% group_by summarise n arrange mutate
#' @export
woe <- function(x, y, ...) {
    
    # validate y
    if (!(
        any(class(y) %in% c("logical", "integer", "numeric"))
        && length(y) == length(x) 
        && all(unique(as.numeric(y)) %in% 0:1)
    )) {
        stop("y must be coercable to numeric with unique values 0 and 1")
    }

    # binning method
    xbin <- bin(x, ...)

    # coerce y to numeric
    y <- as.numeric(y)
    
    # combine x, xbin and y
    z <- data.frame(x = x,
                    xbin = xbin,
                    y = y,
                    stringsAsFactors = FALSE)

    # aggregate method
    z <- agg(x, z)
    
    # aggregate detail
    z$ResponsePercent <- z$ResponseCount/sum(z$ResponseCount)
    z$NonResponsePercent <- z$NonResponseCount/sum(z$NonResponseCount)
    z <- z %>% 
        mutate(
            PercentRatio = ifelse(NonResponsePercent != 0,
                                  ResponsePercent/NonResponsePercent,
                                  NA),
            LogPercentRatio = ifelse(PercentRatio>0,
                                     log(PercentRatio),
                                     NA),
            DifferencePercent = ResponsePercent - NonResponsePercent,
            InformationValue = ifelse(!is.na(LogPercentRatio),
                                      LogPercentRatio*DifferencePercent,
                                      NA),
            ResponseRate = ResponseCount/Total,
            BinSize = Total
        )
    
    # aggregate total
    zt <- z %>% 
        group_by(Method) %>% 
        summarise(
            ResponseCount = sum(ResponseCount),
            NonResponseCount = sum(NonResponseCount),
            InformationValue = sum(InformationValue, na.rm = TRUE)
        ) %>% 
        data.frame() %>% 
        mutate(
            xbin = "*** TOTAL ***",
            Total = as.numeric(NA),
            LowValue = as.numeric(NA),
            HighValue = as.numeric(NA),
            ResponsePercent = as.numeric(NA),
            NonResponsePercent = as.numeric(NA),
            PercentRatio = as.numeric(NA),
            LogPercentRatio = as.numeric(NA),
            DifferencePercent = as.numeric(NA),
            ResponseRate = ResponseCount/(ResponseCount + NonResponseCount),
            BinSize = ResponseCount + NonResponseCount
        )
    zt <- zt[, names(z)]
    
    # combine detail and total
    z <- rbind(z, zt)
    rm(zt)
    z <- mutate(
        z,
        Level = ifelse(is.na(xbin),
                       "(missing)",
                       xbin)
    )
    z$Variable <- getParms()$x
    z <- z[, c("Variable",
               "Method",
               "Level",
               "LowValue",
               "HighValue",
               "ResponseCount",
               "NonResponseCount",
               "ResponsePercent",
               "NonResponsePercent",
               "PercentRatio",
               "LogPercentRatio",
               "DifferencePercent",
               "ResponseRate",
               "BinSize",
               "InformationValue")]
    names(z) <- gsub("Response", "Target", names(z))
    class(z) <- c("mt_woe", class(z))
    return(z)
}

agg <- function(x, z) {
    UseMethod("agg")
}

agg.character <- function(x, z) {
    
    # aggregate by xbin
    z %>% 
        group_by(xbin) %>% 
        summarise(
            ResponseCount = sum(y),
            Total = n()
        ) %>% 
        data.frame() %>% 
        arrange(xbin) %>% 
        mutate(
            LowValue = as.numeric(NA),
            HighValue = as.numeric(NA),
            Method = "Categorical",
            NonResponseCount = Total - ResponseCount
        )

}

agg.factor <- function(x, z) {
    agg.character(x, z)
}

agg.logical <- function(x, z) {
    agg.character(x, z)
}

agg.numeric <- function(x, z) {
    
    # aggregate by xbin
    z %>% 
        group_by(xbin) %>% 
        summarise(
            ResponseCount = sum(y),
            Total = n(),
            LowValue = min(x),
            HighValue = max(x)
        ) %>% 
        data.frame() %>% 
        arrange(LowValue) %>% 
        mutate(
            Method = "Numeric",
            NonResponseCount = Total - ResponseCount
        )

}