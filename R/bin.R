#' @title Bin a variable
#' @description \code{bin()} takes a vector \code{x} and returns a character 
#' vector of the same length, containing a binned version of \code{x}. 
#' Character, factor and logical vectors are coerced to character, while 
#' numeric vectors are binned according to parameter specification. This includes 
#' the number of desired bins, equally spaced vs. equally sized bins and the 
#' percentiles to force as the max/min endpoints of the low/high bins. 
#' \code{NA} values in \code{x} are returned as \code{NA} character values.
#' @usage
#' bin(x, ...)
#' 
#' ## S3 method for class 'numeric'
#' bin(x, numBins = 10, equalBinSize = FALSE, 
#'     minPct = 0, maxPct=100)
#' @param x character, factor, logical or numeric vector
#' @param numBins integer value >= 2; number of desired bins
#' @param equalBinSize logical value; return equally sized (\code{TRUE}) or 
#' equally spaced (\code{FALSE}) bins
#' @param minPct integer between 0 and 100 specifying a percentile to force as 
#' the max endpoint for the low (first) bin
#' @param maxPct integer between 0 and 100 specifying a percentile to force as 
#' the min endpoint for the high (last) bin (must be > \code{minPct})
#' @return A character vector containing binned values of \code{x}.
#' @examples 
#' # character, factor, logical
#' bin(letters)
#' bin(iris$Species)
#' bin(c(TRUE, FALSE, TRUE))
#' # numeric
#' x <- round(rnorm(50, 100, 5))
#' print(x)
#' bin(x)
#' y <- factor(bin(x))
#' z <- as.numeric(substr(levels(y), 2, regexpr(",", levels(y)) - 1))
#' y <- factor(bin(x), levels = levels(y)[order(z)])
#' summary(y)
#' @seealso \code{\link{cut}, \link{cut_number}, \link{quantile}}
#' @importFrom ggplot2 cut_number
#' @importFrom stats quantile
#' @export
bin <- function(x, ...) {
    UseMethod("bin")
}
#' @export
bin.character <- function(x, ...) {
    as.character(x)
}
#' @export
bin.factor <- function(x, ...) {
    bin.character(x)
}
#' @export
bin.logical <- function(x, ...) {
    bin.character(x)
}
#' @export
bin.numeric <- function(x, numBins = 10, equalBinSize = FALSE, 
                        minPct = 0, maxPct = 100) {
    # validate parms
    # numBins
    if (!(
        is.numeric(numBins) 
        && length(numBins) == 1 
        && numBins == as.integer(numBins) 
        && numBins >= 2
    )) {
        stop("numBins must be a length 1 integer >= 2")
    }
    # equalBinSize
    if (!(
        is.logical(equalBinSize) 
        && length(equalBinSize) == 1
    )) {
        stop("equalBinSize must be a length 1 logical")
    }
    # minPct
    if (!(
        is.numeric(minPct) 
        && length(minPct) == 1 
        && minPct == as.integer(minPct) 
        && minPct >= 0 
        && minPct <= 100
    )) {
        stop("minPct must be a length 1 integer between 0 and 100")
    }
    # maxPct
    if (!(
        is.numeric(maxPct) 
        && length(maxPct) == 1 
        && maxPct == as.integer(maxPct) 
        && maxPct >= 0 
        && maxPct <= 100
    )) {
        stop("maxPct must be a length 1 integer between 0 and 100")
    }
    if (!(
        minPct < maxPct
    )) {
        stop("minPct must be < maxPct")
    }
    # handle discrete as character, otherwise proceed as numeric
    z <- length(unique(x))
    if (z <= numBins) {
        print(sprintf(paste("x has [%s] unique values", 
                            "and will be treated as categorical"),
                      z))
        rm(z)
        y <- bin.character(x)
    } else if (!equalBinSize) {
        pcts <- quantile(x, seq(0, 1, 0.01), type = 3, na.rm = TRUE)
        if (minPct == 0 && maxPct == 100) {
            founde <- TRUE
            diglab <- 2
            while (founde) {
                diglab <- diglab + 1
                y <- as.character(cut(x, numBins, dig.lab = diglab))
                founde <- any(grepl("e", unique(y)))
                if (diglab == 10 && founde) {
                    founde <- FALSE
                }
            }
        } else if (maxPct == 100) {
            if (numBins < 3) {
                stop("numBins must be >= 3 when minPct > 0")
            }
            loMax <- pcts[minPct + 1]
            loBin <- ifelse(x <= loMax, 
                            paste0("(", 
                                   floor(min(x, na.rm = TRUE)) - 1, 
                                   ",", 
                                   loMax,"]"), 
                            NA)
            rest <- ifelse(x <= loMax, NA, x)
            # bin rest
            founde <- TRUE
            diglab <- 2
            while (founde) {
                diglab <- diglab + 1
                restBin <- as.character(cut(rest, numBins - 1, dig.lab = diglab))
                founde <- any(grepl("e", unique(restBin)))
                if (diglab == 10 && founde) {
                    founde <- FALSE
                }
            }
            # use regex to replace low value in restBin
            uloBin <- unique(loBin[!is.na(loBin)])
            uloBinMax <- substr(
                uloBin,
                regexpr(pattern = ",", text = uloBin)[[1]] + 1,
                nchar(uloBin) - 1
            )
            restBinLow <- restBin[min(which(rest == min(rest, na.rm = TRUE)))]
            restBinLowNew <- sub(
                pattern = "[^,]*",
                replacement = paste0("(", uloBinMax),
                x = restBinLow
            )
            restBin <- ifelse(restBin == restBinLow, restBinLowNew, restBin)
            y <- ifelse(is.na(restBin), loBin, restBin)
        } else if (minPct == 0) {
            if (numBins < 3) {
                stop("numBins must be >= 3 when maxPct < 100")
            }
            hiMin <- pcts[maxPct + 1]
            hiBin <- ifelse(x > hiMin, 
                            paste0("(", 
                                   hiMin,
                                   ",",
                                   ceiling(max(x, na.rm = TRUE)) + 1, 
                                   "]"), 
                            NA)
            rest <- ifelse(x > hiMin, NA, x)
            # bin rest
            founde <- TRUE
            diglab <- 2
            while (founde) {
                diglab <- diglab + 1
                restBin <- as.character(cut(rest, numBins - 1, dig.lab = diglab))
                founde <- any(grepl("e", unique(restBin)))
                if (diglab == 10 && founde) {
                    founde <- FALSE
                }
            }
            # use regex to replace low value in hiBin
            restBinHigh <- restBin[min(which(rest == max(rest, na.rm = TRUE)))]
            restBinHighMax <- substr(
                restBinHigh,
                regexpr(pattern = ",", text = restBinHigh)[[1]] + 1,
                nchar(restBinHigh) - 1
            )
            hiBin <- ifelse(x > hiMin, 
                            paste0("(", 
                                   restBinHighMax,
                                   ",",
                                   ceiling(max(x, na.rm = TRUE)) + 1, 
                                   "]"), 
                            NA)
            y <- ifelse(is.na(restBin), hiBin, restBin)
        } else {
            if (numBins < 4) {
                stop("numBins must be >= 4 when minPct > 0 and maxPct < 100")
            }
            loMax <- pcts[minPct + 1]
            loBin <- ifelse(x <= loMax, 
                            paste0("(", 
                                   floor(min(x, na.rm = TRUE)) - 1, 
                                   ",", 
                                   loMax,"]"), 
                            NA)
            hiMin <- pcts[maxPct + 1]
            hiBin <- ifelse(x > hiMin,
                            paste0("(",
                                   hiMin,
                                   ",",
                                   ceiling(max(x, na.rm = TRUE)) + 1,
                                   "]"),
                            NA)
            rest <- ifelse(x <= loMax | x > hiMin, NA, x)
            # bin rest
            restAllNA <- all(is.na(rest))
            if (restAllNA) {
                restBin <- as.character(rest)
            } else {
                founde <- TRUE
                diglab <- 2
                while (founde) {
                    diglab <- diglab + 1
                    restBin <- as.character(cut(rest, numBins - 2, dig.lab = diglab))
                    founde <- any(grepl("e", unique(restBin)))
                    if (diglab == 10 && founde) {
                        founde <- FALSE
                    }
                }
            }
            # use regex to replace low value in restBin
            uloBin <- unique(loBin[!is.na(loBin)])
            uloBinMax <- substr(
                uloBin,
                regexpr(pattern = ",", text = uloBin)[[1]] + 1,
                nchar(uloBin) - 1
            )
            if (!restAllNA){
                restBinLow <- restBin[min(which(rest == min(rest, na.rm = TRUE)))]
                restBinLowNew <- sub(
                    pattern = "[^,]*",
                    replacement = paste0("(", uloBinMax),
                    x = restBinLow
                )
                restBin <- ifelse(restBin == restBinLow, restBinLowNew, restBin)
            }
            restBin <- ifelse(is.na(restBin), loBin, restBin)
            # use regex to replace low value in hiBin
            if (!restAllNA){
                restBinHigh <- restBin[min(which(rest == max(rest, na.rm = TRUE)))]
                restBinHighMax <- substr(
                    restBinHigh,
                    regexpr(pattern = ",", text = restBinHigh)[[1]] + 1,
                    nchar(restBinHigh) - 1
                )
            } else {
                restBinHighMax <- uloBinMax
            }
            hiBin <- ifelse(x > hiMin, 
                            paste0("(", 
                                   restBinHighMax,
                                   ",",
                                   ceiling(max(x, na.rm = TRUE)) + 1, 
                                   "]"), 
                            NA)
            y <- ifelse(is.na(restBin), hiBin, restBin)
        }
    } else {
        founde <- TRUE
        diglab <- 2
        while (founde) {
            diglab <- diglab + 1
            y <- try(
                as.character(cut_number(x, numBins, dig.lab = diglab)),
                silent = TRUE
            )
            badtry <- inherits(y, "try-error")
            while (badtry) {
                if (numBins == 2) {
                    optsci <- options()$scipen
                    options(scipen = 999)
                    mid <- median(range(x, na.rm = TRUE))
                    y <- ifelse(x <= mid,
                                paste0("(",
                                       floor(min(x, na.rm = TRUE)) - 1,
                                       ",",
                                       mid,"]"),
                                paste0("(",
                                       mid,
                                       ",",
                                       ceiling(max(x, na.rm = TRUE)) + 1,
                                       "]"))
                    badtry <- FALSE
                    options(scipen = optsci)
                } else {
                    numBins <- numBins - 1
                    y <- try(
                        as.character(cut_number(x, numBins, dig.lab = diglab)),
                        silent = TRUE
                    )
                    badtry <- inherits(y, "try-error")
                }
            }
            founde <- any(grepl("e", unique(y)))
            if (diglab == 10 && founde) {
                founde <- FALSE
            }
        }
        # use regex to replace low value in lo bin
        binLow <- y[min(which(x == min(x, na.rm = TRUE)))]
        binLowNew <- sub(
            pattern = "[^,]*",
            replacement = paste0("(", floor(min(x, na.rm = TRUE)) - 1),
            x = binLow
        )
        y <- ifelse(y == binLow, binLowNew, y)
    }
    return(y)
}