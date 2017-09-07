#' @title Generate a gains chart
#' @description \code{gainsChart} produces a gains chart table, evaluating 
#' \code{y} as the dependent variable and \code{yhat} as the vector of predicted values. 
#' When \code{yhat} is a combined model score, use \code{y2} to supply the other 
#' dependent variable. Users may specify the number of bins (i.e. 10 for deciles) that are desired.
#' @param y logical, integer or numeric vector (dependent variable)
#' @param yhat numeric vector (predicted values of \code{y})
#' @param y2 logical, integer or numeric vector (other dependent variable for combined models)
#' @param numBins integer value >= 2; number of desired bins
#' @return A data frame with class "\code{mt_gainsChart}" containing the 
#' following columns:
#' \itemize{
#'   \item \code{yhatBin}: integer specifying the bin number (i.e. decile)
#'   \item \code{minYhat}: minimum value of \code{yhat}
#'   \item \code{maxYhat}: maximum value of \code{yhat}
#'   \item \code{Freq}: number of observations (relative)
#'   \item \code{cumFreq}: number of observations (cumulative)
#'   \item \code{sumY}: sum of \code{y} values (relative)
#'   \item \code{sumYB} (combined only): sum of \code{y} (binary) values (relative)
#'   \item \code{avgY}: average of \code{y} values (relative)
#'   \item \code{avgYB} (combined only): average of \code{y} (binary) values (relative)
#'   \item \code{Lift}: lift of \code{avgY} (relative)
#'   \item \code{LiftYB} (combined only): lift of \code{avgYB} (relative)
#'   \item \code{sumYC} (combined only): sum of \code{y} (continuous) values (relative)
#'   \item \code{avgYC} (combined only): average of \code{y} (continuous) values (relative)
#'   \item \code{LiftYC} (combined only): lift of \code{avgYC} (relative)
#'   \item \code{perFreqYC} (combined only): average (per Freq) of \code{y} (continuous) values (relative)
#'   \item \code{LiftYC2} (combined only): lift of \code{perFreqYC} (relative)
#'   \item \code{cumSumY}: sum of \code{y} values (cumulative)
#'   \item \code{cumSumYC} (combined only): sum of \code{y} (continuous) values (cumulative)
#'   \item \code{cumAvgY}: average of \code{y} values (cumulative)
#'   \item \code{cumLift}: lift of \code{avgY} (cumulative)
#'   \item \code{cumPctY}: percentage of \code{y} values (cumulative)
#'   \item \code{cumPctYC} (combined only): percentage of \code{y} (continuous) values (cumulative)
#'   \item \code{nonY} (binary only): sum of \code{non-y} values (relative)
#'   \item \code{cumNonY} (binary only): sum of \code{non-y} values (cumulative)
#'   \item \code{cumPctNonY} (binary only): percentage of \code{non-y} values (cumulative)
#'   \item \code{cumPctRandom} (continuous & combined): uniform random percentage (cumulative)
#'   \item \code{KS}: K-S statistic
#' }
#' @examples 
#' # pull in sample scored data frame
#' x <- modelSampleScored
#' # filter to validation subset
#' x <- x[x$ValidateFlag, ]
#' # gains chart binary
#' gainsChart(x$TargetFlag, x$pTargetFlag)
#' # gains chart continuous
#' gainsChart(x[x$TargetFlag, "TargetValue"], x[x$TargetFlag, "pTargetValue"])
#' # gains chart combined
#' gainsChart(x$TargetFlag, x$pTargetFlag*x$pTargetValue, x$TargetValue)
#' @seealso \code{\link{glm}, \link{lm}}
#' @importFrom dplyr %>% group_by summarise n desc arrange mutate row_number select
#' @export
gainsChart <- function(y, yhat, y2 = NULL, numBins = 10) {
    # validate numBins
    if (!(
        is.numeric(numBins) 
        && length(numBins) == 1 
        && numBins == as.integer(numBins) 
        && numBins >= 2
    )) {
        stop("numBins must be an integer value >= 2")
    }
    # validate yhat
    if (!(
        is.numeric(yhat) 
        && sum(is.na(yhat)) == 0
    )) {
        stop("yhat must be numeric with no missing values")
    }
    # validate y and y2
    if (is.null(y2)) {
        cat("y2 is null; validating y ...", sep = "\n")
        if (!(
            any(class(y) %in% c("logical", "integer", "numeric")) 
            && sum(is.na(y)) == 0 
            && length(y) == length(yhat)
        )) {
            stop("y must be logical, integer or numeric with no missing values and the same length as yhat")
        }
        # proceed with y
        y <- as.numeric(y)
        # determine type of y
        yt <- "continuous"
        if (all(unique(y) %in% 0:1)) {
            yt <- "binary"
        }
        cat(paste("y has been determined to be", yt), sep = "\n")
        # assign model type
        mt <- yt
    } else {
        cat("y2 is not null; validating y and y2 ...", sep = "\n")
        if (is.null(y)) {
            stop("y cannot be null when y2 is not null")
        } else {
            # validate y
            if (!(
                any(class(y) %in% c("logical", "integer", "numeric")) 
                && length(y) == length(yhat)
            )) {
                stop("y must be logical, integer or numeric and the same length as yhat")
            }
            # proceed with y
            y <- as.numeric(y)
            # determine type of y
            yt <- "continuous"
            if (all(unique(y[!is.na(y)]) %in% 0:1)) {
                yt <- "binary"
            }
            cat(paste("y has been determined to be", yt), sep = "\n")
            # validate y2
            if (!(
                any(class(y2) %in% c("logical", "integer", "numeric")) 
                && length(y2) == length(yhat)
            )) {
                stop("y2 must be logical, integer or numeric and the same length as yhat")
            }
            # proceed with y2
            y2 <- as.numeric(y2)
            # determine type of y2
            y2t <- "continuous"
            if (all(unique(y2[!is.na(y2)]) %in% 0:1)) {
                y2t <- "binary"
            }
            cat(paste("y2 has been determined to be", y2t), sep = "\n")
            # stop if same type
            if (yt == y2t) {
                stop(paste("y and y2 cannot both be", yt))
            }
            # figure out which is which
            yb <- y
            yc <- y2
            if (yt == "continuous") {
                yb <- y2
                yc <- y
            }
            # stop if binary target has any missings
            if (sum(is.na(yb)) != 0) {
                stop("binary target cannot have any missing values")
            }
            # zero fill missings for continuous target
            yc <- ifelse(is.na(yc), 0, yc)
            # assign model type
            mt <- "combined"
        }
    }
    # at this point, all parameters have been validated
    # if mt %in% c("binary", "continuous") then single model
    # else (if mt == "combined") then combined model
    # bin yhat
    yhb <- bin(yhat, numBins = numBins, equalBinSize = TRUE)
    # evaluate combined model
    if (mt == "combined") {
        # combine parts
        z <- data.frame(
            yb = yb,
            yc = yc,
            yhb = yhb,
            yhat = yhat,
            stringsAsFactors = FALSE
        )
        # aggregate
        z <- z %>% 
            group_by(yhb) %>% 
            summarise(minYhat = min(yhat),
                      maxYhat = max(yhat),
                      Freq = n(),
                      sumYB = sum(yb),
                      sumYC = sum(yc)) %>% 
            data.frame() %>% 
            mutate(
                avgYB = sumYB/Freq,
                avgYC = sumYC/sumYB,
                perFreqYC = sumYC/Freq
            ) %>% 
            arrange(desc(minYhat)) %>% 
            mutate(yhatBin = row_number())
            z$cumFreq <- cumsum(z$Freq)
            z$cumSumYC <- cumsum(z$sumYC)
            z$cumPctYC <- z$cumSumYC/sum(z$sumYC)
            z$cumPctRandom <- z$yhatBin/nrow(z)
            z$LiftYB <- 100*(z$avgYB/(sum(z$sumYB)/sum(z$Freq)))
            z$LiftYC <- 100*(z$avgYC/(sum(z$sumYC)/sum(z$sumYB)))
            z$LiftYC2 <- 100*(z$perFreqYC/(sum(z$sumYC)/sum(z$Freq)))
            z$KS <- z$cumPctYC - z$cumPctRandom
            z <- z[c(
                "yhatBin",
                "minYhat",
                "maxYhat",
                "Freq",
                "cumFreq",
                "sumYB",
                "avgYB",
                "LiftYB",
                "sumYC",
                "avgYC",
                "LiftYC",
                "perFreqYC",
                "LiftYC2",
                "cumSumYC",
                "cumPctYC",
                "cumPctRandom",
                "KS"
            )]
            total <- z[nrow(z), ]
            for (i in 1:length(total)) {
                total[1, i] <- as.numeric(NA)
            }
            total$Freq <- sum(z$Freq)
            total$sumYB <- sum(z$sumYB)
            total$avgYB <- total$sumYB/total$Freq
            total$sumYC <- sum(z$sumYC)
            total$avgYC <- total$sumYC/total$sumYB
            total$perFreqYC <- total$sumYC/total$Freq
    } else {
        # combine parts
        z <- data.frame(
            y = y,
            yhb = yhb,
            yhat = yhat,
            stringsAsFactors = FALSE
        )
        # aggregate
        z <- z %>% 
            group_by(yhb) %>% 
            summarise(minYhat = min(yhat),
                      maxYhat = max(yhat),
                      Freq = n(),
                      sumY = sum(y)) %>% 
            data.frame() %>% 
            mutate(avgY = sumY/Freq) %>% 
            arrange(desc(minYhat)) %>% 
            mutate(yhatBin = row_number())
        z$cumFreq <- cumsum(z$Freq)
        z$cumSumY <- cumsum(z$sumY)
        z$cumAvgY <- z$cumSumY/z$cumFreq
        z$cumPctY <- z$cumSumY/sum(z$sumY)
        z$cumPctRandom <- z$yhatBin/nrow(z)
        z$Lift <- 100*(z$avgY/(sum(z$sumY)/sum(z$Freq)))
        z$cumLift <- 100*(z$cumAvgY/(sum(z$sumY)/sum(z$Freq)))
        z <- z[c(
            "yhatBin",
            "minYhat",
            "maxYhat",
            "Freq",
            "cumFreq",
            "sumY",
            "avgY",
            "Lift",
            "cumSumY",
            "cumAvgY",
            "cumLift",
            "cumPctY",
            "cumPctRandom"
        )]
        if (mt == "binary") {
            z <- select(z, -cumPctRandom)
            z$nonY <- z$Freq - z$sumY
            z$cumNonY <- cumsum(z$nonY)
            z$cumPctNonY <- z$cumNonY/sum(z$nonY)
            z$KS <- z$cumPctY - z$cumPctNonY
        } else {
            z$KS <- z$cumPctY - z$cumPctRandom
        }
        total <- z[nrow(z), ]
        for (i in 1:length(total)) {
            total[1, i] <- as.numeric(NA)
        }
        total$Freq <- sum(z$Freq)
        total$sumY <- sum(z$sumY)
        total$avgY <- total$sumY/total$Freq
        if (yt == "binary") {
            total$nonY <- total$Freq - total$sumY
        }
    }
    z <- rbind(z, total)
    z$yhatBin <- as.character(z$yhatBin)
    z[nrow(z), "yhatBin"] <- "Total"
    for (i in 2:length(z)) {
        z[, i] <- as.numeric(z[, i])
    }
    row.names(z) <- NULL
    class(z) <- c("mt_gainsChart", mt, class(z))
    return(z)
}