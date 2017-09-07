#' miscTools
#'
#' @name miscTools
#' @description R package with miscellaneous tools
#' @docType package
NULL

#' Sample scored modeling dataset
#'
#' A data frame containing the basic contents of a scored modeling dataset.
#'
#' @format A data frame with the following structure:
#' \itemize{
#'   \item \code{ValidateFlag}: logical; distinguishes development/validation samples
#'   \item \code{TargetFlag}: logical; binary outcome for logistic model
#'   \item \code{TargetValue}: numeric; continuous outcome for linear model
#'   \item \code{pTargetFlag}: numeric; predicted value for logistic model
#'   \item \code{pTargetValue}: numeric; predicted value for linear model
#' }
"modelSampleScored"
