% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gainsChartBarGraph.R
\name{gainsChartBarGraph}
\alias{gainsChartBarGraph}
\title{Produce a quantile bar graph based on gains chart(s)}
\usage{
gainsChartBarGraph(xb, xv = NULL, cb = "#009DDC", cv = "#77BF30",
  xLabel = "Model Quantile", yLabel = "Mean Outcome",
  yType = ifelse("binary" \%in\% class(xb), "pct", "dlr"),
  yDigits = ifelse("binary" \%in\% class(xb), 1, 0))
}
\arguments{
\item{xb}{\code{mt_gainsChart} object}

\item{xv}{\code{mt_gainsChart} object}

\item{cb}{character string; fill color for \code{xb} bars (valid color)}

\item{cv}{character string; fill color for \code{xv} bars (valid color)}

\item{xLabel}{character string; x-axis label}

\item{yLabel}{character string; y-axis label}

\item{yType}{character string; y-axis format type; valid 
values are \code{"int"}, \code{"dlr"} and \code{"pct"}}

\item{yDigits}{non-negative integer value indicating the number of decimal 
places to show when hovering over the bars}
}
\value{
A \code{\link{plot_ly}} visualization of a 
mt_\code{\link{gainsChart}} object, showing the average Y value (\code{avgY} 
for "binary"/"continuous"; \code{perFreqYC} for "combined") by quantile (
\code{yhatBin}).
}
\description{
\code{gainsChartBarGraph} produces a bar graph based on a 
mt_\code{\link{gainsChart}} object. When two such objects are supplied, the 
function produces a combined bar graph, where the first object (\code{xb}) is 
labeled as "Build" and the second object (\code{xv}) is labeled as "Validate".
}
\examples{
# pull in sample scored data frame
x <- modelSampleScored
# filter to validation subset
x <- split(x, x$ValidateFlag)
# gains chart binary
y <- lapply(x, function(z){gainsChart(z$TargetFlag, z$pTargetFlag)})
# bar graph
gainsChartBarGraph(y[[1]], y[[2]])
# gains chart continuous
y <- lapply(x, function(z){
    gainsChart(z[z$TargetFlag, "TargetValue"], z[z$TargetFlag, "pTargetValue"])
})
# bar graph
gainsChartBarGraph(y[[1]], y[[2]])
# gains chart combined
y <- lapply(x, function(z){
    gainsChart(z$TargetFlag, z$pTargetFlag*z$pTargetValue, z$TargetValue)
})
# bar graph
gainsChartBarGraph(y[[1]], y[[2]])
}
\seealso{
\code{\link{gainsChart}, \link{plot_ly}}
}
