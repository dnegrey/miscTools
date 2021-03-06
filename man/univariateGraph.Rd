% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/univariateGraph.R
\name{univariateGraph}
\alias{univariateGraph}
\title{Produce a univariate graph}
\usage{
univariateGraph(data, xLabel, yLabel, yType, yDigits, yRangeMode = "tozero",
  barColor = "#BDDFF7", lineColor = "#000000")
}
\arguments{
\item{data}{\code{mt_univariateSummary} data frame}

\item{xLabel}{character string; \code{x} variable label}

\item{yLabel}{character string; \code{y} variable label}

\item{yType}{character string; \code{y} variable format type; valid 
values are \code{"int"}, \code{"dlr"} and \code{"pct"}}

\item{yDigits}{non-negative integer value indicating the number of decimal 
places to show for values of the \code{y} variable}

\item{yRangeMode}{character string; \code{"tozero"} (y-axis starts at 0) or 
\code{"auto"} (y-axis extremes determined by data)}

\item{barColor}{character string; fill color for bars (valid color)}

\item{lineColor}{character string; line color (valid color)}
}
\value{
A \code{\link{plot_ly}} visualization of a 
mt_\code{\link{univariateSummary}} object, showing the distribution of a 
binned variable \code{x} overlayed with an aggregated summary statistic of a 
variable \code{y}.
}
\description{
\code{univariateGraph()} produces a univariate graph based on 
the contents of the input data frame, which must be a mt_\code{\link{univariateSummary}} 
object.
}
\examples{
# numeric x logical y
x <- c(rnorm(5000, 100, 10), rep(as.numeric(NA), 500))
y <- runif(5500)
z <- univariateSummary(x, y < 0.50)
univariateGraph(z, "Random Normal", "Random Uniform", "pct", 1)

# numeric x continuous y
z <- univariateSummary(x, y*100, 2, numBins = 15)
univariateGraph(z, "Random Normal", "Random Uniform", "dlr", 0)

# character x continuous y
z <- univariateSummary(iris$Species, iris$Sepal.Length, 2)
univariateGraph(z, "Species", "Sepal Length", "int", 2)
}
\seealso{
\code{\link{univariateSummary}}, \code{\link{plot_ly}}
}
