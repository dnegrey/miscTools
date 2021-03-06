% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cleanNumberFormat.R
\name{cleanNumberFormat}
\alias{cleanNumberFormat}
\title{Convert a numeric vector to character with pretty formatting}
\usage{
cleanNumberFormat(x, type, digits = 0)
}
\arguments{
\item{x}{numeric vector to format}

\item{type}{string describing the format type; valid values are 
\code{"int"}, \code{"dlr"} and \code{"pct"}}

\item{digits}{non-negative integer value indicating the number of decimal 
places to be used}
}
\value{
A character vector of formatted numbers.
}
\description{
\code{cleanNumberFormat} applies commonly used numeric 
formatting conventions to numeric vectors and returns them as character 
vectors
}
\examples{
x <- rnorm(5)
print(x)
cleanNumberFormat(x, "pct", 1)
cleanNumberFormat(x*10000, "int")
cleanNumberFormat(x*10000, "dlr", digits = 2)
}
\seealso{
\code{\link{round}, \link{format}, \link{trimws}}
}
