% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/textToColumns.R
\name{textToColumns}
\alias{textToColumns}
\title{Split character vector into data frame columns}
\usage{
textToColumns(x, split)
}
\arguments{
\item{x}{character vector, each element of which is to be split}

\item{split}{character value to be used as the splitter}
}
\value{
A data frame of character vectors containing all of the split 
values. The number of columns will be the maximum number of splits found. 
Observations with less than the maximum number of splits will have 
\code{NA} values in columns for which they do not have a split. Column 
names will default to \code{V1 - Vn}, where \code{n} is the maximum number 
of splits found.
}
\description{
\code{textToColumns()} splits a character vector into data 
frame columns based on a specified split value
}
\examples{
x <- c("abc|123", "def|456")
textToColumns(x, "|")
}
\seealso{
\code{\link{strsplit}}
}
