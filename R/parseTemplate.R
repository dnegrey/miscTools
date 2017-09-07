#' @title Parse a template
#' @description \code{parseTemplate()} reads \code{fileIn} and uses 
#' \code{sprintf} to make string replacements.
#' @param fileIn path to template file (quoted string)
#' @param ... objects to use as string replacements
#' @return A string representing the contents of \code{fileIn} after the 
#' specified replacements have been made. As the string is a length one 
#' character vector, new lines (\code{\\n}) have been inserted accordingly.
#' @examples
#' x <- c("My %s favorite", "%s is a %s.")
#' write(x, "x.txt")
#' system("more x.txt")
#' y <- parseTemplate("x.txt", "least", "animal", "cat")
#' print(y)
#' write(y, "y.txt")
#' system("more y.txt")
#' file.remove("x.txt", "y.txt")
#' @seealso \code{\link{sprintf}}
#' @export
parseTemplate <- function(fileIn, ...) {
    # read in file
    x <- readLines(fileIn, warn = FALSE)
    # collapse with line breaks
    x <- paste(x, collapse = "\n")
    # apply replacements
    sprintf(x, ...)
}
