#' Make Lowercase
#'
#' This function ensure that all alphabetic data in the raw seizure data table is lowercase
#' @param x Column of the raw data table (either severity, length, type, variable, or cluster)
#' @return Column of the raw data table with all alphabetic data in lowercase (if it wasn't already)
#' @keywords make lowercase

# Purpose - This script ensures that all of the characters of a data value in the raw data
#           table are lowercase if they are not lower case already
# Parameters - x = column of the raw data table (either severity, length, type, variable, or cluster)
makelowercase <- function(x) {
  for (i in length(x)) {
    if (is.character(x[i])) {
      x[i] <- tolower(x[i])
    }
  }
  return(x)
}
