#' Rank With Values
#'
#' This function draws the appropriate ranking value from the appropriate ranking subtable
#' based on the value found in the raw data table (for variables that have discrete observations,
#' i.e. variable, type, and severity)
#' @param x Observed value in the raw data table
#' @param a First two columns of the ranking subtable
#' @return ranking value to be assigned to this observed data value
#' @keywords rank value

# Purpose - This script is used to draw the appropriate ranking value from the approriate ranking
#           subtable based on the value found in the raw data table. The function rankwithvalues
#           is used for the ranking of any variable that assigns ranking values based on discrete
#           values (i.e. variable, type, severity).
# Parameters - x = observed value in raw data table,
#              a = first two columns of ranking subtable
rankwithvalues <- function(x,a) {
  if ((is.null(x)) || (is.na(x)) || (x == "")) {
    return(0)
  }
  for (i in 1:((dim(a)[1])-1)) {
    if (tolower(as.character(x)) == as.character(a[i,1])) {
      return(as.numeric(a[i,2]))
    }
  }
  return(as.numeric(a[dim(a)[1],2]))
}
