#' Rank With Ranges
#'
#' This function draws the appropriate ranking value from the appropriate ranking subtable
#' based on the value found in the raw data table (for variables that have observations that
#' are observed in ranges, i.e. length and cluster)
#' @param x Observed value in the raw data table
#' @param b First two columns of the ranking subtable
#' @return ranking value to be assigned to this observed data value
#' @keywords rank range

# Purpose - This script is used to draw the appropriate ranking value from the approriate ranking
#           subtable based on the value found in the raw data table. The function rankwithranges is used for the ranking
#           of any variable that assigns ranking values based on ranges (i.e. length and cluster)
# Parameters - x = observed value in raw data table,
#              a = first two columns of ranking subtable,
#              b = first two columns of ranking subtable
rankwithranges <- function(x,b) {
  if ((is.null(x)) || (is.na(x)) || (x == "")) {
    return(0)
  } else if (tolower(as.character(x)) == "u") {
    for (i in 1:(dim(b)[1])) {
      if (b[i,1] == "u") {
        return(as.numeric(b[i,2]))
      }
    }
  }
  for (i in 1:(dim(b)[1])) {
    if (b[i,1] == "u") {
      b <- b[-i,]
      break
    }
  }
  if (dim(b)[1] > 1) {
    for (i in 1:((dim(b)[1])-1)) {
      if ((as.numeric(as.character(x))) >= as.numeric(as.character((b[i,1])))
          && (as.numeric(as.character((x))) < as.numeric(as.character(b[(i+1),1])))) {
        return(as.numeric(b[i,2]))
      }
    }
  }
  return(as.numeric(b[dim(b)[1],2]))
}
