#' Calculate Ranks
#'
#' This function will take a portion of a ranking table and order it in
#' increasing numerical order based on ranking value
#' @param n Ranking subtable
#' @param m Maximum rank value
#' @return Ordered ranking subtable
#' @keywords calculate rank

# Purpose - This script will take a portion of a ranking table and order it in
#           increasing numerical order based on ranking value
# Parameters - n = ranking subtable, m = maximum rank value
calculate_ranks <- function(n,m) {
  ranks <- c()
  j <- 0
  while (j < (m+1)) {
    for (i in 1:(dim(n)[1])) {
      if (n$SEIZURE_RANKING[i] == j) {
        ranks <- rbind(ranks,n[i,colnames(n)=="SEIZURE_ENTRY" | colnames(n)=="SEIZURE_RANKING"])
      }
    }
    j <- j + 1
  }
  return(ranks)
}
