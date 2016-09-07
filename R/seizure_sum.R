#' Seizure sum
#'
#' This function takes the raw seizure table and assign to each observed value within
#' within that table a ranking value based on the ranking assignments given within
#' the ranking table. If a row in the data table is a missing day (i.e. DATA_QUALITY_S = 3),
#' then assign NA values to that row for now, to be dealt with later.
#' @param x Raw data table
#' @param a Severity ranking subtable
#' @param b Length ranking subtable
#' @param c Type ranking subtable
#' @param d Variable ranking subtable
#' @param e Cluster ranking subtable
#' @return A table that stores the ranking values assigned to each observed data value (NA for missing data)
#' @keywords rank range

# Purpose - This script will take the raw seizure table and assign to each observed value within
#           within that table a ranking value based on the ranking assignments given within
#           the ranking table. If a row in the data table is a missing day (i.e. DATA_QUALITY_S = 3),
#           then assign NA values to that row for now, to be dealt with later
# Parameters - x = raw data table, a = severity ranking subtable, b = length ranking subtable,
#              c = type ranking subtable, d = variable ranking subtable, e = cluster ranking subtable
seizure_sum <- function(x,a,b,c,d,e) {
  srank <- rep(0,dim(x)[1]);
  lrank <- rep(0,dim(x)[1]);
  trank <- rep(0,dim(x)[1]);
  vrank <- rep(0,dim(x)[1]);
  crank <- rep(0,dim(x)[1]);
  seizureloadvalues <- rep(0,dim(x)[1]);
  ranktable <- cbind(srank,lrank,trank,vrank,crank)
  seizureloadvalues <- rep(0,dim(x)[1])
  for (i in 1:(dim(x)[1])) {
    if (x$DATA_QUALITY_S[i] == 3) {
      ranktable[i,c(1,2,3,4,5)] <- c(NA,NA,NA,NA,NA)
      seizureloadvalues[i] <- NA
    } else {
      ranktable[i,1] <- rankwithvalues(x$SEIZURE_SEVERITY[i],a[,c(1,2)])
      ranktable[i,2] <- rankwithranges(x$SEIZURE_LENGTH[i],b[,c(1,2)])
      ranktable[i,3] <- rankwithvalues(x$SEIZURE_TYPE[i],c[,c(1,2)])
      ranktable[i,4] <- rankwithvalues(x$SEIZURE_VARIABLES[i],d[,c(1,2)])
      ranktable[i,5] <- rankwithranges(x$SEIZURE_CLUSTER[i],e[,c(1,2)])
      for (j in 1:5) {
        seizureloadvalues[i] <- seizureloadvalues[i] + ranktable[i,j]
      }
    }
  }
  ranktable <- cbind(ranktable,seizureloadvalues)
  colnames(ranktable) <- c("severity","length","type","variables","cluster","load")
  return(ranktable)
}
