#' Seizure calculate
#'
#' This function will take the weights observed at the clinic dates and interpolate the weights of the patient 
#' on non-clinic days, based on linear interpolation
#' @param Anthropometrics The anthropometrics source table, to be turned into a table with interpolated weights
#' @return Anthropometrics table with interpolated weights
#' @keywords weight interpolation

weight_interp <- function(Anthropometrics) {
  
  #WT Interpolation
  anthro <- Anthropometrics
  #demo <- Demographics.Identified
  y1 <- as.Date(anthro$DATE[1], format="%m/%d/%Y")
  y2 <- as.Date(anthro$DATE[length(anthro[,1])], format="%m/%d/%Y")
  DATE <- seq(y1, y2, by="days")
  #WT
  table <- anthro[complete.cases(anthro$WT),]
  y <- as.numeric(na.omit(anthro$WT))
  i <- 1:(length(y)-1)
  x1 <- y[i]
  x2 <- y[i+1]
  diffx <- (x2 - x1)
  Date1 <- as.Date(table$DATE[i], format="%m/%d/%Y")
  Date2 <- as.Date(table$DATE[i+1], format="%m/%d/%Y")
  x <- Date2 - Date1
  difftotal <- as.numeric(x, units="days")
  
  WT_DAY <- c()
  for (i in seq(length(y)-1)) {
    WT_DAY <- c(WT_DAY, ((1:difftotal[i]/difftotal[i])*diffx[i])+x1[i])
  }
  WT_DAY <- c(table$WT[1], WT_DAY)
  
  z <- length(DATE)
  a <- which(!is.na(anthro$WT))[1]
  b <- as.Date(anthro$DATE[a], format="%m/%d/%Y")
  c <- which(DATE==b)
  WT_DAY <- c(rep(NA, c-1), WT_DAY)
  length(WT_DAY) <- z
  
  WT_DAY
  
  weights <- cbind.data.frame(rep(unique(anthro$MRNUMBER),length(WT_DAY)),DATE,WT_DAY)
  colnames(weights)[1] <- "MRNUMBER"
  return(weights)
}
