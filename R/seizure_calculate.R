#' Seizure calculate
#'
#' This function will perform the calculations of the values for percent seizure free (on baseline days
#' and therapy days), seizure response (daily and for every 30 days), percent seizure
#' free response, seizure score, seizure number response (daily and for every 30 days),
#' and seizure number score
#' @param x Seizure load table
#' @param n Number that signifies which column number corresponse with the SEIZURE_NUMBER_DAY column
#' @param l Number that signifies which column number corresponds with the SEIZURE_LOAD_DAY column
#' @param baseline Subset of load table corresponding with baseline days
#' @param therapy Subset of load table corresponding with therapy days
#' @param patient Four-letter patient initials
#' @param mrnumber Medical record number object
#' @param type DAY_TYPE column object
#' @param quality DATA_QUALITY_S column object
#' @param number SEIZURE_NUMBER_DAY/SEIZURE_LOAD_DAY data frame object
#' @return Seizure response/score calculations
#' @keywords seizure response

# Purpose - This script will perform the calculations of the values for percent seizure free (on baseline days
#           and therapy days), seizure response (daily and for every 30 days), percent seizure
#           free response, seizure score, seizure number response (daily and for every 30 days),
#           and seizure number score
# Parameters - x = seizure load table
#              n = number that signifies which column number corresponds with the SEIZURE_NUMBER_DAY column
#              l = number that signifies which column number corresponds with the SEIZURE_LOAD_DAY column
#              baseline = subset of load table corresponding with baseline days
#              therapy = subset of load table corresponding with therapy days
#              patient = four-letter patient initials
#              mrnumber = medical record number object
#              type = DAY_TYPE column object
#              quality = DATA_QUALITY_S column object
#              number = SEIZURE_NUMBER_DAY/SEIZURE_LOAD_DAY data frame object
seizure_calculate <- function(x,n,l,baseline,therapy,patient,mrnumber,type,quality,number) {

  ## percent free days during baseline
  base.free <- dim(baseline[baseline$SEIZURE_NUMBER_DAY==0,])[1]
  free.base <- (base.free/dim(baseline)[1])*100
  print(paste("The percentage of baseline days with no seizures is:",free.base,"%"))

  ## percent free days during therapy
  therapy.30.days <- split(therapy[,l],ceiling(seq_along(therapy[,l])/30))
  c <- c(1:dim(therapy)[1])
  r <- c[c[]/30 > 0 & c[]/30 <= 1]
  free.30.days <- data.frame(therapy$DATE[min(r)],
                             therapy$DATE[max(r)],
                             (length(therapy.30.days[[1]][therapy.30.days[[1]]==0])/(length(therapy.30.days[[1]])))*100)
  colnames(free.30.days)[1:2] <- c("FIRST_DATE","LAST_DATE")
  colnames(free.30.days)[3] <- "%_SEIZURE_FREE_DAYS"
  if (ceiling(dim(therapy)[1]/30) >= 2) {
    for (i in 2:(ceiling(dim(therapy)[1]/30))) {
      therapy.free <- (length(therapy.30.days[[i]][therapy.30.days[[i]]==0])/(length(therapy.30.days[[i]])))*100
      r <- c[c[]/30 > i-1 & c[]/30 <= i]
      newrow <- data.frame(therapy$DATE[min(r)],therapy$DATE[max(r)],therapy.free)
      colnames(newrow) <- colnames(free.30.days)
      free.30.days <- data.frame(rbind(free.30.days,newrow))
      colnames(free.30.days) <- colnames(newrow)
    }
  }

  baseline <- subset(baseline,baseline[,l]!=0)
  base.median <- median(baseline[,l])
  base.number.median <- median(baseline[,n])

  t <- data.frame(type)
  q <- data.frame(quality)
  q <- q[,-1]
  sn <- data.frame(number[,1])
  sl <- data.frame(number[,2])
  response <- data.frame(rep(NA,dim(therapy)[1]),rep(NA,dim(therapy)[1]),rep(NA,dim(therapy)[1]))

  if (free.base!=100) {
    ## daily response calculation
    daily.response <- c((therapy[,l]/base.median)*100)
    daily.response <- data.frame(therapy$DATE,daily.response)
    colnames(daily.response)[1] <- "DATE"
    colnames(daily.response)[2] <- "SEIZURE_RESPONSE"

    ## daily number response calculation
    response <- c((therapy[,n]/base.number.median)*100)
    response <- data.frame(daily.response,response)
  }
  response <- data.frame(rep(mrnumber,dim(response)[1]),response)
  response[,2] <- therapy$DATE
  colnames(response)[1] <- "MRNUMBER"
  colnames(response)[2] <- "DATE"
  colnames(response)[3] <- "SEIZURE_RESPONSE"
  colnames(response)[4] <- "SEIZURE_NUMBER_RESPONSE"

  response <- data.frame(response[,1:2],t,q,sl,sn,response[,3:4])
  colnames(response)[c(3,4,5,6,7,8)] <- c("DAY_TYPE","DAY_QUALITY_S","SEIZURE_LOAD_DAY","SEIZURE_NUMBER_DAY",
                                          "SEIZURE_RESPONSE_DAY","SEIZURE_NUMBER_RESPONSE_DAY")

  period.response <- data.frame(rep(NA,dim(free.30.days)[1]),rep(NA,dim(free.30.days)[1]),rep(NA,dim(free.30.days)[1]),rep(NA,dim(free.30.days)[1]))
  period.response[,1:2] <- free.30.days[,1:2]
  colnames(period.response)[1:2] <- colnames(free.30.days)[1:2]
  if (free.base!=100) {
    ## 30 day response calculation
    for (i in 1:(ceiling(dim(therapy)[1]/30))) {
      r <- c()
      period.median <- median(therapy.30.days[[i]][therapy.30.days[[i]]!=0])
      if (is.na(period.median) || period.median == 0) {
        r <- 0
      } else {
        r <- (period.median/base.median)*100
      }
      period.response[i,3] <- r
    }

    colnames(period.response)[3] <- "SEIZURE_RESPONSE_30_DAYS"

    ## 30 day number response calculation
    therapy.number.30.days <- split(therapy[,n],ceiling(seq_along(therapy[,n])/30))
    period.number.response <- c()
    for (i in 1:(ceiling(dim(therapy)[1]/30))) {
      period.number.median <- median(therapy.number.30.days[[i]][therapy.number.30.days[[i]]!=0])
      if (is.na(period.number.median) || period.number.median == 0) {
        r <- 0
      } else {
        r <- (period.number.median/base.number.median)*100
      }
      period.number.response <- c(period.number.response,r)
    }

    period.response <- data.frame(period.response,period.number.response)
    colnames(period.response)[4] <- "SEIZURE_NUMBER_RESPONSE"
  }

  ## percent free response
  percent.free.response <- period.response
  percent.free.response[,3] <- 100 - (free.30.days[,3] - free.base)
  colnames(percent.free.response)[3] <- "%_SEIZURE_FREE_RESPONSE"

  overall.score <- percent.free.response
  overall.score[,3] <- data.frame(rep(NA,dim(period.response)[1]))
  overall.number.score <- percent.free.response
  overall.number.score[,3] <- data.frame(rep(NA,dim(period.response)[1]))
  if (free.base!=100) {
    ## score
    score.1 <- (((free.30.days[,3])/100) * ((percent.free.response[,3])/100)) * 100
    score.2 <- (((100-free.30.days[,3])/100) * ((period.response[,3])/100)) * 100
    score <- score.1 + score.2
    overall.score <- percent.free.response
    overall.score[,3] <- score
    colnames(overall.score)[3] <- "SEIZURE_SCORE_30_DAYS"

    ## number score
    score.3 <- (((free.30.days[,3])/100) * ((percent.free.response[,3])/100)) * 100
    score.4 <- (((100-free.30.days[,3])/100) * ((period.response[,4])/100)) * 100
    score2 <- score.3 + score.4
    overall.number.score <- percent.free.response
    overall.number.score[,3] <- score2
    colnames(overall.number.score)[3] <- "SEIZURE_NUMBER_SCORE"
  }

  results <- data.frame(overall.score[,1],
                        overall.score[,2],
                        cbind(free.30.days[,3],
                              period.response[,3],
                              period.response[,4],
                              percent.free.response[,3],
                              overall.score[,3],
                              overall.number.score[,3]))

  results <- data.frame(rep(mrnumber,dim(results)[1]),results)
  colnames(results)[1] <- "MRNUMBER"
  colnames(results)[2:3] <- c("FIRST_DATE","LAST_DATE")
  colnames(results)[4] <- "%_SEIZURE_FREE"
  colnames(results)[5] <- "SEIZURE_RESPONSE"
  colnames(results)[6] <- "SEIZURE_NUMBER_RESPONSE"
  colnames(results)[7] <- "% SEIZURE_FREE_RESPONSE"
  colnames(results)[8] <- "SEIZURE_SCORE"
  colnames(results)[9] <- "SEIZURE_NUMBER_SCORE"

  na <- rep(NA,dim(response)[1])
  outcome <- data.frame(response,a=na,b=na,c=na,d=na)
  colnames(outcome)[9] <- "%_SEIZURE_FREE_30_DAYS"
  colnames(outcome)[10] <- "SEIZURE_RESPONSE_30_DAYS"
  colnames(outcome)[11] <- "%_SEIZURE_FREE_RESPONSE_30_DAYS"
  colnames(outcome)[12] <- "SEIZURE_SCORE_30_DAYS"

  outcome[outcome$DATE %in% results$LAST_DATE,c(9:12)] <- results[,c(4,5,7,8)]

  return(outcome)
}
