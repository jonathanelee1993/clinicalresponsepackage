#' Missing sums
#'
#' This function is used to assign ranking values to days with no observed data in the raw
#' data table, i.e. missing days. The rules are as follows: for each block of missing data,
#' count the number of consecutive missing days. Split this block up into two portions, either
#' of equal length if this block is of even length, or the upper subblock one longer than the
#' lower subblock if the block is of odd length. To fill the upper subblock, we will draw the
#' load and number values of the amount of days with observed data numbering the length of the
#' upper subblock immediately preceding the upper subblock and use those as the load and number
#' values for the subblock. Similar process is done for the lower subblock, looking at the days
#' with observed data immediately following the subblock. If the beginning or the end of the rows
#' of data are reached before the required number of rows of observed data have been found, repeat
#' the data found as many times as necessary to fill the subblock of missing data
#' @param x Subset of the seizure load table
#' @param y Subset of the DATA_QUALITY_S column of the raw data table that matches with the rows of the seizure load table being used by this function
#' @return The seizure load table with any missing values replaced with the appropriate seizure number/load value according to the missing data rules
#' @keywords rank range

# Purpose - This script is used to assign ranking values to days with no observed data in the raw
#           data table, i.e. missing days. The rules are as follows: for each block of missing data,
#           count the number of consecutive missing days. Split this block up into two portions, either
#           of equal length if this block is of even length, or the upper subblock one longer than the
#           lower subblock if the block is of odd length. To fill the upper subblock, we will draw the
#           load and number values of the amount of days with observed data numbering the length of the
#           upper subblock immediately preceding the upper subblock and use those as the load and number
#           values for the subblock. Similar process is done for the lower subblock, looking at the days
#           with observed data immediately following the subblock. If the beginning or the end of the rows
#           of data are reached before the required number of rows of observed data have been found, repeat
#           the data found as many times as necessary to fill the subblock of missing data
# Parameters - x = subset of the seizure load table
#              y = subset of the DATA_QUALITY_S column of the raw data table that matches with the rows
#                  of the seizure load table being used in this script
missing_sums <- function(x,y) {

  y <- y

  a <- 1
  while (a <= (dim(x)[1])) {

    if ((as.numeric(as.character(y[a])) == 3) && (is.na(x[a,9]))) {

      # Count the number of consecutive missing day rows, store this value as well as the row numbers
      k <- 1
      rownums <- c(a)
      if (a < dim(x)[1]) {
        for (j in (a+1):((dim(x)[1]))) {
          if (as.numeric(as.character(y[j])) == 3) {
            k <- k + 1
            rownums <- c(rownums,j)
          } else {
            break
          }
        }
      }

      # Determing the lengths of the upper and lower subblocks. If the first row of the table is found in the
      # block of missing data, there will be no upper subblock, and likewise with the lower subblock if the
      # last row of the table is found in the block of missing data. Otherwise, either make the upper and lower
      # subblock of even length, or of lengths ceiling(k/2) and floor(k/2)
      upper <- 0
      lower <- 0
      if (1 %in% rownums) {
        lower <- k
      } else if (dim(x)[1] %in% rownums) {
        upper <- k
      } else {
        if (k%%2 != 0) {
          upper <- ceiling(k/2)
          lower <- floor(k/2)
        } else {
          upper <- k/2
          lower <- k/2
        }
      }

      # Create temp data frames to store the data that will replace the upper and lower subblocks of the
      # missing data block
      tempupper <- data.frame()
      templower <- data.frame()

      if (upper != 0) {

        # Iterates upwards from the row immediately preceding the first row of the upper subblock to the first
        # row of the table and determine the first ceiling(k/2) amount of days to draw load and number
        # values from. If there are not enough days found before the first row of the table is reached,
        # repeat the values found as many times as necessary to fill the upper subblock with values
        tempupper <- x[x$date==x$date[a]-1,]
        while (length(unique(tempupper$date)) < upper & !(min(x$date[!is.na(x$severity)]) %in% unique(tempupper$date))) {
          tempupper <- rbind.data.frame(x[!is.na(x$severity) & x$date<min(tempupper$date) & x$date==max(x$date[!is.na(x$severity) & x$date<min(unique(tempupper$date))]),],tempupper)
        }
        if (length(unique(tempupper$date)) < upper) {
          n <- ceiling(upper/length(unique(tempupper$date)))
          tempupper <- do.call("rbind",replicate(n,tempupper,simplify=FALSE))
          if (length(unique(tempupper$date))*n > upper) {
            len <- (length(unique(templower$date))*n)-upper
            index <- 1
            while (index <= len) {
              tempdate <- tempupper$date[1]
              tempupper <- tempupper[-1,]
              if (tempdate!=tempupper$date[1]) {
                index <- index + 1
              }
            }
          }
        }

        # Replace the dates drawn from the rows in the table with the rows originally found in the upper subblock
        tempupper[,3:7] <- NA
        newdate <- x$date[a]
        if (dim(tempupper)[1] > 1) {
          if (length(unique(tempupper$date))==1 && upper>1) {
            for (i in 1:(dim(tempupper)[1]-1)) {
              storeolddate <- templower$date[i]
              templower$date[i] <- newdate
              newdate <- newdate + 1
            }
          } else {
            for (i in 1:(dim(tempupper)[1]-1)) {
              storeolddate <- tempupper$date[i]
              tempupper$date[i] <- newdate
              if (storeolddate != tempupper$date[i+1]) {
                newdate <- newdate + 1
              }
            }
          }
        }
        tempupper$date[dim(tempupper)[1]] <- newdate
      }

      if (lower != 0) {

        # Iterates downwards from the row immediately folliwing the last row of the upper subblock to the last
        # row of the table and determine the first floor(k/2) amount of days to draw load and number
        # values from. If there are not enough days found before the last row of the table is reached,
        # repeat the values found as many times as necessary to fill the lower subblock with values
        templower <- x[x$date==x$date[a+k],]
        while (length(unique(templower$date)) < lower & !(max(x$date[!is.na(x$severity)]) %in% unique(templower$date))) {
          templower <- rbind.data.frame(templower,x[!is.na(x$severity) & x$date>max(unique(templower$date)) & x$date==min(x$date[!is.na(x$severity) & x$date>max(unique(templower$date))]),])
        }
        if (length(unique(templower$date)) < lower) {
          n <- ceiling(lower/length(unique(templower$date)))
          templower <- do.call("rbind",replicate(n,templower,simplify=FALSE))
          if (length(unique(templower$date))*n > lower) {
            len <- (length(unique(templower$date))*n)-lower
            index <- 1
            while (index <= len) {
              tempdate <- templower$date[length(templower$date)]
              templower <- templower[-(length(templower$date)),]
              if (tempdate!=templower$date[length(templower$date)]) {
                index <- index + 1
              }
            }
          }
        }

        # Replace the dates drawn from the rows in the table with the rows originally found in the lower subblock
        templower[,3:7] <- NA
        newdate <- x$date[a+upper]
        if (dim(templower)[1] > 1) {
          if (length(unique(templower$date))==1 && lower>1) {
            for (i in 1:(dim(templower)[1]-1)) {
              storeolddate <- templower$date[i]
              templower$date[i] <- newdate
              newdate <- newdate + 1
            }
          } else {
            for (i in 1:(dim(templower)[1]-1)) {
              storeolddate <- templower$date[i]
              templower$date[i] <- newdate
              if (storeolddate != templower$date[i+1]) {
                newdate <- newdate + 1
              }
            }
          }
        }
        templower$date[dim(templower)[1]] <- newdate

      }

      # Split the table into two parts: the rows preceding the missing block of data, and the rows following
      # the missing block of data. Combine these parts with the tempupper and templower tables placed in
      # between them appropriately
      temp1 <- data.frame(x[-(a:(dim(x)[1])),])
      temp2 <- data.frame(x[-(1:(a+k-1)),])
      colnames(temp1) <- colnames(x)
      colnames(temp2) <- colnames(x)
      temp <- rbind.data.frame(temp1,tempupper,templower,temp2)
      colnames(temp) <- colnames(x)

      # Pad the DATA_QUALITY_S vector with the appropriate number of 3's and in the appropriate location
      # (This needs to be done because all missing days consist of one row in the raw data table, but drawing
      # data from other days that have multiple rows of data will lead to the size of the table increasing, and
      # this increase needs to be accounted for in order to loop through the table properly)
      if (a == 1) {
        y <- c(rep(3,(dim(temp)[1]-length(y)+k)),y[((a+k):length(y))])
      } else if (a == (dim(x)[1] - upper - lower + 1)) {
        y <- c(y[1:(a-1)],rep(3,(dim(temp)[1]-length(y)+k)))
      } else {
        y <- c(y[1:(a-1)],rep(3,(dim(temp)[1]-length(y)+k)),y[((a+k):length(y))])
      }

      # Determine the amount of rows that were added to the table after filling in missing data
      add <- (dim(temp)[1])-(dim(x)[1])
      x <- temp

      # Ensure that the rownames of the table are still increasing integers starting at 1
      rownames(x) <- seq(1:(dim(x)[1]))

      # Set a to be equal to the number of the row immediately following the block of missing data that now has
      # had load and number values placed into it. Since this block has been dealt with, we do not need to loop through
      # its rows. This will save some amount of time in running the script
      a <- a + lower + upper + add

    } else {
      a <- a + 1
    }
  }

  rownames(x) <- rep(NULL,dim(x)[1])
  return(x)
}
