#' Calculate seizure load
#'
#' This function allow you to start the process of seizure load/response calculations
#' @keywords seizure load
#' @export
#' @import RMySQL
#' @import ggplot2
#' @import openxlsx
#' @examples
#' calculate_seizure_load()

# Purpose - This script will take raw seizure data from the FILA_SEIZURE_DATA_SOURCE file and ranking data
#           from the FILA_SEIZURE_RANKING_DATA_SOURCE file and calculate daily seizure loads based on the
#           ranking values given for that patient
#           The script will give you as the user the option to specify the identifer signifying
#           the patient you wish to run this script for, and the directory where the two needed xlsx files
#           can be found
#           When the script finishes calculating daily seizure loads, you will be asked if you wish to save
#           a FILA_SEIZURE_LOAD file into the work directory for your viewing
calculate_seizure_load <- function() {

  # Input patient identifier
  print("Input the identifier that signify the patient we are doing calculations for")
  print("Example: FILA")
  patient <- readline(prompt="Enter here: ")

  # Input directory in which the SEIZURE_DATA_SOURCE and SEIZURE_RANKING_SOURCE files can be found
  # This script assumes that they are located within the same folder
  print("Input the directory that you wish to draw this patient's SEIZURE_DATA_SOURCE file from")
  print("Example: C:/Folder_Name/")
  directory <- readline(prompt="Enter here: ")
  setwd(directory)

  # Paste the patient string using paste and gsub functions to create the two file names
  # Then use read.xlsx() to read these files into R as data frames
  data <- "SEIZURE_DATA_SOURCE.xlsx"
  data <- gsub(" ","",paste(patient,"_",data))
  data <- read.xlsx(data,sheet=1,detectDates=TRUE)
  
  print("Type 'yes' if this patient's SEIZURE_RANKING_SOURCE file can be found in the same folder as this patient's SEIZURE_DATA_SOURCE table. Type 'no' if it is in a different folder")
  answer <- ""
  while(tolower(answer)!="yes" & tolower(answer)!="no") {
    answer <- readline(prompt="Enter here: ")
  }
  if (answer=="no") {
    print("Input the directory that you wish to draw this patient's SEIZURE_DATA_SOURCE file from")
    print("Example: C:/Folder_Name/")
    directory <- readline(prompt="Enter here: ")
    setwd(directory)
  }

  ranking <- "SEIZURE_RANKING_SOURCE.xlsx"
  ranking <- gsub(" ","",paste(patient,"_",ranking))
  ranking <- read.xlsx(ranking,sheet=1,detectDates=TRUE)

  # Remove any fully empty, dataless rows in the raw data table
  # Then remove the time stamp from each date if they are listed
  data <- data[!is.na(data$MRNUMBER),]
  for (i in 1:length(data$DATE)) {
    if (nchar(as.character(data$DATE[i]))>10) {
      data$DATE <- substr(data$DATE,1,nchar(as.character(data$DATE))-5)
    }
  }
  data$DATE <- as.Date(data$DATE,format="%m/%d/%Y")

  # Save data table in its current form to object "savedata". This will be used later in the script
  savedata <- data[,!is.na(colnames(data)) & colnames(data)!="ENTERED" & colnames(data)!="AUDITED" & colnames(data)!="COMMENTS"] ## Save a copy of the FILA_SEIZURE_DATA_SOURCE file for later use

  # Save medical record number to object "mrnumber"
  mrnumber <- unique(data$MRNUMBER)

  print("Calculating ranks. Please wait...")

  # Break up ranking table into five subtables, split by the seizure parameter
  # Save each of these tables into separate objects
  n1 <- ranking[grepl("severity",tolower(ranking$SEIZURE_PARAMETER)),]
  n2 <- ranking[grepl("length",tolower(ranking$SEIZURE_PARAMETER)),]
  n3 <- ranking[grepl("type",tolower(ranking$SEIZURE_PARAMETER)),]
  n4 <- ranking[grepl("variable",tolower(ranking$SEIZURE_PARAMETER)),]
  n5 <- ranking[grepl("cluster",tolower(ranking$SEIZURE_PARAMETER)),]

  # Calculate max ranking value found in each subtable
  n1max <- max(n1$SEIZURE_RANKING)
  n2max <- max(n2$SEIZURE_RANKING)
  n3max <- max(n3$SEIZURE_RANKING)
  n4max <- max(n4$SEIZURE_RANKING)
  n5max <- max(n5$SEIZURE_RANKING)

  # Call function that will organize the ranking values in each subtable by increasing numerical order
  ranks1 <- calculate_ranks(n1,n1max)
  ranks2 <- calculate_ranks(n2,n2max)
  ranks3 <- calculate_ranks(n3,n3max)
  ranks4 <- calculate_ranks(n4,n4max)
  ranks5 <- calculate_ranks(n5,n5max)

  # Ensure that all data in the raw data table is in a consistent lowercase form
  data$SEIZURE_SEVERITY <- makelowercase(data$SEIZURE_SEVERITY)
  data$SEIZURE_LENGTH <- makelowercase(data$SEIZURE_LENGTH)
  data$SEIZURE_TYPE <- makelowercase(data$SEIZURE_TYPE)
  data$SEIZURE_VARIABLE <- makelowercase(data$SEIZURE_VARIABLE)
  data$SEIZURE_CLUSTER <- makelowercase(data$SEIZURE_CLUSTER)

  # Call function that will assign the appropriate ranking value to each observation in the raw data
  # table according to the values found in the ranking subtables
  # Then save the resulting load table to object "values"
  # Then bind the columns of the load table to the following columns of the raw data table:
  # DAY_TYPE, SEIZURE_NUMBER
  values <- seizure_sum(data,ranks1,ranks2,ranks3,ranks4,ranks5)
  values <- cbind(as.numeric(data$DAY_TYPE),values[,1:5],as.numeric(data$SEIZURE_NUMBER),values[,6])

  # Multiply the sum of the rankings in each row of the load table by that row's seizure number
  values[,8] <- (values[,7]*values[,8])
  colnames(values)[7:8] <- c("number","sum")

  # Bind the columns of the load table to the DATE column of the raw data table
  values <- data.frame(data$DATE,values)
  colnames(values)[c(1,2)] <- c("date","day_type")

  print("Calculating sums for missing days. Please wait...")

  # Call a function that will fill rows with missing data with values for seizure load and seizure number
  # This function will be called two times: first for the subset of the load table with
  # day type equal to 1 (baseline days), then for the subset of the load table with day type
  # not equal to 1 (days on therapy)
  # Then bind the resulting load subtables of the two calls to the function back into one table
values1 <- missing_sums(values[values$day_type==1,],data[data$DAY_TYPE==1,colnames(data)=="DATA_QUALITY_S"])
  values2 <- missing_sums(values[values$day_type!=1,],data[data$DAY_TYPE!=1,colnames(data)=="DATA_QUALITY_S"])
  values1 <- data.frame(values1)
  values2 <- data.frame(values2)
  colnames(values1) <- colnames(values)
  colnames(values2) <- colnames(values1)
  values <- rbind.data.frame(values1,values2)

  # Ensure that the load table is organized by increasing date
  values <- values[order(values$date,decreasing=FALSE,na.last=TRUE),]

  # Create two vectors that will be filled with daily seizure load and seizure number values
  seizure.load.per.day <- rep(NA,dim(values)[1])
  seizure.number.per.day <- rep(NA,dim(values)[1])

  print("Now calculating daily seizure loads. Please wait...")

  # For each day in the load table, calculate the sum of the load and number values by day
  # List this sum in the first row of each day. Any other rows will have an NA value
  k <- 0
  for (i in 1:(dim(values)[1])) {
    if (k > 0) {
      k <- k - 1
    } else {
      k <- 0
      seizure.load.per.day[i] <- values[i,9]
      seizure.number.per.day[i] <- values[i,8]
      if (i != dim(values)[1]) {
        for (j in 1:(dim(values)[1]-i)) {
          if (values[(i+j),1] == values[i,1]) {
            k <- k + 1
          } else if (values[(i+j),1] != values[i,1]) {
            break
          }
        }
      }
      if (k > 0) {
        for (m in 1:k) {
          seizure.load.per.day[i] <- as.numeric(seizure.load.per.day[i]) + as.numeric(values[(i+m),9])
          seizure.number.per.day[i] <- as.numeric(seizure.number.per.day[i]) + as.numeric(values[(i+m),8])
        }
      }
    }
  }

  # Add the seizure load and seizure number per day columns to the load table
  # Then ensure each column name is consistent with the names found in the data dictionary
  seizure_load <- data.frame(cbind(rep(mrnumber,length(seizure.load.per.day))),values,seizure.number.per.day,seizure.load.per.day)
  colnames(seizure_load)[1] <- "MRNUMBER"
  colnames(seizure_load)[2] <- "DATE"
  colnames(seizure_load)[3] <- "DAY_TYPE"
  colnames(seizure_load)[4] <- "SEIZURE_SEVERITY"
  colnames(seizure_load)[5] <- "SEIZURE_LENGTH"
  colnames(seizure_load)[6] <- "SEIZURE_TYPE"
  colnames(seizure_load)[7] <- "SEIZURE_VARIABLES"
  colnames(seizure_load)[8] <- "SEIZURE_CLUSTER"
  colnames(seizure_load)[9] <- "SEIZURE_NUMBER"
  colnames(seizure_load)[10] <- "SEIZURE_LOAD"
  colnames(seizure_load)[11] <- "SEIZURE_NUMBER_DAY"
  colnames(seizure_load)[12] <- "SEIZURE_LOAD_DAY"

  # Ensure that the load table is organized by increasing date
  seizure_load <- seizure_load[order(seizure_load$DATE),]

  # Check to see if number of rows for missing days in the source table differs from number of
  # such rows in load table. If so, pad the source table with the appropriate number of rows
  # in the appropriate locations so that the two tables can be combined
  if (dim(savedata[savedata$DATA_QUALITY_S==3,])[1] != dim(seizure_load[seizure_load$DATE %in% savedata[savedata$DATA_QUALITY_S==3,colnames(savedata)=="DATE"],])[1]) {
    for (j in unique(savedata[savedata$DATA_QUALITY_S==3,colnames(savedata)=="DATE"])) {
      if (dim(seizure_load[seizure_load$DATE==j,])[1] > 1) {
        numrowsload <- nrow(seizure_load[seizure_load$DATE==j,])
        newrows <- data.frame(mrnumber,savedata[savedata$DATE==j,colnames(savedata)=="DATE"][1],savedata[savedata$DATE==j,colnames(savedata)=="DAY_TYPE"][1],3,NA,NA,NA,NA,NA,NA)
        newrows <- as.data.frame(lapply(newrows,rep,numrowsload))
        colnames(newrows) <- colnames(savedata)
        savedata <- rbind.data.frame(savedata[savedata$DATE<j,],newrows,savedata[savedata$DATE>j,])
      }
    }
  }
  
  comments <- rep(NA,dim(seizure_load)[1]) 
  seizure_load <- cbind.data.frame(seizure_load,comments)
  colnames(seizure_load)[dim(seizure_load)[2]] <- "COMMENTS"
  for (i in unique(seizure_load$DATE)) {
    if (length(data[data$DATE==i,c("COMMENTS")])>0) {
      seizure_load[seizure_load$DATE==i,c("comments")] <- data[data$DATE==i,c("COMMENTS")]
    }
  }

  # Name the columns of the temp load file
  # The "load" object will be what is saved to the patient folder if you wish to view the
  # FILA_SEIZURE_LOAD file.
  # This object differs from the seizure_load object in that the seizure_load object is what
  # will be passed on to future functions for creation of FILA_SEIZURE_DATA_CLINICAL and for
  # insertion of data into the database
  load <- data.frame(savedata,seizure_load[,c(4,5,6,7,8,9,10)])
  colnames(load)[4] <- "DAY_QUALITY_S"
  colnames(load)[11] <- "SEIZURE_SEVERITY_RANK"
  colnames(load)[12] <- "SEIZURE_LENGTH_RANK"
  colnames(load)[13] <- "SEIZURE_TYPE_RANK"
  colnames(load)[14] <- "SEIZURE_VARIABLE_RANK"
  colnames(load)[15] <- "SEIZURE_CLUSTER_RANK"
  colnames(load)[16] <- "SEIZURE_NUMBER_SEIZURE"
  colnames(load)[17] <- "SEIZURE_LOAD_SEIZURE"

  observe_load <- FALSE
  print("Would you like to save a temporary file to look at the seizure loads?")
  print("Type 'YES' to save a file to look at, type 'NO' to move onto next step")
  rl <- " "
  while (tolower(rl)!="yes" && tolower(rl)!="no") {
    rl <- readline(prompt="Enter here: ")
  }
  if (tolower(rl)=="yes") {
    observe_load <- TRUE
  }
  if (observe_load == TRUE) {
    print("Type 'yes' if you wish to save the SEIZURE_LOAD file in the same folder as this patient's SEIZURE_RANKING_SOURCE table. Type 'no' if it is in a different folder")
    answer <- ""
    while(tolower(answer)!="yes" & tolower(answer)!="no") {
      answer <- readline(prompt="Enter here: ")
    }
    if (answer=="no") {
      print("Input the directory that you wish to save this patient's SEIZURE_LOAD file in")
      print("Example: C:/Folder_Name/")
      directory <- readline(prompt="Enter here: ")
      setwd(directory)
    }
    print(paste("Saving seizure load table as",gsub(" ","",paste(patient,"_SEIZURE_LOAD.xlsx")),"in directory",getwd()))
    xlsx <- "SEIZURE_LOAD.xlsx"
    xlsx <- gsub(" ","",paste(patient,"_",xlsx))
    xlsx::write.xlsx2(load,file=xlsx,showNA=FALSE,row.names=FALSE)
    print("Type 'OKAY' whenever you are ready to move on to the next step")
    print("Or type 'QUIT' if you would like to exit")
    while (tolower(rl)!="okay" && tolower(rl)!="quit") {
      rl <- readline(prompt="Enter here: ")
    }
  } else {
    rl <- "okay"
  }
  if (tolower(rl)=="okay") {
    calculate_seizure_response(patient,data,seizure_load,load,ranking)
  }
}
