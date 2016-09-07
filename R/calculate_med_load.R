#' Calculate med load
#'
#' This function allow you to start the process of med load/response calculations
#' @keywords med load
#' @export
#' @importFrom lubridate interval
#' @importFrom lubridate dyears
#' @import RMySQL
#' @import ggplot2
#' @import openxlsx
#' @examples
#' calculate_seizure_load()

# Purpose - This script will take raw med data from the FILA_MED_DATA_SOURCE file, ranking data from
#           the MED_RANKING_SOURCE file, anthropometrics data from the FILA_ANTHROPOMETRICS_CLINICAL
#           file, and demographics data from the DEMOGRAPHICS_SOURCE file and calculate daily seizure loads
#           based on the ranking values and anthropometric/demographics data given for that patient
#           The script will give you as the user the option to specify the identifier signifying
#           the patient you wish to run this script for, and the directory where the two needed xlsx files
#           can be found
#           When the script finishes calculating daily seizure loads, you will be asked if you wish to save
#           a FILA_MED_LOAD file into the work directory for your viewing
calculate_med_load <- function() {

  ## Read in all relevant data from xlsx files
  print("Input the identifier that signify the patient we are doing calculations for")
  print("Example: FILA")
  patient <- readline(prompt="Enter here: ")

  print("Input the directory that you wish to draw this patient's DEMOGRAPHICS_SOURCE file from")
  print("Example: C:/Folder_Name/")
  directory <- readline(prompt="Enter here: ")
  setwd(directory)

  demo <- "DEMOGRAPHICS_SOURCE.xlsx"
  demo <- read.xlsx(demo,sheet=1,detectDates=TRUE)

  print("Type 'yes' if the MED_RANKING_SOURCE file can be found in the same folder as the DEMOGRAPHICS_SOURCE table. Type 'no' if it is in a different folder")
  answer <- ""
  while(tolower(answer)!="yes" & tolower(answer)!="no") {
    answer <- readline(prompt="Enter here: ")
  }
  if (answer=="no") {
    print("Input the directory that you wish to draw the MED_RANKING_SOURCE file from")
    print("Example: C:/Folder_Name/")
    directory <- readline(prompt="Enter here: ")
    setwd(directory)
  }

  ranking <- "MED_RANKING_SOURCE.xlsx"
  ranking <- read.xlsx(ranking,sheet=1,detectDates=TRUE)

  print("Type 'yes' if this patient's ANTHROPOMETRICS_SOURCE file can be found in the same folder as the MED_RANKING_SOURCE table. Type 'no' if it is in a different folder")
  answer <- ""
  while(tolower(answer)!="yes" & tolower(answer)!="no") {
    answer <- readline(prompt="Enter here: ")
  }
  if (answer=="no") {
    print("Input the directory that you wish to draw this patient's ANTHROPOMETRICS_SOURCE file from")
    print("Example: C:/Folder_Name/")
    directory <- readline(prompt="Enter here: ")
    setwd(directory)
  }

  anthro <- "ANTHROPOMETRICS_SOURCE.xlsx"
  anthro <- gsub(" ","",paste(patient,"_",anthro))
  anthro <- read.xlsx(anthro,sheet=1,detectDates=TRUE)
  
  print("Type 'yes' if this patient's MED_DATA_SOURCE file can be found in the same folder as this patient's ANTHROPOMETRICS_SOURCE table. Type 'no' if it is in a different folder")
  answer <- ""
  while(tolower(answer)!="yes" & tolower(answer)!="no") {
    answer <- readline(prompt="Enter here: ")
  }
  if (answer=="no") {
    print("Input the directory that you wish to draw this patient's MED_DATA_SOURCE file from")
    print("Example: C:/Folder_Name/")
    directory <- readline(prompt="Enter here: ")
    setwd(directory)
  }
  
  data <- "MED_DATA_SOURCE.xlsx"
  data <- gsub(" ","",paste(patient,"_",data))
  data <- read.xlsx(data,sheet=1,detectDates=TRUE)

  ## Remove all empty rows from each data frame
  data <- data[!is.na(data$MRNUMBER),]
  anthro <- anthro[!is.na(anthro$MRNUMBER),]
  demo <- demo[!is.na(demo$LAST),]
  ranking <- ranking[!is.na(ranking$MED_GENERIC_NAME),]

  print("Calculating med intake in mg/kg/day, please wait...")

  ## Save patient's medical record number and birthdate as R objects
  mrnumber <- unique(anthro$MRNUMBER)
  birthdate <- demo$DOB[demo$MRNUMBER==mrnumber]
  birthdate <- as.Date(birthdate,format="%m/%d/%Y")
  
  first <- unique(demo[demo$MRNUMBER==mrnumber,colnames(demo)=="FIRST"])
  last <- unique(demo[demo$MRNUMBER==mrnumber,colnames(demo)=="LAST"])

  ## Ensure that date is formatted properly (removing time stamp if necessary)
  for (i in 1:length(data$DATE)) {
    if (nchar(as.character(data$DATE[i]))>10) {
      data$DATE <- substr(data$DATE,1,nchar(as.character(data$DATE))-5)
    }
  }
  ## Split raw data table into two parts: one where day type = 3, and one where day type != 3
  data$DATE <- as.Date(data$DATE,format="%m/%d/%Y")
  sub <- data[data$DAY_TYPE==3 & data$DATE <= data[data$DAY_TYPE!=3,colnames(data)=="DATE"][1],]
  data <- data[!(data$DATE %in% sub$DATE),]

  ## Save relevant columns of data table as object 'med_dose'
  med_dose <- data[,colnames(data)=="DATE" | colnames(data)=="MED_ID" | colnames(data)=="DAILY_MED_DOSE_MG"]

  ## Obtain interpolated weights for the anthropometrics source table
  anthro <- weight_interp(anthro)
  
  anthro$DATE <- as.Date(anthro$DATE,format="%m/%d/%Y")
  last_date <- as.Date(max(max(unique(anthro[,colnames(anthro)=="DATE"])),max(unique(med_dose[,1]))))

  ## Create table with start and end dates for each period of specific med usage and dosage
  END_DATE <- data.frame(rep(unique(med_dose$DATE)[2]-1,length(med_dose[med_dose$DATE==unique(med_dose$DATE)[2-1],2])))
  colnames(END_DATE) <- "END_DATE"
  if (length(unique(med_dose$DATE))>=3) {
    for (i in 3:(length(unique(med_dose$DATE)))) {
      end_date <- data.frame(rep(unique(med_dose$DATE)[i]-1,length(med_dose[med_dose$DATE==unique(med_dose$DATE)[i-1],1])))
      colnames(end_date) <- "END_DATE"
      END_DATE <- data.frame(rbind(END_DATE,end_date))
      colnames(END_DATE) <- "END_DATE"
    }
  }
  na <- cbind(rep(NA,length(med_dose[med_dose$DATE==unique(med_dose$DATE)[length(unique(med_dose$DATE))],colnames(med_dose)=="DATE"])))
  colnames(na) <- "END_DATE"
  END_DATE <- data.frame(rbind(END_DATE,na))
  med_dose <- cbind(med_dose$DATE,END_DATE,med_dose[,colnames(med_dose)!="DATE"])
  colnames(med_dose)[1] <- "START_DATE"

  ## Create table with start and end dates for each period of specific weight
  anthro <- data.frame(anthro,birthdate)
  colnames(anthro)[dim(anthro)[2]] <- "BIRTHDATE"

  ## Create table 'med_intake' in which med intake per med per day will be calculated based on weight and dosage
  med_intake <- data.frame(start.date=as.Date(character()),med.id=character(),
                           dosage=integer(),weight=integer(),med.intake=integer())
  temp_date <- med_dose$START_DATE[1]

  for (i in 1:((as.integer(last_date-med_dose$START_DATE[1]))+1)) {
    compare <- max(unique(med_dose$START_DATE)[unique(med_dose$START_DATE)<=temp_date])
    temp_med_intake <- data.frame(temp_date,med_dose[med_dose$START_DATE==compare,colnames(med_dose)=="MED_ID" | colnames(med_dose)=="DAILY_MED_DOSE_MG"],NA,NA)
    colnames(temp_med_intake) <- c("DATE","MED_ID","DOSAGE","WEIGHT","MED_INTAKE")
    med_intake <- data.frame(rbind(med_intake,temp_med_intake))
    if (temp_date %in% anthro$DATE) {
      weight <- anthro[anthro$DATE==temp_date,colnames(anthro)=="WT_DAY"]
    } else {
      weight <- anthro[anthro$DATE==max(anthro$DATE),colnames(anthro)=="WT_DAY"]
    }
    med_intake[med_intake$DATE==temp_date,colnames(med_intake)=="WEIGHT"] <- weight
    med_intake[med_intake$DATE==temp_date,colnames(med_intake)=="MED_INTAKE"] <- med_intake[med_intake$DATE==temp_date,colnames(med_intake)=="DOSAGE"]/med_intake[med_intake$DATE==temp_date,colnames(med_intake)=="WEIGHT"]
    temp_date <- med_dose$START_DATE[1]+i
  }

  print("Calculating minimum dose in mg/kg/day, please wait...")

  ## Denominator: Minimum dose in mg/kg/day

  ## Create table 'med_min_dose' where age per day (rounded to the nearest two decimal places) is
  ## calculated, and then used to determine the lower med limit for each med each day (with the exception)
  ## of med with MID0003, where weight is used instead of age)
  med_min_dose <- data.frame(med_intake[,colnames(med_intake)!="MED_INTAKE"],AGE=NA,MIN_DOSE=NA)

  temp_date <- med_dose$START_DATE[1]
  for (i in 1:(as.integer(last_date-med_dose[1,1])+1)) {
    duration <- interval(birthdate,temp_date)
    med_min_dose[med_min_dose$DATE==temp_date,colnames(med_min_dose)=="AGE"] <- round(duration/dyears(1),2)

    for (j in med_min_dose[med_min_dose$DATE==temp_date,colnames(med_min_dose)=="MED_ID"]) {
      compare <- unique(med_min_dose[med_min_dose$DATE==temp_date,colnames(med_min_dose)=="AGE"])
      if (j == "MID0003") {
        compare <- unique(med_min_dose[med_min_dose$DATE==temp_date,colnames(med_min_dose)=="WEIGHT"])
      }
      for (k in 1:(length(ranking[ranking$MED_ID==j,colnames(ranking)=="MED_ID"]))) {
        if ((compare >= ranking[ranking$MED_ID==j,][k,colnames(ranking)=="MED_LIMIT_LOW"]) && (compare < ranking[ranking$MED_ID==j,][k,colnames(ranking)=="MED_LIMIT_HIGH"])) {
          med_min_dose[med_min_dose$DATE==temp_date & med_min_dose$MED_ID==j,colnames(med_min_dose)=="MIN_DOSE"] <- ranking[ranking$MED_ID==j,][k,colnames(ranking)=="MED_MIN_DOSE"]
          break
        }
      }
    }

    temp_date <- temp_date+1
  }

  print("Calculating med load per day, please wait...")

  med_min_dose[,colnames(med_min_dose)=="WEIGHT"] <- round(med_min_dose$WEIGHT,2)

  ## Use numerator and denominator to calculate the med load
  ## Calculate med load per med and and total med load per day
  med_load <- data.frame(med_min_dose[,colnames(med_min_dose)!="MIN_DOSE"],med_intake[,colnames(med_intake)=="MED_INTAKE"],med_min_dose[,colnames(med_min_dose)=="MIN_DOSE"],MED_LOAD_PER_MED=NA,MED_LOAD_DAY=NA)
  colnames(med_load)[c(6,7)] <- c("MED_INTAKE","MIN_DOSE")
  med_load$MED_LOAD_PER_MED <- round(med_load$MED_INTAKE/med_load$MIN_DOSE,4)
  med_load[,colnames(med_load)=="MED_INTAKE"] <- round(med_load$MED_INTAKE,4)
  for (i in unique(med_load$DATE)) {
    med_load[med_load$DATE==i,colnames(med_load)=="MED_LOAD_DAY"][1] <- sum(med_load[med_load$DATE==i,colnames(med_load)=="MED_LOAD_PER_MED"])
  }

  ## Calculate total med number per day and display it in the first row of each observed date in the column MED_NUMBER_DAY
  med.number.per.day <- rep(NA,dim(med_load)[1])
  med_load <- data.frame(med_load,med.number.per.day)
  colnames(med_load)[10] <- "MED_NUMBER_DAY"
  for (i in unique(med_load[,1])) {
    med_load[med_load$DATE==i,colnames(med_load)=="MED_NUMBER_DAY"][1] <- length(med_load[med_load$DATE==i & med_load[,colnames(med_load)=="MED_INTAKE"]!=0,colnames(med_load)=="MED_INTAKE"])
  }

  med_load <- data.frame(mrnumber,med_load)
  colnames(med_load)[1] <- "MRNUMBER"

  ## Create day type column and determine the range of dates each row of the med load table falls into within the
  ## raw data table to determine which value to put in this row in column DAY_TYPE
  med_load$DATE <- as.Date(med_load$DATE,format="%m/%d/%Y")
  na <- rep(NA,dim(med_load)[1])
  med_load <- data.frame(med_load[,1:2],na,med_load[,3:11])
  colnames(med_load)[3] <- "DAY_TYPE"
  for (i in 1:dim(med_load)[1]) {
    med_load$DAY_TYPE[i] <- unique(data[data$DATE==max(unique(data$DATE[data$DATE<=med_load$DATE[i]])),colnames(data)=="DAY_TYPE"])
  }

  ## Calculate daily med dose per milligram per kilogram, and then create the two versions of the med load
  ## table: a temp version for your viewing, and one that will be stored in the MySQL database
  daily.dosage <- data.frame(DATE=as.Date(as.character()),DAILY_MED_DOSE_MG=integer(),DAILY_MED_DOSE_MG_KG=integer())
  daily.dosage[1:dim(med_load)[1],c("DATE")] <- med_load$DATE
  daily.dosage$DAILY_MED_DOSE_MG <- med_load$DOSAGE[med_load$DATE==daily.dosage$DATE]

  daily.dosage$DAILY_MED_DOSE_MG_KG <- round(daily.dosage$DAILY_MED_DOSE_MG/(med_load$WEIGHT[med_load$DATE==daily.dosage$DATE]),4)
  load <- data.frame(med_load[,1:3],med_load[,4],daily.dosage[,2:3],med_load[,9:10])
  colnames(load) <- c("MRNUMBER","DATE","DAY_TYPE","MED_ID","DAILY_MED_DOSE_MG","DAILY_MED_DOSE_MG_KG","MED_MIN_DOSE","MED_LOAD_MED")
  colnames(med_load) <- c("MRNUMBER","DATE","DAY_TYPE","MED_ID","DAILY_MED_DOSE_MG","WT","AGE","DAILY_MED_DOSE_MG_KG","MED_MIN_DOSE","MED_LOAD_MED","MED_LOAD_DAY","MED_NUMBER_DAY")

  ## If there are any days in the raw data table with day type = 3, add those rows to the med load table.
  ## These observations will be found in the MySQL database
  if (dim(sub)[1] > 0) {
    sub.med_load <- data.frame(sub[,colnames(sub) %in% c("MRNUMBER","DATE","DAY_TYPE")],
                               sub[,colnames(sub) %in% c("MED_ID","DAILY_MED_DOSE_MG")],
                               NA,NA,NA,NA,NA,NA,NA)
    colnames(sub.med_load) <- colnames(med_load)
    sub.med_load$AGE <- round((sub.med_load$DATE-birthdate)/dyears(1),2)
    for (i in 1:dim(sub.med_load)[1]) {
      if (sub.med_load$MED_ID[i] != "MID0003") {
        sub.med_load$MED_MIN_DOSE[i] <- ranking[ranking$MED_ID==sub.med_load$MED_ID[i]
                                                & sub.med_load$AGE[i] >= ranking$MED_LIMIT_LOW
                                                & sub.med_load$AGE[i] <= ranking$MED_LIMIT_HIGH,
                                                colnames(ranking)=="MED_MIN_DOSE"]
      }
    }
    med_load <- rbind.data.frame(sub.med_load,med_load)

    sub.load <- sub.med_load[,colnames(sub.med_load) %in% c("MRNUMBER","DATE","DAY_TYPE",
                                                            "MED_ID","DAILY_MED_DOSE_MG",
                                                            "DAILY_MED_DOSE_MG_KG","MED_MIN_DOSE",
                                                            "MED_LOAD_MED")]
    colnames(sub.load) <- colnames(load)
    load <- rbind.data.frame(sub.load,load)
  }
  
  comments <- rep(NA,dim(med_load)[1]) 
  med_load <- cbind.data.frame(med_load,comments)
  colnames(med_load)[dim(med_load)[2]] <- "COMMENTS"
  for (i in unique(med_load$DATE)) {
    if (length(data[data$DATE==i,c("COMMENTS")])>0) {
      med_load[med_load$DATE==i,c("COMMENTS")] <- data[data$DATE==i,c("COMMENTS")]
    }
  }

  observe_load <- FALSE
  print("Would you like to save a temporary file to look at the med loads?")
  print("Type 'YES' to save a file to look at, type 'NO' to move onto next step")
  rl <- " "
  while (tolower(rl)!="yes" && tolower(rl)!="no") {
    rl <- readline(prompt="Enter here: ")
  }
  if (tolower(rl)=="yes") {
    observe_load <- TRUE
  }
  if (observe_load == TRUE) {
    print("Type 'yes' if you wish to save the MED_LOAD file in the same folder as this patient's MED_DATA_SOURCE table. Type 'no' if it is in a different folder")
    answer <- ""
    while(tolower(answer)!="yes" & tolower(answer)!="no") {
      answer <- readline(prompt="Enter here: ")
    }
    if (answer=="no") {
      print("Input the directory that you wish to save this patient's MED_LOAD file in")
      print("Example: C:/Folder_Name/")
      directory <- readline(prompt="Enter here: ")
      setwd(directory)
    }
    print(paste("Saving med load table as",gsub(" ","",paste(patient,"_MED_LOAD.xlsx")),"in directory",getwd()))
    xlsx <- "MED_LOAD.xlsx"
    xlsx <- gsub(" ","",paste(patient,"_",xlsx))
    xlsx::write.xlsx2(med_load,file=xlsx,showNA=FALSE,row.names=FALSE)
    print("Type 'OKAY' whenever you are ready to move on to the next step")
    print("Or type 'QUIT' if you would like to exit")
    while (tolower(rl)!="okay" && tolower(rl)!="quit") {
      rl <- readline(prompt="Enter here: ")
    }
  } else {
    rl <- "okay"
  }
  if (tolower(rl)=="okay") {
    calculate_med_response(patient,data,med_load,load,first,last)
  }
}
