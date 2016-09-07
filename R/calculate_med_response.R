#' Calculate med response
#'
#' This function allow you to calculate med responses (i.e. med response, % med free, med score, etc.).
#' You can upload this data to a MySQL database if you choose.
#' @param patient A patient identifier
#' @param data The med raw data table
#' @param y The med load table
#' @param calculated The med load table including baseline days (to be uploaded to the MySQL database)
#' @param first Patient's first name
#' @param last Patient's last name
#' @keywords med response

# Purpose - This script will calculate values for percent med free (on baseline days
#           and therapy days), med response (daily and for every 30 days), percent med
#           free response, med score, med number response (daily and for every 30 days),
#           and med number score. The results will be used to create the
#           FILA_MED_DATA_CLINICAL.xlsx file and a subset will also be placed into the MySQL
#           database if you grant permission
calculate_med_response <- function(patient,data,y,calculated,first,last) {

  # Read in the seizure data source file, to be used to determine the last day that should be used in
  # calculation of the med response values
  string <- gsub(" ","",paste(patient,"_","SEIZURE_DATA_SOURCE.xlsx"))
  datecheck <- read.xlsx(string,sheet=1,detectDates=TRUE)
  datecheck <- subset(datecheck,!is.na(datecheck$DATE))
  datecheck$DATE <- as.Date(datecheck$DATE,format="%m/%d/%Y")

  # Remove all rows of the med load table that do not have observed values in the MED_LOAD_DAY
  # column (i.e. we only want to have one row for each day)
  y <- subset(y,!is.na(y$MED_LOAD_DAY))
  y$DATE <- as.Date(y$DATE,format="%m/%d/%Y")
  data <- subset(data,!is.na(data$DATE))
  data$DATE <- as.Date(data$DATE,format="%m/%d/%Y")

  # Compare the last day found in the seizure raw data table with the last day found in the med load
  # table. In either possible case, the last day of the med load table will be changed to match the last
  # day of the seizure raw data table. If the first is later than the second, then we will add rows into
  # the med load table with values equivalent to those in the last observed row in the med load table.
  # Otherwise, we will remove the appropriate number of rows from the med table
  if ((max(unique(datecheck$DATE))) > (max(unique(y$DATE)))) {
    temp <- data.frame(y[y$DATE==max(unique(y$DATE)),])
    temp$DATE <- max(unique(y$DATE))+1
    if ((max(unique(datecheck$DATE)))-(max(unique(y$DATE)))>=2) {
      for (i in 2:((max(unique(datecheck$DATE))) - (max(unique(y$DATE))))) {
        if (((max(unique(datecheck$DATE))) - (max(unique(y$DATE)))) == 0) {
          break
        }
        t <- data.frame(y[y$DATE==max(unique(y$DATE)),])
        t$DATE <- max(unique(y$DATE))+i
        temp <- rbind.data.frame(temp,t)
      }
    }
    y <- rbind.data.frame(y,temp)
  } else if ((max(unique(datecheck$DATE))) < (max(unique(y$DATE)))) {
    temp <- y[y$DATE <= max(unique(datecheck$DATE)),]
    rm(y)
    y <- temp
  }

  # Split the med load table into two subsets: baseline days and therapy days
  med.baseline <- subset(y,y$DAY_TYPE==1)
  med.therapy <- subset(y,y$DAY_TYPE!=1)

  # Create objects for the patient's medical record number, as well as their
  # DAY_TYPE, MED_NUMBER_DAY, and MED_LOAD_DAY columns
  daytype <- med.therapy$DAY_TYPE
  mednumber <- y[!is.na(y$MED_NUMBER_DAY) & (y$DATE %in% med.therapy$DATE),colnames(y)=="MED_NUMBER_DAY" | colnames(y)=="MED_LOAD_DAY"]
  mrnumber <- unique(data$MRNUMBER)
  y <- y[,-(colnames(y)=="MRNUMBER")]

  # Call to function that will compute the response and score values for each day or each period of 30 days
  if (dim(med.baseline[med.baseline$MED_NUMBER_DAY>0,])[1]>0) {
    results <- med_calculate(y,which(colnames(med.therapy)=="MED_NUMBER_DAY"),which(colnames(med.therapy)=="MED_LOAD_DAY"),med.baseline,med.therapy,patient,mrnumber,daytype,mednumber)
    comments <- rep(NA,dim(results)[1]) 
    results <- cbind.data.frame(results,comments)
    colnames(results)[dim(results)[2]] <- "COMMENTS"
    for (i in unique(results$DATE)) {
      if (length(data[data$DATE==i,c("COMMENTS")])>0) {
        results[results$DATE==i,c("COMMENTS")] <- data[data$DATE==i,c("COMMENTS")][1]
      }
    }
    print("Responses and scores have been calculated")
  } else {
    print("Baseline is zero, no med response values can be calculated")
  }

  if ((dim(med.baseline[med.baseline$MED_NUMBER_DAY>0,])[1]>0)) {
    
    c <- getwd()
    
    print("Type 'yes' if you wish to save all graphs in the same folder as this patient's MED_LOAD file. Type 'no' if you would like for them to be in a different folder")
    answer <- ""
    while(tolower(answer)!="yes" & tolower(answer)!="no") {
      answer <- readline(prompt="Enter here: ")
    }
    if (answer=="no") {
      print("Input the directory that you wish to save this patient's med graphs in")
      print("Example: C:/Folder_Name/")
      directory <- readline(prompt="Enter here: ")
      setwd(directory)
    }
    
    results$DATE <- as.Date(results$DATE)
    string <- paste(first,gsub(" ","",paste(last,":")),"Daily Med Response")
    len <- ceiling((results$DATE[length(results$DATE)] - results$DATE[1])/4)
    base <- ggplot(results,aes(results$DATE,MED_RESPONSE_DAY)) 
    base <- base + geom_hline(yintercept=100) 
    base <- base + geom_point(aes(colour=factor(results$DAY_TYPE)),size=0.5) + geom_line(aes(DATE,MED_RESPONSE_DAY,colour=factor(results$DAY_TYPE)),size=0.1)
    base <- base + scale_x_date(limits=c(results$DATE[1],results$DATE[length(results$DATE)]),
                                breaks=seq(results$DATE[1],results$DATE[length(results$DATE)],length.out=8),
                                date_labels="%m/%d/%Y") 
    base <- base + ggtitle(string) + xlab("Date on Therapy") + ylab("Daily Med Response (%)")
    base <- base + scale_y_continuous(limits=c(0,ceiling(max(results$MED_RESPONSE_DAY)/50)*50),
                                      breaks=seq(0,ceiling(max(results$MED_RESPONSE_DAY)/50)*50,
                                                 length.out=ceiling(max(results$MED_RESPONSE_DAY)/50)+1))
    base <- base + theme(panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank())
    base <- base + theme(axis.line.x=element_line(size=0.5,colour="black"),axis.line.y=element_line(size=0.5,colour="black"))
    base <- base + theme(axis.text.x=element_text(size=5))
    base <- base + scale_color_manual(name="",labels=c("Daily Med Response"),values=c("brown"))
    file <- gsub(" ","",paste(patient,"_MED_DAILY_GRAPH.jpeg"))
    ggsave(base,filename=file,width=6,height=4)
    print(paste(gsub(" ","",paste(patient,"_MED_DAILY_GRAPH.jpeg")),"created and saved"))
    
    temp <- results[!(is.na(results$MED_SCORE_30_DAYS)),]
    temp$DAY_TYPE <- "2"
    temp$DAY_TYPE <- factor(temp$DAY_TYPE,levels=c("2"))
    if (length(results$MED_SCORE_30_DAYS)>0) {
      string <- paste("Med Score per 30 days:",first,last)
      plotscore <- ggplot(temp,aes(x=DATE,y=MED_SCORE_30_DAYS,colour=DAY_TYPE,fill=DAY_TYPE))
      plotscore <- plotscore + geom_hline(yintercept=100)
      plotscore <- plotscore + geom_line(aes(DATE,MED_SCORE_30_DAYS,colour=factor(temp$DAY_TYPE)))
      plotscore <- plotscore + geom_point(aes(colour=factor(temp$DAY_TYPE),fill=factor(temp$DAY_TYPE)),shape=24)
      plotscore <- plotscore + xlab("Date on Therapy") + ylab("Score (%)") + ggtitle(string)
      plotscore <- plotscore + theme(axis.title=element_text(angle=0,size=10))
      plotscore <- plotscore + theme(plot.title=element_text(angle=0,size=10))
      plotscore <- plotscore + theme(legend.title=element_text(size=14,face="bold"))
      plotscore <- plotscore + scale_fill_manual(name="",labels=c("Med Score"),values=c("black"))
      plotscore <- plotscore + scale_color_manual(name="",labels=c("Med Score"),values=c("red"))
      plotscore <- plotscore + theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())
      plotscore <- plotscore + theme(panel.border=element_rect(colour="black",fill=NA,size=1.5))
      plotscore <- plotscore + theme(axis.text.x=element_text(size=6))
      plotscore <- plotscore + scale_x_date(limits=c(temp$DATE[1],temp$DATE[length(temp$DATE)]), 
                                            breaks=seq(temp$DATE[1],temp$DATE[length(temp$DATE)],length.out=8),
                                            date_labels="%m/%d/%Y")
      plotscore <- plotscore + scale_y_continuous(limits=c(0,ceiling(max(temp$MED_SCORE_30_DAYS)/50)*50),
                                                  breaks=seq(0,ceiling(max(temp$MED_SCORE_30_DAYS)/50)*50,
                                                             length.out=ceiling(max(temp$MED_SCORE_30_DAYS)/50)+1))
      file <- gsub(" ","",paste(patient,"_MED_SCORE_GRAPH.jpeg"))
      ggsave(plotscore,filename=file,width=6,height=4)
      
      print(paste(gsub(" ","",paste(patient,"_MED_SCORE_GRAPH.jpeg")),"created and saved"))
    }
  } else {
    results <- data.frame(med.therapy[,colnames(med.therapy) %in% c("MRNUMBER","DATE","DAY_TYPE","MED_LOAD_DAY","MED_NUMBER_DAY")])
    results <- cbind.data.frame(results,MED_RESPONSE_DAY=NA,MED_NUMBER_RESPONSE_DAY=NA,'%_MED_FREE_30_DAYS'=NA,
                                MED_RESPONSE_30_DAYS=NA,'%_MED_FREE_RESPONSE_30_DAYS'=NA,MED_SCORE_30_DAYS=NA)
  }
  
  print("Type 'yes' if you wish to save this patient's MED_DATA_CLINICAL file in the same folder as this patient's MED_LOAD file. Type 'no' if you would like for it to be in a different folder")
  answer <- ""
  while(tolower(answer)!="yes" & tolower(answer)!="no") {
    answer <- readline(prompt="Enter here: ")
  }
  if (answer=="no") {
    print("Input the directory that you wish to save this patient's MED_DATA_CLINICAL file in")
    print("Example: C:/Folder_Name/")
    directory <- readline(prompt="Enter here: ")
    setwd(directory)
  }
  
  print(paste("Saving med clinical outcome table as",gsub(" ","",paste(patient,"_MED_DATA_CLINICAL.xlsx")),"in directory",getwd()))
  xlsx <- "MED_DATA_CLINICAL.xlsx"
  xlsx <- gsub(" ","",paste(patient,"_",xlsx))
  xlsx::write.xlsx2(results,file=xlsx,showNA=FALSE,row.names=FALSE)
}
