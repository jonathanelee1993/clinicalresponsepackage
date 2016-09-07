#' Calculate seizure response
#'
#' This function allow you to calculate seizure responses (i.e. seizure response, % seizure free, seizure score, etc.).
#' You can upload this data to a MySQL database if you choose.
#' @param patient A patient identifier
#' @param data The seizure raw data table
#' @param x The seizure load table
#' @param sourcedata The seizure load table including baseline days (to be uploaded to the MySQL database)
#' @param ranking The seizure ranking table
#' @keywords seizure response

# Purpose - This script will calculate values for percent seizure free (on baseline days
#           and therapy days), seizure response (daily and for every 30 days), percent seizure
#           free response, seizure score, seizure number response (daily and for every 30 days),
#           and seizure number score. The results will be used to create the
#           FILA_SEIZURE_DATA_CLINICAL.xlsx file
calculate_seizure_response <- function(patient,data,x,sourcedata,ranking) {

  # Remove all rows of the seizure load table that do not have observed values in the SEIZURE_NUMBER_DAY
  # column (i.e. we only want to have one row for each day)
  x <- subset(x,!is.na(x$SEIZURE_NUMBER_DAY))
  x <- x[,colnames(x)=="MRNUMBER" | colnames(x)=="DATE" | colnames(x)=="DAY_TYPE" | colnames(x)=="SEIZURE_SEVERITY" | colnames(x)=="SEIZURE_LENGTH" | colnames(x)=="SEIZURE_TYPE" | colnames(x)=="SEIZURE_VARIABLE" | colnames(x)=="SEIZURE_CLUSTER" | colnames(x)=="SEIZURE_NUMBER" | colnames(x)=="SEIZURE_LOAD" | colnames(x)=="SEIZURE_NUMBER_DAY" | colnames(x)=="SEIZURE_LOAD_DAY"]
  x$DATE <- as.Date(x$DATE)

  # Create two subsets of the load table: one for days on baseline and the other for days on therapy
  seizure.baseline <- subset(x,x$DAY_TYPE==1)
  seizure.therapy <- subset(x,x$DAY_TYPE!=1)
  # Create objects for the patient's medical record number, DAY_TYPE column,
  # DATA_QUALITY_S column, and SEIZURE_NUMBER_DAY/SEIZURE_LOAD_DAY columns
  mrnumber <- unique(x$MRNUMBER)
  daytype <- seizure.therapy$DAY_TYPE
  seizurequality <- unique(data[as.Date(data$DATE) %in% as.Date(seizure.therapy$DATE),colnames(data)=="DATE" | colnames(data)=="DATA_QUALITY_S"])
  seizurenumber <- x[!is.na(x$SEIZURE_NUMBER_DAY) & x$DATE %in% seizure.therapy$DATE,colnames(x)=="SEIZURE_NUMBER_DAY" | colnames(x)=="SEIZURE_LOAD_DAY"]
  x <- x[,-(colnames(x)=="MRNUMBER")]

  # Call to function that will compute the response and score values for each day or each period of 30 days
  if (dim(seizure.therapy)[1]>0) {
    results <- seizure_calculate(x,which(colnames(seizure.therapy)=="SEIZURE_NUMBER_DAY"),which(colnames(seizure.therapy)=="SEIZURE_LOAD_DAY"),seizure.baseline,seizure.therapy,patient,mrnumber,daytype,seizurequality,seizurenumber)
    comments <- rep(NA,dim(results)[1]) 
    results <- cbind.data.frame(results,comments)
    colnames(results)[dim(results)[2]] <- "COMMENTS"
    for (i in unique(results$DATE)) {
      if (length(data[data$DATE==i,c("COMMENTS")])>0) {
        results[results$DATE==i,c("COMMENTS")] <- data[data$DATE==i,c("COMMENTS")][1]
      }
    }
    print("Response and scores have been calculated")
    
    # If there is a baseline day with seizure load greater than 0, create a graph of the daily seizure responses
    # and then save this graph in the patient folder
    if (max(sourcedata[sourcedata$DAY_TYPE==1,colnames(sourcedata)=="SEIZURE_LOAD_SEIZURE"])>0) {
      
      c <- getwd()
      
      # Input directory in which the DEMOGRAPHICS_SOURCE file can be found
      print("Input the directory that you wish to draw the DEMOGRAPHICS_SOURCE file from")
      print("Example: C:/Folder_Name/")
      directory <- readline(prompt="Enter here: ")
      setwd(directory)
      
      # Paste the patient string using paste and gsub functions to create the two file names
      # Then use read.xlsx() to read these files into R as data frames
      demo <- "DEMOGRAPHICS_SOURCE.xlsx"
      demo <- read.xlsx(demo,sheet=1,detectDates=TRUE)
      
      first <- unique(demo[demo$MRNUMBER==mrnumber,colnames(demo)=="FIRST"])
      last <- unique(demo[demo$MRNUMBER==mrnumber,colnames(demo)=="LAST"])
      
      setwd(c)
      
      print("Type 'yes' if you wish to save all graphs in the same folder as this patient's SEIZURE_LOAD file. Type 'no' if you would like for them to be in a different folder")
      answer <- ""
      while(tolower(answer)!="yes" & tolower(answer)!="no") {
        answer <- readline(prompt="Enter here: ")
      }
      if (answer=="no") {
        print("Input the directory that you wish to save this patient's seizure graphs in")
        print("Example: C:/Folder_Name/")
        directory <- readline(prompt="Enter here: ")
        setwd(directory)
      }
      
      results$DATE <- as.Date(results$DATE)
      string <- paste(first,gsub(" ","",paste(last,":")),"Daily Seizure Response")
      len <- ceiling((results$DATE[length(results$DATE)] - results$DATE[1])/4)
      base <- ggplot(results,aes(x=results$DATE,y=results$SEIZURE_RESPONSE_DAY)) 
      base <- base + geom_hline(yintercept=100)
      base <- base + geom_point(aes(colour=factor(results$DAY_TYPE)),size=0.5) + geom_line(aes(DATE,SEIZURE_RESPONSE_DAY,colour=factor(results$DAY_TYPE)),size=0.1)
      base <- base + scale_x_date(limits=c(results$DATE[1],results$DATE[length(results$DATE)]),
                                  breaks=seq(results$DATE[1],results$DATE[length(results$DATE)],length.out=8),
                                  date_labels="%m/%d/%Y") 
      base <- base + ggtitle(string) + xlab("Date on Therapy") + ylab("Daily Seizure Response (%)")
      base <- base + scale_y_continuous(limits=c(0,ceiling(max(results$SEIZURE_RESPONSE_DAY)/50)*50),
                                        breaks=seq(0,ceiling(max(results$SEIZURE_RESPONSE_DAY)/50)*50,
                                        length.out=ceiling(max(results$SEIZURE_RESPONSE_DAY)/50)+1))
      base <- base + theme(panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank())
      base <- base + theme(axis.line.x=element_line(size=0.5,colour="black"),axis.line.y=element_line(size=0.5,colour="black"))
      base <- base + theme(axis.text.x=element_text(size=5))
      base <- base + scale_color_manual(name="",labels=c("Daily Seizure Response"),values=c("brown"))
      file <- gsub(" ","",paste(patient,"_SEIZURE_DAILY_GRAPH.jpeg"))
      ggsave(base,filename=file,width=6,height=4)
      
      print(paste(gsub(" ","",paste(patient,"_SEIZURE_DAILY_GRAPH.jpeg")),"created and saved"))
      
      temp <- results[!(is.na(results$SEIZURE_SCORE_30_DAYS)),]
      temp$DAY_TYPE <- "2"
      temp$DAY_TYPE <- factor(temp$DAY_TYPE,levels=c("2"))
      if (length(results$SEIZURE_SCORE_30_DAYS)>0) {
        string <- paste("Seizure Score per 30 days:",first,last)
        plotscore <- ggplot(temp,aes(x=temp$DATE,y=temp$SEIZURE_SCORE_30_DAYS))
        plotscore <- plotscore + geom_hline(yintercept=100)
        plotscore <- plotscore + geom_line(aes(DATE,SEIZURE_SCORE_30_DAYS,colour=factor(temp$DAY_TYPE)))
        plotscore <- plotscore + geom_point(aes(colour=factor(temp$DAY_TYPE),fill=factor(temp$DAY_TYPE)),shape=24)
        plotscore <- plotscore + xlab("Date on Therapy") + ylab("Score (%)") + ggtitle(string)
        plotscore <- plotscore + theme(axis.title=element_text(angle=0,size=10))
        plotscore <- plotscore + theme(plot.title=element_text(angle=0,size=10))
        plotscore <- plotscore + theme(legend.title=element_text(size=14,face="bold"))
        plotscore <- plotscore + scale_fill_manual(name="",labels=c("Seizure Score"),values=c("black"))
        plotscore <- plotscore + scale_color_manual(name="",labels=c("Seizure Score"),values=c("blue"))
        plotscore <- plotscore + theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())
        plotscore <- plotscore + theme(panel.border=element_rect(colour="black",fill=NA,size=1.5))
        plotscore <- plotscore + theme(axis.text.x=element_text(size=6))
        plotscore <- plotscore + scale_x_date(limits=c(temp$DATE[1],temp$DATE[length(temp$DATE)]), 
                                              breaks=seq(temp$DATE[1],temp$DATE[length(temp$DATE)],length.out=8),
                                              date_labels="%m/%d/%Y")
        plotscore <- plotscore + scale_y_continuous(limits=c(0,ceiling(max(temp$SEIZURE_SCORE_30_DAYS)/50)*50),
                                                    breaks=seq(0,ceiling(max(temp$SEIZURE_SCORE_30_DAYS)/50)*50,
                                                               length.out=ceiling(max(temp$SEIZURE_SCORE_30_DAYS)/50)+1))
        file <- gsub(" ","",paste(patient,"_SEIZURE_SCORE_GRAPH.jpeg"))
        ggsave(plotscore,filename=file,width=6,height=4)
        
        print(paste(gsub(" ","",paste(patient,"_SEIZURE_SCORE_GRAPH.jpeg")),"created and saved"))
      }
      
      setwd(c)
      
      print("Type 'yes' if you wish to save this patient's SEIZURE_DATA_CLINICAL file in the same folder as this patient's SEIZURE_LOAD file. Type 'no' if you would like for it to be in a different folder")
      answer <- ""
      while(tolower(answer)!="yes" & tolower(answer)!="no") {
        answer <- readline(prompt="Enter here: ")
      }
      if (answer=="no") {
        print("Input the directory that you wish to save this patient's SEIZURE_DATA_CLINICAL file in")
        print("Example: C:/Folder_Name/")
        directory <- readline(prompt="Enter here: ")
        setwd(directory)
      }
      
      print(paste("Saving seizure clinical outcome table as",gsub(" ","",paste(patient,"_SEIZURE_DATA_CLINICAL.xlsx")),"in directory",getwd()))
      xlsx <- "SEIZURE_DATA_CLINICAL.xlsx"
      xlsx <- gsub(" ","",paste(patient,"_",xlsx))
      xlsx::write.xlsx2(results,file=xlsx,showNA=FALSE,row.names=FALSE)
    }
  } else {
    print("No therapy days, therefore response and score values will not be calculated")
  }
}
