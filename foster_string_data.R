library(htmltab)
library(reshape)
library(reshape2)
library(zoo)

#read url file
read.csv ("foster_string_urls.csv", header=TRUE) -> foster_string_urls

#make empty data table
foster_string_data <- htmltab(doc = "http://www.nwd-wc.usace.army.mil/ftppub/water_quality/tempstrings/FOS_S1_2010_09.html",header = 1,which=3)
foster_string_data <- foster_string_data[NULL,]

#download and compile data
for(i in 1:nrow(foster_string_urls)) {
  row <- foster_string_urls[i,]
  url <- paste(row$url,sep = "")
  #assign(paste("fos_str_",row$y,"_", row$m, sep = ""),htmltab(doc = url,header = 1,which=3))
  #df_list <- rbind(df_list,paste("fos_str_",row$y,"_", row$m, sep = ""))
  foster_string_data <- rbind(foster_string_data,htmltab(doc = url,header = 1,which=3))
}
rm(i,url,row)

#set NA's
foster_string_data[ foster_string_data == "-" ] = NA
foster_string_data[ foster_string_data == "Â" ] = NA
foster_string_data$Date <- na.locf(foster_string_data$Date,na.rm=FALSE)


#keep date and time (might use later)
#foster_string_data$date_time <- (strptime(paste (foster_string_data$Date,foster_string_data$Time), "%m/%d/%Y %H:%M"))
#colnames(foster_string_data)[15] <- "date_time"
#foster_string_data <- subset (foster_string_data,select = -c(Date,Time,X))

#keep just date
foster_string_data$Date <- as.Date(foster_string_data$Date, format="%m/%d/%Y")
foster_string_data <- subset (foster_string_data,select = -c(Time,X))

#convert temps from "factor" to "numeric"
foster_string_data$X0.5ft <- as.numeric(as.character(foster_string_data$X0.5ft))
foster_string_data$X5ft <- as.numeric(as.character(foster_string_data$X5ft))
foster_string_data$X10ft<- as.numeric(as.character(foster_string_data$X10ft))
foster_string_data$X15ft<- as.numeric(as.character(foster_string_data$X15ft))
foster_string_data$X20ft<- as.numeric(as.character(foster_string_data$X20ft))
foster_string_data$X30ft<- as.numeric(as.character(foster_string_data$X30ft))
foster_string_data$X40ft<- as.numeric(as.character(foster_string_data$X40ft))
foster_string_data$X50ft<- as.numeric(as.character(foster_string_data$X50ft))
foster_string_data$X60ft<- as.numeric(as.character(foster_string_data$X60ft))
foster_string_data$X70ft<- as.numeric(as.character(foster_string_data$X70ft))
foster_string_data$X80ft<- as.numeric(as.character(foster_string_data$X80ft))

###string temps need boundaries because they aren't as clean as USGS data
#omit string temps over 40C
foster_string_data$X0.5ft <- as.numeric(ifelse(foster_string_data$X0.5ft>25,NA,foster_string_data$X0.5ft))
foster_string_data$X5ft <- as.numeric(ifelse(foster_string_data$X0.5ft>25,NA,foster_string_data$X5ft))
foster_string_data$X10ft <- as.numeric(ifelse(foster_string_data$X10ft>25,NA,foster_string_data$X10ft))
foster_string_data$X15ft <- as.numeric(ifelse(foster_string_data$X15ft>25,NA,foster_string_data$X15ft))
foster_string_data$X20ft <- as.numeric(ifelse(foster_string_data$X20ft>25,NA,foster_string_data$X20ft))
foster_string_data$X30ft <- as.numeric(ifelse(foster_string_data$X30ft>25,NA,foster_string_data$X30ft))
foster_string_data$X40ft <- as.numeric(ifelse(foster_string_data$X40ft>25,NA,foster_string_data$X40ft))
foster_string_data$X50ft <- as.numeric(ifelse(foster_string_data$X50ft>25,NA,foster_string_data$X50ft))
foster_string_data$X60ft <- as.numeric(ifelse(foster_string_data$X60ft>25,NA,foster_string_data$X60ft))
foster_string_data$X70ft <- as.numeric(ifelse(foster_string_data$X70ft>25,NA,foster_string_data$X70ft))
foster_string_data$X80ft <- as.numeric(ifelse(foster_string_data$X80ft>25,NA,foster_string_data$X80ft))

#omit string temps under 0C
foster_string_data$X0.5ft <- as.numeric(ifelse(foster_string_data$X0.5ft<0,NA,foster_string_data$X0.5ft))
foster_string_data$X5ft <- as.numeric(ifelse(foster_string_data$X0.5ft<0,NA,foster_string_data$X5ft))
foster_string_data$X10ft <- as.numeric(ifelse(foster_string_data$X10ft<0,NA,foster_string_data$X10ft))
foster_string_data$X15ft <- as.numeric(ifelse(foster_string_data$X15ft<0,NA,foster_string_data$X15ft))
foster_string_data$X20ft <- as.numeric(ifelse(foster_string_data$X20ft<0,NA,foster_string_data$X20ft))
foster_string_data$X30ft <- as.numeric(ifelse(foster_string_data$X30ft<0,NA,foster_string_data$X30ft))
foster_string_data$X40ft <- as.numeric(ifelse(foster_string_data$X40ft<0,NA,foster_string_data$X40ft))
foster_string_data$X50ft <- as.numeric(ifelse(foster_string_data$X50ft<0,NA,foster_string_data$X50ft))
foster_string_data$X60ft <- as.numeric(ifelse(foster_string_data$X60ft<0,NA,foster_string_data$X60ft))
foster_string_data$X70ft <- as.numeric(ifelse(foster_string_data$X70ft<0,NA,foster_string_data$X70ft))
foster_string_data$X80ft <- as.numeric(ifelse(foster_string_data$X80ft<0,NA,foster_string_data$X80ft))

#convert temp data from wide format, to long format (columns to rows)
foster_string_data <- melt(foster_string_data,id.vars="Date")

#average temp by date and height
foster_string_data <- aggregate(value~Date+variable,data=foster_string_data, FUN="mean")

#write.csv(foster_string_data,"foster_string_data_BACKUP.csv")
#read.csv ("foster_string_data_BACKUP.csv", header=TRUE) -> foster_string_data
