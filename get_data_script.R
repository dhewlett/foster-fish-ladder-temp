###packages needs cleanup###

#install.packages ("plyr")
#install.packages ("xlsx")
#install.packages ("RCurl")
#install.packages("rjson")
#install.packages("reshape2")
#install.packages("htmltab")
#install.packages("reshape")
#install.packages("reshape2")
#install.packages("zoo")
library(htmltab)
library(reshape)
library(reshape2)
library(zoo)
library(rjson)
library(plyr)
library(xlsx)
library(RCurl)

#####################
###River temp data###
#####################

#generate current URL for data request using yesterday's date
t1 <- "http://waterservices.usgs.gov/nwis/dv/?format=excel&sites=14186200,14185000&startDT=2008-01-01&endDT="
t2 <- Sys.Date()-1
t3 <- "&parameterCd=00010"
download_url<-paste(t1,t2,t3,sep="")
download_url    
rm(t1,t2,t3)

#get data
download.file(url=download_url,destfile="USGS_above_foster_res.xlsx", mode="wb")
rm(download_url)

#import data from xlsx to data frames
USGS_mfsnt <- read.xlsx(file="USGS_above_foster_res.xlsx",sheetName="14186200",header=TRUE)
USGS_ssnt <- read.xlsx(file="USGS_above_foster_res.xlsx",sheetName="14185000",header=TRUE)

#average 3 temperature columns, and remove all other columns
names(USGS_mfsnt)[2] <- "temp_1"
names(USGS_mfsnt)[4] <- "temp_2"
names(USGS_mfsnt)[6] <- "temp_3"
USGS_mfsnt$mean <- rowMeans(subset(USGS_mfsnt,select=c(temp_1,temp_2,temp_3)))
USGS_mfsnt <- subset(USGS_mfsnt,select=c(DateTime,mean))

names(USGS_ssnt)[2] <- "temp_1"
names(USGS_ssnt)[4] <- "temp_2"
names(USGS_ssnt)[6] <- "temp_3"
USGS_ssnt$mean <- rowMeans(subset(USGS_ssnt,select=c(temp_1,temp_2,temp_3)))
USGS_ssnt <- subset(USGS_ssnt,select=c(DateTime,mean))

file.remove("USGS_above_foster_res.xlsx")

##################
###Forebay Data###
##################

#generate current URL for data request using yesterday's date
t1 <- "http://www.nwd-wc.usace.army.mil/dd/common/web_service/webexec/getjson?backward=7d&startdate=09%2F01%2F2008+07%3A00&enddate="
t2 <- paste(format(Sys.Date()-1, "%m"),"%2F",format(Sys.Date()-1, "%d"),"%2F",format(Sys.Date()-1, "%Y"),sep ="")
t3 <- "+07%3A00&query=%5B%22FOS.Elev-Forebay.Ave.~1Day.1Day.Best%22%5D&timezone=GMT"
download_url <- paste(t1,t2,t3,sep="")
rm(t1,t2,t3)

#download data
raw_data <- fromJSON(file=download_url)

#remove headers
temp <- melt((((raw_data$FOS)$timeseries)$'FOS.Elev-Forebay.Ave.~1Day.1Day.Best')$values)

#data shifting
temp <- subset (temp,select = -c(L1))
temp$L2 <- as.character(temp$L2)
temp$L2[temp$L2==1] <- 'timestamp'
temp$L2[temp$L2==2] <- 'ht'
temp$L2[temp$L2==3] <- 'X3'
foster_forebay_ht <- unstack(temp)
foster_forebay_ht <- subset (foster_forebay_ht,select = -c(X3))

#data formating
foster_forebay_ht$timestamp <- data.frame(do.call('rbind', strsplit(as.character(foster_forebay_ht$timestamp),'T',fixed=TRUE)))[,1]
foster_forebay_ht$timestamp <- as.character(foster_forebay_ht$timestamp)
foster_forebay_ht$timestamp <- as.Date(foster_forebay_ht$timestamp, format="%Y-%m-%d")

options(digits = 5)
foster_forebay_ht$ht <- as.numeric(foster_forebay_ht$ht)

#cleanup
rm(temp,raw_data,download_url)

##################
###String Data###
##################

#read url file
read.csv ("foster_string_urls.csv", header=TRUE) -> foster_string_urls

#make empty data table
foster_string_data <- htmltab(doc = "http://www.nwd-wc.usace.army.mil/ftppub/water_quality/tempstrings/FOS_S1_2010_09.html",header = 1,which=3)
foster_string_data <- foster_string_data[NULL,]

#download and compile data
#this will download data between 9/2010 and 12/2018, and is based on foster_string_urls.csv
#some data won't exist yet, so there will be errors when the script tries to download pages that don't exist
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

#######################
###Comiling/Analysis###
#######################
#create intake height column
foster_forebay_ht$intake <- ifelse(foster_forebay_ht$timestamp<"2014-04-01",foster_forebay_ht$ht-600,foster_forebay_ht$ht-585)
#if height was the same as old...
#height$intake <- height$Data-600

