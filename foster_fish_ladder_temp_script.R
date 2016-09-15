##########################
###Environment/Packages###
##########################

packages <- c("plyr", "xlsx", "RCurl", "rjson", "htmltab", "reshape", "reshape2", "zoo")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))}
rm(packages)

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
USGS_mfsnt$temp <- rowMeans(subset(USGS_mfsnt,select=c(temp_1,temp_2,temp_3)))
USGS_mfsnt <- subset(USGS_mfsnt,select=c(DateTime,temp))
USGS_mfsnt$DateTime <- as.Date(USGS_mfsnt$DateTime, format="%Y-%m-%d")

names(USGS_ssnt)[2] <- "temp_1"
names(USGS_ssnt)[4] <- "temp_2"
names(USGS_ssnt)[6] <- "temp_3"
USGS_ssnt$temp <- rowMeans(subset(USGS_ssnt,select=c(temp_1,temp_2,temp_3)))
USGS_ssnt <- subset(USGS_ssnt,select=c(DateTime,temp))
USGS_ssnt$DateTime <- as.Date(USGS_ssnt$DateTime, format="%Y-%m-%d")

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

#use this line to read previously downloaded data. If no data exists run the indented code block.
#read.csv ("foster_string_data_BACKUP.csv", header=TRUE) -> foster_string_data

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

write.csv(foster_string_data,"foster_string_data_BACKUP.csv")
#read.csv ("foster_string_data_BACKUP.csv", header=TRUE) -> foster_string_data

##############
###Analysis###
##############
#create intake height column
foster_forebay_ht$intake_ht <- ifelse(foster_forebay_ht$timestamp<"2014-04-01",foster_forebay_ht$ht-600,foster_forebay_ht$ht-585)
#if height was the same as old...
#foster_forebay_ht$intake <- foster_forebay_ht$ht-600

#assign temp string data point to intake height
foster_forebay_ht$intake_string_ht <- 
  ifelse(foster_forebay_ht$intake_ht<2.75,"X0.5ft",
         ifelse(foster_forebay_ht$intake_ht<7.5,"X5ft",
                ifelse(foster_forebay_ht$intake_ht<12.5,"X10ft",
                       ifelse(foster_forebay_ht$intake_ht<17.5,"X15ft",
                              ifelse(foster_forebay_ht$intake_ht<25,"X20ft",
                                     ifelse(foster_forebay_ht$intake_ht<35,"X30ft",
                                            ifelse(foster_forebay_ht$intake_ht<45,"X40ft",
                                                   ifelse(foster_forebay_ht$intake_ht<55,"X50ft",
                                                          ifelse(foster_forebay_ht$intake_ht<65,"X60ft",
                                                                 ifelse(foster_forebay_ht$intake_ht<75,"X70ft","X80ft"
                                                                        ))))))))))
#merge temp data with height data based on timestamp AND temp string height
intake_temp <- merge(x=foster_forebay_ht,y=foster_string_data,by.x=c("timestamp","intake_string_ht"),by.y=c("Date","variable"),all.x = TRUE)
#rm(string.temp,height)

#rename columns
intake_temp$forebay_ht <- intake_temp$ht
intake_temp$date <- intake_temp$timestamp
intake_temp$DateTime <- intake_temp$date
intake_temp$intake_temp <- intake_temp$value
intake_temp <-subset(intake_temp,select=c("DateTime","intake_temp"))

#merge three data sets based on date
plot_data <- merge(x=USGS_mfsnt, y=USGS_ssnt, by='DateTime',all=TRUE)
plot_data <- merge(x=plot_data, y=intake_temp, by.x='DateTime', by.y='DateTime', all=TRUE)

#rename columns
plot_data$mfsnt <- plot_data$temp.x
plot_data$ssnt <- plot_data$temp.y
plot_data <-subset(plot_data,select=c("DateTime","mfsnt","ssnt","intake_temp"))

#remove assumed erronious data and irrelevant data.
plot_data$intake_temp <- ifelse(plot_data$DateTime>"2014-01-23" & plot_data$DateTime<"2014-01-30",NA,plot_data$intake_temp)
plot_data$intake_temp <- ifelse(plot_data$DateTime>"2015-04-07" & plot_data$DateTime<"2015-05-21",NA,plot_data$intake_temp)
plot_data$intake_temp <- ifelse(plot_data$DateTime>"2015-03-02" & plot_data$DateTime<"2015-03-13",NA,plot_data$intake_temp)
plot_data <- subset(plot_data,DateTime>as.Date("2010-01-01"))

#output csv as backup
write.csv(plot_data,"foster_fish_ladder_temp_data.csv")
rm(foster_forebay_ht, foster_string_data, intake_temp, USGS_mfsnt, USGS_ssnt)

#plot_data <- subset(read.csv ("foster_fish_ladder_temp_data.csv", header=TRUE),select=c("DateTime","mfsnt","ssnt","intake_temp"))
#plot_data$DateTime <- as.Date(plot_data$DateTime, format="%Y-%m-%d")

###############
##Sample plot##
###############

ggplot(data=plot_data, aes(x=DateTime))+
  geom_line(aes(y=intake_temp, colour='Foster ladder intake'))+
  geom_line(aes(y=ssnt, colour='South Santiam at Cascadia'))+
  geom_line(aes(y=mfsnt, colour='Middle Santiam below Green Peter'))+ 
  scale_y_continuous(breaks=seq(0, 25, 2))+
  xlab("Date")+
  ylab("Temp (C)")+
  scale_colour_discrete(name="Source")+
  scale_x_date(date_breaks = "3 month",date_labels = "%b -%y")+
  #theme(panel.grid.major = element_line(colour = 1),
  #      panel.grid.minor = element_line(colour = 2))+
  geom_vline(xintercept = as.numeric(as.Date("2014-04-01")), linetype="dashed", 
             color = "red", size=1.25)

