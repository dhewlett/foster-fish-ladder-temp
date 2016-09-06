#set working direcotry
#setwd("P:/Willam/new BiOp/HRME Chinook/HewlettProjects/R/Foster temp")
setwd("~/R/Foster temp")

#check/install packages
packages <- c("reshape2", "lubridate", "scales", "ggplot2")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))}
rm(packages)

#enable packages
library(reshape2)
library(lubridate)
library(scales)
library(ggplot2)

#read tables
cas.temp <- read.csv ("cascadia.csv", header=TRUE)
string.temp <- read.csv ("FosterForebayTemp.csv", header=TRUE, na.strings = "-")
gp.temp <- read.csv ("green_peter.csv", header=TRUE)
height <- read.csv ("FosterForebayHT.csv", header=TRUE)



##CAS AND GP prep
#convert timestamps to dates
cas.temp$DateTime <- as.character(cas.temp$DateTime)
cas.temp$DateTime <- strptime(cas.temp$DateTime,"%Y-%m-%d %H:%M")
gp.temp$DateTime <- as.character(gp.temp$DateTime)
gp.temp$DateTime <- strptime(gp.temp$DateTime,"%Y-%m-%d %H:%M")

#round timestamp by days
cas.temp$DateTime <- round(cas.temp$DateTime,"days")
gp.temp$DateTime <- round(gp.temp$DateTime,"days")

#average temp by day
cas.temp$DateTime <- as.character(cas.temp$DateTime)
cas.temp <- aggregate(X2.00010~DateTime,data=cas.temp, FUN="mean")
cas.temp$DateTime <- strptime(cas.temp$DateTime,"%Y-%m-%d")
gp.temp$DateTime <- as.character(gp.temp$DateTime)
gp.temp <- aggregate(X1.00010~DateTime,data=gp.temp, FUN="mean")
gp.temp$DateTime <- strptime(gp.temp$DateTime,"%Y-%m-%d")



##FOSTER temp prep
#remove redundant temp sting column names
string.temp <- subset(string.temp, Time!="Time")
string.temp <- subset (string.temp,select = -c(Date,Time,date.all))

#convert timestamp to dates
string.temp$date.time <- as.character(string.temp$date.time)
string.temp$date.time <- strptime(string.temp$date.time,"%m/%d/%Y %H:%M")

#round timestamp by days
string.temp$date.time <- round(string.temp$date.time,"days")
string.temp$date.time <- as.character(string.temp$date.time)

#convert temps from "factor" to "numeric"
string.temp$X0.5 <- as.numeric(as.character(string.temp$X0.5))
string.temp$X5<- as.numeric(as.character(string.temp$X5))
string.temp$X10<- as.numeric(as.character(string.temp$X10))
string.temp$X15<- as.numeric(as.character(string.temp$X15))
string.temp$X20<- as.numeric(as.character(string.temp$X20))
string.temp$X30<- as.numeric(as.character(string.temp$X30))
string.temp$X40<- as.numeric(as.character(string.temp$X40))
string.temp$X50<- as.numeric(as.character(string.temp$X50))
string.temp$X60<- as.numeric(as.character(string.temp$X60))
string.temp$X70<- as.numeric(as.character(string.temp$X70))
string.temp$X80<- as.numeric(as.character(string.temp$X80))

###string temps need boundaries because they aren't as clean as USGS data
#omit string temps over 40C
string.temp$X0.5 <- as.numeric(ifelse(string.temp$X0.5>25,NA,string.temp$X0.5))
string.temp$X5 <- as.numeric(ifelse(string.temp$X5>25,NA,string.temp$X5))
string.temp$X10 <- as.numeric(ifelse(string.temp$X10>25,NA,string.temp$X10))
string.temp$X15 <- as.numeric(ifelse(string.temp$X15>25,NA,string.temp$X15))
string.temp$X20 <- as.numeric(ifelse(string.temp$X20>25,NA,string.temp$X20))
string.temp$X30 <- as.numeric(ifelse(string.temp$X30>25,NA,string.temp$X30))
string.temp$X40 <- as.numeric(ifelse(string.temp$X40>25,NA,string.temp$X40))
string.temp$X50 <- as.numeric(ifelse(string.temp$X50>25,NA,string.temp$X50))
string.temp$X60 <- as.numeric(ifelse(string.temp$X60>25,NA,string.temp$X60))
string.temp$X70 <- as.numeric(ifelse(string.temp$X70>25,NA,string.temp$X70))
string.temp$X80 <- as.numeric(ifelse(string.temp$X80>25,NA,string.temp$X80))

#omit string temps under 0C
string.temp$X0.5 <- as.numeric(ifelse(string.temp$X0.5<0,NA,string.temp$X0.5))
string.temp$X5 <- as.numeric(ifelse(string.temp$X5<0,NA,string.temp$X5))
string.temp$X10 <- as.numeric(ifelse(string.temp$X10<0,NA,string.temp$X10))
string.temp$X15 <- as.numeric(ifelse(string.temp$X15<0,NA,string.temp$X15))
string.temp$X20 <- as.numeric(ifelse(string.temp$X20<0,NA,string.temp$X20))
string.temp$X30 <- as.numeric(ifelse(string.temp$X30<0,NA,string.temp$X30))
string.temp$X40 <- as.numeric(ifelse(string.temp$X40<0,NA,string.temp$X40))
string.temp$X50 <- as.numeric(ifelse(string.temp$X50<0,NA,string.temp$X50))
string.temp$X60 <- as.numeric(ifelse(string.temp$X60<0,NA,string.temp$X60))
string.temp$X70 <- as.numeric(ifelse(string.temp$X70<0,NA,string.temp$X70))
string.temp$X80 <- as.numeric(ifelse(string.temp$X80<0,NA,string.temp$X80))

#average string.temp data by the hour
string.temp <- aggregate(cbind(X0.5,X5,X10,X15,X20,X30,X40,X50,X60,X70,X80)~date.time,data=string.temp, FUN="mean")
string.temp$date.time <- strptime(string.temp$date.time,"%Y-%m-%d")

#convert temp data from wide format, to long format (columns to rows)
string.temp <- melt(string.temp,id.vars="date.time")



##FOSTER forebay ht prep
#convert date.time from "factor" to time
height$date.time <- as.character(height$date.time)
height$date.time <- strptime(height$date.time,"%m/%d/%Y %H:%M")

#round height data to days
height$date.time <- round(height$date.time,"days")

#average height data based on days
height$date.time <- as.character(height$date.time)
height <- aggregate(Data~date.time,data=height, FUN="mean")
height$date.time <- strptime(height$date.time,"%Y-%m-%d")

#create intake height column
height$intake <- ifelse(height$date.time<"2014-04-01",height$Data-600,height$Data-585)
#if height was the same as old...
#height$intake <- height$Data-600

#assign temp string data point to intake height
height$temp.column <- 
  ifelse(height$intake<2.75,"X0.5",
         ifelse(height$intake<7.5,"X5",
                ifelse(height$intake<12.5,"X10",
                       ifelse(height$intake<17.5,"X20",
                              ifelse(height$intake<25,"X20",
                                     ifelse(height$intake<35,"X30",
                                            ifelse(height$intake<45,"X40",
                                                   ifelse(height$intake<55,"X50",
                                                          ifelse(height$intake<65,"X60",
                                                                 ifelse(height$intake<75,"X70","X80"))))))))))



##Analysis
#convert various columns to character so they will match when merging
string.temp$variable <- as.character(string.temp$variable)
height$date.time <- as.character(height$date.time)
string.temp$date.time <- as.character(string.temp$date.time)

#merge temp data with height data based on timestamp AND temp string height
intake.temp <- merge(x=height,y=string.temp,by.x=c("date.time","temp.column"),by.y=c("date.time","variable"),all.x = TRUE)
#rm(string.temp,height)

#renaming columns
intake.temp$timestamp <- intake.temp$date.time
intake.temp$temp.string.ht <- intake.temp$temp.column
intake.temp$forebay.ht <- intake.temp$Data
intake.temp$intake.ht <- intake.temp$intake
intake.temp$temp.string.temp <- intake.temp$value
intake.temp <-subset(intake.temp,select=c("timestamp","forebay.ht","intake.ht","temp.string.ht","temp.string.temp"))

#convert time back from character to PLOTNG time format
intake.temp$timestamp <- strptime(intake.temp$timestamp,"%Y-%m-%d")

#merge three data sets based on date
plot.data <- merge(x=cas.temp, y=gp.temp, by='DateTime',all=TRUE)
plot.data <- merge(x=plot.data, y=intake.temp, by.x='DateTime', by.y='timestamp', all=TRUE)

#rename columns
plot.data$cas.temp <- plot.data$X2.00010
plot.data$gp.temp <- plot.data$X1.00010
plot.data <- subset(plot.data,select=c(
  "DateTime",
  "gp.temp",
  "cas.temp",
  "temp.string.temp"))
plot.data$DateTime <- as.Date(plot.data$DateTime)

#set year bounds
plot.data <- subset(plot.data, year(DateTime)>2010)

#remove Jan 2014 string data. Erronious.
plot.data$temp.string.temp <- ifelse(plot.data$DateTime>"2014-01-23" & plot.data$DateTime<"2014-01-30",NA,plot.data$temp.string.temp)
plot.data$temp.string.temp <- ifelse(plot.data$DateTime>"2015-04-07" & plot.data$DateTime<"2015-05-21",NA,plot.data$temp.string.temp)
plot.data$temp.string.temp <- ifelse(plot.data$DateTime>"2015-03-02" & plot.data$DateTime<"2015-03-13",NA,plot.data$temp.string.temp)

#plot.data <- subset(plot.data,DateTime<"2014-01-24" | DateTime>"2014-01-29")
#plot.data <- subset(plot.data,DateTime<"2015-04-08" | DateTime>"2015-05-20")
#plot.data <- subset(plot.data,DateTime<"2015-03-03" | DateTime>"2015-03-12")




##GRAPH
png("P:/Willam/new BiOp/HRME Chinook/HewlettProjects/R/Foster temp/plots/11-14 daily.png", width=1200, height=600, res=96)
print(plot( 
  
  
ggplot(data=plot.data, aes(x=DateTime))+
  geom_line(aes(y=temp.string.temp, colour='Foster ladder intake'))+
  geom_line(aes(y=cas.temp, colour='South Santiam at Cascadia'))+
  geom_line(aes(y=gp.temp, colour='Middle Santiam below Green Peter'))+ 
  scale_x_date(breaks = date_breaks("3 months"),
               minor_breaks = "months",
               labels = date_format("%b-%y"))+
  xlab("Date")+
  ylab("Temp (C)")+
  scale_colour_discrete(name="Source")+
  theme(panel.grid.major = element_line(colour = ),
        panel.grid.minor = element_line(colour = ))




))
dev.off()
#rm(cas.temp,string.temp,gp.temp,height,intake.temp,plot.data,temp)
#rm(cas.temp,gp.temp,height,intake.temp,plot.data,string.temp)
#write.csv(plot.data,"test.csv")
#write.csv(string.temp,"test.csv")
#write.csv(height,"test.csv")




