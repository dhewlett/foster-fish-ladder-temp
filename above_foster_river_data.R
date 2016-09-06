#install.packages ("plyr")
#install.packages ("xlsx")
#install.packages ("RCurl")
library(plyr)
library(xlsx)
library(RCurl)



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


