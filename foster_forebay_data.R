library(rjson)
library(tidyr)

json_file <- "http://www.nwd-wc.usace.army.mil/dd/common/web_service/webexec/getjson?backward=7d&startdate=09%2F01%2F2008+07%3A00&enddate=09%2F05%2F2016+07%3A00&query=%5B%22FOS.Elev-Forebay.Ave.~1Day.1Day.Best%22%5D&timezone=GMT"
json_data <- fromJSON(file=json_file)
temp <- melt((((json_data$FOS)$timeseries)$'FOS.Elev-Forebay.Ave.~1Day.1Day.Best')$values)
temp <- subset (temp,select = -c(L1))
temp$L2 <- factor(temp$L2)
temp$L2 <- as.character(temp$L2)
temp$L2[temp$L2==1] <- 'timestamp'
temp$L2[temp$L2==2] <- 'ht'
temp$L2[temp$L2==3] <- 'X3'



temp1 <- dcast(temp, formula = value ~ L2, value.var="value" )


reshape(temp,direction='wide')


temp1 <- t(temp)
temp1 <- melt(temp,id.vars='L2',variable_name=value)

nodata <- data.frame('timestamp'= character(0), 'ht'= character(0), 'X3' = character(0))


rm(temp1,temp2, temp3, temp4, temp5,temp,nodata)
rm(json_data,json,temp,json_file)






