library(rjson)
library(reshape2)

#download data
json_file <- "http://www.nwd-wc.usace.army.mil/dd/common/web_service/webexec/getjson?backward=7d&startdate=09%2F01%2F2008+07%3A00&enddate=09%2F05%2F2016+07%3A00&query=%5B%22FOS.Elev-Forebay.Ave.~1Day.1Day.Best%22%5D&timezone=GMT"
json_data <- fromJSON(file=json_file)

#remove headers
temp <- melt((((json_data$FOS)$timeseries)$'FOS.Elev-Forebay.Ave.~1Day.1Day.Best')$values)

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
foster_forebay_ht$ht <- as.numeric(foster_forebay_ht$ht)
foster_forebay_ht$ht <- format(round((foster_forebay_ht$ht), 2), nsmall = 2)

#cleanup
rm(temp,json_data,json_file)
