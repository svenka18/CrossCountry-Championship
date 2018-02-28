require(dplyr)
require(data.table)
require(stringr)
require(lubridate)
require(zoo)

#Day
files <- list.files(path="C:/Users/saipr/Desktop/Sai Priya/ML Projects/CrossCountry/Data/Weather_day", pattern="*.csv", full.names=T, recursive=FALSE)
for(i in files){
  #reading each file
  tmp <- read.csv(i, header=T, na.strings=c("","NA"))
  #selecting only necessary columns
  tmp <- tmp[,c(1,3,4,5,6,11,17,18,19)]
  #Time
  y<-strsplit(as.character(tmp$DATE),' ') 
  z<-data.frame(tmp$DATE, do.call(rbind, y))
  tmp$time<-z$X2
  tmp$time <-strptime(tmp$time,format='%H:%M')
  #Date, converting into common format
  a <- as.Date(tmp$DATE,format="%m/%d/%Y") # Produces NA when format is not "%m/%d/%Y"
  b <- as.Date(tmp$DATE,format="%Y-%m-%d") # Produces NA when format is not "%d.%m.%Y"
  a[is.na(a)] <- b[!is.na(b)] # Combine both while keeping their ranks
  tmp$DATE <- a
  #Year
  tmp$year<-substr(tmp$DATE,1,4)
  tmp$year<-as.numeric(tmp$year)
  #Cleansing Temp
  tmp$HOURLYDRYBULBTEMPF[which(substring(tmp$HOURLYDRYBULBTEMPF,3)=="s")]<-NA
  tmp$HOURLYDRYBULBTEMPF[which(substring(tmp$HOURLYDRYBULBTEMPF,2)=="s")]<-NA
  tmp$HOURLYDRYBULBTEMPF[tmp$HOURLYDRYBULBTEMPF == '*'] <- NA
  tmp$HOURLYDRYBULBTEMPF[tmp$HOURLYDRYBULBTEMPF=="VRB"]<-NA
  tmp$HOURLYDRYBULBTEMPF<-as.numeric(as.character(tmp$HOURLYDRYBULBTEMPF))
  #Cleansing Wind speed
  tmp$HOURLYWindSpeed[which(substring(tmp$HOURLYWindSpeed,3)=="s")]<-NA
  tmp$HOURLYWindSpeed[which(substring(tmp$HOURLYWindSpeed,2)=="s")]<-NA
  tmp$HOURLYWindSpeed[tmp$HOURLYWindSpeed == '*'] <- NA
  tmp$HOURLYWindSpeed[tmp$HOURLYWindSpeed=="VRB"]<-NA
  tmp$HOURLYWindSpeed<-as.numeric(as.character(tmp$HOURLYWindSpeed))
  #Cleansing wind direction
  tmp$HOURLYWindDirection[which(substring(tmp$HOURLYWindDirection,3)=="s")]<-NA
  tmp$HOURLYWindDirection[which(substring(tmp$HOURLYWindDirection,2)=="s")]<-NA
  tmp$HOURLYWindDirection[tmp$HOURLYWindDirection == '*'] <- NA
  tmp$HOURLYWindDirection[tmp$HOURLYWindDirection=="VRB"]<-NA
  tmp$HOURLYWindDirection<-as.numeric(as.character(tmp$HOURLYWindDirection))
  #Cleansing humidity
  tmp$HOURLYRelativeHumidity[which(substring(tmp$HOURLYRelativeHumidity,3)=="s")]<-NA
  tmp$HOURLYRelativeHumidity[which(substring(tmp$HOURLYRelativeHumidity,2)=="s")]<-NA
  tmp$HOURLYRelativeHumidity[tmp$HOURLYRelativeHumidity == '*'] <- NA
  tmp$HOURLYRelativeHumidity[tmp$HOURLYRelativeHumidity=="VRB"]<-NA
  tmp$HOURLYRelativeHumidity<-as.numeric(as.character(tmp$HOURLYRelativeHumidity))
  #filtering times
  tmp_Day<- subset(tmp,time>="2017-07-20 09:00:00" & time<="2017-07-20 10:00:00")
  tmp_Eve<- subset(tmp,time>="2017-07-20 15:00:00" & time<="2017-07-20 16:00:00")
  tmp_Day<- aggregate(tmp_Day[,-5], list(tmp_Day$DATE), mean,na.rm=TRUE)
  #tmp_Eve<- aggregate(tmp_Eve[,-5], list(tmp_Eve$DATE), mean,na.rm=TRUE)
  #tmp <- rbind(tmp_Day,tmp_Eve)
  tmp<-tmp_Day
  names(tmp)[1]<-paste("DATE")  # works
  y<-strsplit(as.character(tmp$time),' ') 
  z<-data.frame(tmp$time, do.call(rbind, y))
  tmp$time<-z$X2
  tmp$STATION<-NULL
  #adding county name from file name
  tmp$county<-str_sub(basename(i),1,-5)
  #writing back to the file
  write.csv(tmp, i)
}
View(tmp_Day)
View(tmp)

#Evening
files <- list.files(path="C:/Users/saipr/Desktop/Sai Priya/ML Projects/CrossCountry/Data/Weather_eve", pattern="*.csv", full.names=T, recursive=FALSE)
for(i in files){
  #reading each file
  tmp <- read.csv(i, header=T, na.strings=c("","NA"))
  #selecting only necessary columns
  tmp <- tmp[,c(1,3,4,5,6,11,17,18,19)]
  #Time
  y<-strsplit(as.character(tmp$DATE),' ') 
  z<-data.frame(tmp$DATE, do.call(rbind, y))
  tmp$time<-z$X2
  tmp$time <-strptime(tmp$time,format='%H:%M')
  #Date, converting into common format
  a <- as.Date(tmp$DATE,format="%m/%d/%Y") # Produces NA when format is not "%m/%d/%Y"
  b <- as.Date(tmp$DATE,format="%Y-%m-%d") # Produces NA when format is not "%d.%m.%Y"
  a[is.na(a)] <- b[!is.na(b)] # Combine both while keeping their ranks
  tmp$DATE <- a
  #Year
  tmp$year<-substr(tmp$DATE,1,4)
  tmp$year<-as.numeric(tmp$year)
  #Cleansing Temp
  tmp$HOURLYDRYBULBTEMPF[which(substring(tmp$HOURLYDRYBULBTEMPF,3)=="s")]<-NA
  tmp$HOURLYDRYBULBTEMPF[which(substring(tmp$HOURLYDRYBULBTEMPF,2)=="s")]<-NA
  tmp$HOURLYDRYBULBTEMPF[tmp$HOURLYDRYBULBTEMPF == '*'] <- NA
  tmp$HOURLYDRYBULBTEMPF[tmp$HOURLYDRYBULBTEMPF=="VRB"]<-NA
  tmp$HOURLYDRYBULBTEMPF<-as.numeric(as.character(tmp$HOURLYDRYBULBTEMPF))
  #Cleansing Wind speed
  tmp$HOURLYWindSpeed[which(substring(tmp$HOURLYWindSpeed,3)=="s")]<-NA
  tmp$HOURLYWindSpeed[which(substring(tmp$HOURLYWindSpeed,2)=="s")]<-NA
  tmp$HOURLYWindSpeed[tmp$HOURLYWindSpeed == '*'] <- NA
  tmp$HOURLYWindSpeed[tmp$HOURLYWindSpeed=="VRB"]<-NA
  tmp$HOURLYWindSpeed<-as.numeric(as.character(tmp$HOURLYWindSpeed))
  #Cleansing wind direction
  tmp$HOURLYWindDirection[which(substring(tmp$HOURLYWindDirection,3)=="s")]<-NA
  tmp$HOURLYWindDirection[which(substring(tmp$HOURLYWindDirection,2)=="s")]<-NA
  tmp$HOURLYWindDirection[tmp$HOURLYWindDirection == '*'] <- NA
  tmp$HOURLYWindDirection[tmp$HOURLYWindDirection=="VRB"]<-NA
  tmp$HOURLYWindDirection<-as.numeric(as.character(tmp$HOURLYWindDirection))
  #Cleansing humidity
  tmp$HOURLYRelativeHumidity[which(substring(tmp$HOURLYRelativeHumidity,3)=="s")]<-NA
  tmp$HOURLYRelativeHumidity[which(substring(tmp$HOURLYRelativeHumidity,2)=="s")]<-NA
  tmp$HOURLYRelativeHumidity[tmp$HOURLYRelativeHumidity == '*'] <- NA
  tmp$HOURLYRelativeHumidity[tmp$HOURLYRelativeHumidity=="VRB"]<-NA
  tmp$HOURLYRelativeHumidity<-as.numeric(as.character(tmp$HOURLYRelativeHumidity))
  #filtering times
  #tmp_Day<- subset(tmp,time>="2017-07-20 09:00:00" & time<="2017-07-20 10:00:00")
  tmp_Eve<- subset(tmp,time>="2017-07-20 15:00:00" & time<="2017-07-20 16:00:00")
  #tmp_Day<- aggregate(tmp_Day[,-5], list(tmp_Day$DATE), mean,na.rm=TRUE)
  tmp_Eve<- aggregate(tmp_Eve[,-5], list(tmp_Eve$DATE), mean,na.rm=TRUE)
  #tmp <- rbind(tmp_Day,tmp_Eve)
  tmp<-tmp_Eve
  names(tmp)[1]<-paste("DATE")  # works
  y<-strsplit(as.character(tmp$time),' ') 
  z<-data.frame(tmp$time, do.call(rbind, y))
  tmp$time<-z$X2
  tmp$STATION<-NULL
  #adding county name from file name
  tmp$county<-str_sub(basename(i),1,-5)
  #writing back to the file
  write.csv(tmp, i)
}


