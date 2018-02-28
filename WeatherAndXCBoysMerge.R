require(dplyr)
require(stringr)

#Day

#Merging all weather files
files <- list.files(path="C:/Users/saipr/Desktop/Sai Priya/ML Projects/CrossCountry/Data/Weather_day", pattern="*.csv", full.names=T, recursive=FALSE)
out.file<-""
county_names<-""
for(i in files){
  #reading each file
  tmp <- read.csv(i, header=T, na.strings=c("","NA"))
  out.file <- rbind(out.file, tmp)
  county_names<-rbind(county_names,str_sub(basename(i),1,-5))
}

write.csv(out.file,"C:/Users/saipr/Desktop/Sai Priya/ML Projects/CrossCountry/Data/weather_day.csv")
county_names
Counties<- read.csv("C:/Users/saipr/Desktop/Sai Priya/ML Projects/CrossCountry/Data/weather_day.csv",header=T, na.strings=c("","NA"))
Counties<-Counties[-1,]
sapply(Counties, function(x) sum(is.na(x))) 

#Date, converting into common format
a <- as.Date(Counties$DATE,format="%m/%d/%Y") # Produces NA when format is not "%m/%d/%Y"
b <- as.Date(Counties$DATE,format="%Y-%m-%d") # Produces NA when format is not "%d.%m.%Y"
a[is.na(a)] <- b[!is.na(b)] # Combine both while keeping their ranks
Counties$DATE <- a
Counties$DATE<-as.character(as.character.Date(Counties$DATE)) 
typeof(Counties$DATE)
View(Counties)

#handling Na's
Counties$HOURLYDRYBULBTEMPF<-na.approx(Counties$HOURLYDRYBULBTEMPF)
Counties$HOURLYWindSpeed<-na.approx(Counties$HOURLYWindSpeed)
Counties$HOURLYWindDirection<-na.approx(Counties$HOURLYWindDirection)
Counties$HOURLYRelativeHumidity<-na.approx(Counties$HOURLYRelativeHumidity)
sapply(Counties, function(x) sum(is.na(x))) 

#Cross Country Data
xc_boys <- read.csv("C:/Users/saipr/Desktop/Sai Priya/ML Projects/CrossCountry/Data/MeetCounty.csv", header=T, na.strings=c("","NA"))
xc_boys$county_original<-xc_boys$meet_county
xc_boys$county<-xc_boys$meet_county
xc_boys$meet_county<-NULL
View(xc_boys)
#Date, converting into common format
xc_boys$DATE <- as.Date(xc_boys$DATE,format="%m/%d/%Y") # Produces NA when format is not "%m/%d/%Y"
xc_boys$DATE<-as.character(as.character.Date(xc_boys$DATE)) 
#substring(xc_boys$DATE,1,2)="20"
typeof(xc_boys$DATE)

# For other counties
xc_boys$county[xc_boys$county %in% county_names == FALSE & xc_boys$meet_region=="Western Mountain"] <- "Burke"
xc_boys$county[xc_boys$county %in% county_names == FALSE & xc_boys$meet_region=="Coastal"] <- "Lee"
xc_boys$county[xc_boys$county %in% county_names == FALSE & xc_boys$meet_region=="Piedmont"] <- "Beaufort"
View(xc_boys)

#Merging weather and xc_boys
final_day1 <- left_join(xc_boys,Counties,by=c("DATE","county"))
View(final_day1)
dim(final_day1)
sapply(final_day1, function(x) sum(is.na(x))) 

#handling records that are not in weather file
final_day2 <- anti_join(final_day1,Counties,by=c("DATE","county"))
final_day2$county[final_day2$Region=="Western Mountain"] <- "Burke"
final_day2$county[final_day2$Region=="Coastal"] <- "Lee"
final_day2$county[final_day2$Region=="Piedmont"] <- "Beaufort"
View(final_day2)
colnames(final_day2)
final_day2<-final_day2[,c(1:28)]
final_day2 <- left_join(final_day2,Counties,by=c("DATE","county"))
sapply(final_day2, function(x) sum(is.na(x))) 
dim(final_day2)
#removing records that are not in weather file before merging
final_day1<-subset(final_day1,!is.na(HOURLYDRYBULBTEMPF))
dim(final_day1)
final_day1<-final_day1[,-c(29:30)]
final_day2<-final_day2[,-c(29:30)]
#Merging
final_day<-rbind(final_day1,final_day2)
dim(final_day)
sapply(final_day, function(x) sum(is.na(x))) 
write.csv(final_day,"C:/Users/saipr/Desktop/Sai Priya/ML Projects/CrossCountry/Data/xc_boys_day.csv")

#Evening
#Merging all weather files
files <- list.files(path="C:/Users/saipr/Desktop/Sai Priya/ML Projects/CrossCountry/Data/Weather_eve", pattern="*.csv", full.names=T, recursive=FALSE)
out.file<-""
county_names<-""
for(i in files){
  #reading each file
  tmp <- read.csv(i, header=T, na.strings=c("","NA"))
  out.file <- rbind(out.file, tmp)
  county_names<-rbind(county_names,str_sub(basename(i),1,-5))
}
write.csv(out.file,"C:/Users/saipr/Desktop/Sai Priya/ML Projects/CrossCountry/Data/weather_eve.csv")

Counties<- read.csv("C:/Users/saipr/Desktop/Sai Priya/ML Projects/CrossCountry/Data/weather_eve.csv",header=T, na.strings=c("","NA"))
Counties<-Counties[-1,]
sapply(Counties, function(x) sum(is.na(x))) 

#Date, converting into common format
a <- as.Date(Counties$DATE,format="%m/%d/%Y") # Produces NA when format is not "%m/%d/%Y"
b <- as.Date(Counties$DATE,format="%Y-%m-%d") # Produces NA when format is not "%d.%m.%Y"
a[is.na(a)] <- b[!is.na(b)] # Combine both while keeping their ranks
Counties$DATE <- a
Counties$DATE<-as.character(as.character.Date(Counties$DATE)) 
typeof(Counties$DATE)
View(Counties)

#handling Na's
Counties$HOURLYDRYBULBTEMPF<-na.approx(Counties$HOURLYDRYBULBTEMPF)
Counties$HOURLYWindSpeed<-na.approx(Counties$HOURLYWindSpeed)
Counties$HOURLYWindDirection<-na.approx(Counties$HOURLYWindDirection)
Counties$HOURLYRelativeHumidity<-na.approx(Counties$HOURLYRelativeHumidity)
sapply(Counties, function(x) sum(is.na(x))) 

#Cross Country Data
xc_boys <- read.csv("C:/Users/saipr/Desktop/Sai Priya/ML Projects/CrossCountry/Data/MeetCounty.csv", header=T, na.strings=c("","NA"))
xc_boys$county_original<-xc_boys$meet_county
xc_boys$county<-xc_boys$meet_county
xc_boys$meet_county<-NULL

#Date, converting into common format
xc_boys$DATE <- as.Date(xc_boys$DATE,format="%m/%d/%Y") # Produces NA when format is not "%m/%d/%Y"
xc_boys$DATE<-as.character(as.character.Date(xc_boys$DATE)) 
#substring(xc_boys$DATE,1,2)="20"
typeof(xc_boys$DATE)

# For other counties
xc_boys$county[xc_boys$county %in% county_names == FALSE & xc_boys$meet_region=="Western Mountain"] <- "Burke"
xc_boys$county[xc_boys$county %in% county_names == FALSE & xc_boys$meet_region=="Coastal"] <- "Lee"
xc_boys$county[xc_boys$county %in% county_names == FALSE & xc_boys$meet_region=="Piedmont"] <- "Beaufort"
typeof(xc_boys$DATE)
View(xc_boys)

#Merging weather and xc_boys
final_eve1 <- left_join(xc_boys,Counties,by=c("DATE","county"))
dim(final_eve1)
View(final_eve1)

#handling records that are not in weather file
final_eve2 <- anti_join(final_eve1,Counties,by=c("DATE","county"))
final_eve2$county[final_eve2$Region=="Western Mountain"] <- "Burke"
final_eve2$county[final_eve2$Region=="Coastal"] <- "Lee"
final_eve2$county[final_eve2$Region=="Piedmont"] <- "Beaufort"
dim(final_eve2)
View(final_eve2)
final_eve2<-final_eve2[,c(1:28)]
final_eve2 <- left_join(final_eve2,Counties,by=c("DATE","county"))
sapply(final_eve2, function(x) sum(is.na(x))) 

#removing records that are not in weather file before merging
final_eve1<-subset(final_eve1,!is.na(HOURLYDRYBULBTEMPF))
dim(final_eve1)
final_eve1<-final_eve1[,-c(29:30)]
final_eve2<-final_eve2[,-c(29:30)]
#Merging
final_eve<-rbind(final_eve1,final_eve2)
dim(final_eve)
sapply(final_eve, function(x) sum(is.na(x))) 
write.csv(final_eve,"C:/Users/saipr/Desktop/Sai Priya/ML Projects/CrossCountry/Data/xc_boys_eve.csv")

final_day$day <- weekdays(as.Date(final_day$DATE,'%Y-%m-%d'))
final_eve$day <- weekdays(as.Date(final_eve$DATE,'%Y-%m-%d'))
dim(final_day)
dim(final_eve)
write.csv(final_day,"C:/Users/saipr/Desktop/Sai Priya/ML Projects/CrossCountry/Data/xc_boys_day.csv")
write.csv(final_eve,"C:/Users/saipr/Desktop/Sai Priya/ML Projects/CrossCountry/Data/xc_boys_eve.csv")

xc_weekday<- read.csv("C:/Users/saipr/Desktop/Sai Priya/ML Projects/CrossCountry/Data/xc_weekday.csv",header=T, na.strings=c("","NA"))
xc_weekend<- read.csv("C:/Users/saipr/Desktop/Sai Priya/ML Projects/CrossCountry/Data/xc_weekend.csv",header=T, na.strings=c("","NA"))
dim(xc_weekday)
dim(xc_weekend)
final_boys_weather<-rbind(xc_weekday,xc_weekend)
dim(final_boys_weather)
write.csv(final_boys_weather,"C:/Users/saipr/Desktop/Sai Priya/ML Projects/CrossCountry/Data/final_boys_weather.csv")

sapply(final_boys_weather, function(x) sum(is.na(x))) 


colnames(final_day)
colnames(xc_weekday)
xc_boys$day <- weekdays(as.Date(xc_boys$DATE,'%Y-%m-%d'))
c<-subset(xc_boys,day=="Sunday")
View(c)
