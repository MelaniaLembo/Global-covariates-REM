# check date coherence between first few days of July
# set everything to one measurements every hour 
# (dropped intermediate measures)

# July 8th is missing data for 16:52 so we consider data from July
# 9th to the end of the month

weather_Jul<- NULL
days<-seq(9,31)

for (k in days) {
  
  if(k<10){
    weather_currentday<-read.csv(paste("../raw_data/weather_Jul0",as.character(k),"2023.csv", sep = ""))
    weather_currentday$Date<- as.Date(paste("2023-07-0",as.character(k), sep = ""))
  }else{
    weather_currentday<-read.csv(paste("../raw_data/weather_Jul",as.character(k),"2023.csv", sep = ""))
    weather_currentday$Date<- as.Date(paste("2023-07-",as.character(k), sep = ""))
  }
  
  weather_currentday$DateTime<-as.POSIXct(paste(weather_currentday$Date,weather_currentday$Time, sep = " "),format="%Y-%m-%d %I:%M %p")
  if(nrow(weather_currentday)>24){
    # dropping intermediate measurements
    weather_currentday <- weather_currentday[which(minute(ymd_hms(weather_currentday$DateTime))==52),]
    #print(nrow(weather_currentday))
  }
  
  weather_Jul<-rbind(weather_Jul,weather_currentday)
  
}

weather_Jul<-weather_Jul[,!names(weather_Jul) %in% c("Time", "Date")]
rownames(weather_Jul)<-NULL

# removing useless columns (wind gust, dew point and wind direction)
weather_Jul<-weather_Jul[,-which(names(weather_Jul) %in% c("Wind", "Wind.Gust", "Dew.Point"))]
weather_Jul$Condition<-as.factor(weather_Jul$Condition)

# handle other columns that are chr because of unit measure
# separate from unit and convert to numeric
remove_temp_um<-function(x){
  as.numeric(unlist(strsplit(x, " °C")))
}

remove_hum_um<-function(x){
  unname(as.numeric(unlist(strsplit(x, " %"))))
}

remove_windsp_um<-function(x){
  unname(as.numeric(unlist(strsplit(x, " km/h"))))
}

remove_pressure_um<-function(x){
  s1<-unname(unlist(strsplit(x, ",")))
  s2<-unname(unlist(strsplit(s1[2], " hPa")))
  s12<-paste(s1[1],s2, sep ="")
  unname(as.numeric(s12))
}

remove_prec_um<-function(x){
  unname(as.numeric(unlist(strsplit(x, " mm"))))
}

weather_Jul$Temperature<-sapply(weather_Jul$Temperature, remove_temp_um)
weather_Jul$Humidity<-sapply(weather_Jul$Humidity, remove_hum_um)
weather_Jul$Wind.Speed<-sapply(weather_Jul$Wind.Speed,remove_windsp_um)
weather_Jul$Pressure<-sapply(weather_Jul$Pressure,remove_pressure_um)
weather_Jul$Precip.<-sapply(weather_Jul$Precip.,remove_prec_um)


rm(list = c("weather_currentday","k", "days"))
