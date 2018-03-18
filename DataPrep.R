library(dplyr)
sun<-read.csv('airlinedelaycauses_DelayedFlights.csv')
airline<-read.csv('Airport_Codes_mapped_to_Latitude_Longitude_in_the_United_States.csv')
rownames(airline) <- airline$locationID
airline[, 2:3] <- sapply(airline[, 2:3], as.numeric)

airline$Longitude <- paste("-",airline$Longitude,sep="")
airline$Latitude <- paste(airline$Latitude,sep="")
write.csv(airline,"airport_loc.csv")

busy_airport<-c('ATL','LAX','ORD','DFW','JFK','DEN','SFO')
marjor_airport<-c('ATL','LAX','ORD','DFW','JFK','DEN','SFO','LAS','CLT','SEA','PHX','MIA','MCO','IAH','EWR','MSP','BOS','DTW','PHL','LGA')
major_airline<-c('DL','UA','WN','VX','AA')
major<-sun%>%subset(Origin%in%busy_airport&UniqueCarrier%in%major_airline&DEST%in%marjor_airport)

#About Putting latitude and longtitude
origin<-airline
dest<-airline
colnames(origin)[1]<-'Origin'
colnames(dest)[1]<-'Dest'
mysample <- major
mysample <- merge(mysample, dest, by = 'Dest')
mysample <- (merge(mysample, origin, by = 'Origin'))
colnames(mysample)[31:32]<-c('Start_Latitude','Start_Longitude')
colnames(mysample)[33:34]<-c('End_Latitude','End_Longitude')
mysample$X<-NULL
any(is.na(mysample[,31:34]))
write.csv(mysample,"travel_loc.csv")


airportdelay<-major%>%select(Origin,Dest,Month,DayOfWeek,UniqueCarrier,ArrDelay)%>%group_by(Origin,Dest,Month,UniqueCarrier,DayOfWeek)%>%summarise(avg = mean(ArrDelay,na.rm = TRUE),n=n())

