library(tidyverse)
library(maps)
library(geosphere)
par(mar=c(0,0,0,0))
map('state',col="#f2f2f2", fill=TRUE, bg="white", lwd=0.05,mar=rep(0,4))
Start<-'LAX'
Des<-c('MSP','BOS','DFW','PHL')

for (i in (1:length(Des))){
  data=rbind(airline[Start,][,2:3],airline[Des[i],][,2:3]) %>% as.data.frame()
  inter <- gcIntermediate(c(as.numeric(data[1,1]),as.numeric(data[1,2])), 
                          c(as.numeric(data[2,1]),as.numeric(data[2,2])),n=50, addStartEnd=TRUE, breakAtDateLine=F)             
  lines(inter, col="slateblue", lwd=2)
  points(x=data$Longitude, y=data$Latitude,  cex=3, pch=20,col=c('#333274','#ff0043'))
  text(rownames(data), x=as.numeric(data$Longitude), y=as.numeric(data$Latitude),  col="slateblue", cex=1, pos=4)
}


# MSP=c(-93.2217,44.8819)
# LAX = c(-118.4072,33.9425)
# DFW=c(-97.0381,32.8969)
data=rbind(MSP,LAX) %>% as.data.frame()
colnames(data)=c("long","lat")
inter <- gcIntermediate(LAX,MSP, n=50, addStartEnd=TRUE, breakAtDateLine=F)             
lines(inter, col="slateblue", lwd=2)
points(x=data$long, y=data$lat,  cex=3, pch=20,col=c('#333274','#ff0043'))
text(rownames(data), x=data$long, y=data$lat,  col="slateblue", cex=1, pos=4)


