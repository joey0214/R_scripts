library(ggmap)
library(mapproj)


gxmap <- get_map(location = 'Guangxi', zoom = 6)
ggmap(gxmap)



hjdata <- read.table("~/Desktop/tmpdData/guilin.csv", header=FALSE, sep=",")
names(data) <- c('name','lan','lon')
data <- hjdata[c('V1','V2','V3')]
data$lan <- as.numeric(data$lan)
data$lon <- as.numeric(data$lon)
landata <- as.numeric(hjdata$V2)
londata <- as.numeric(hjdata$V3)

ggmap(gxmap <- get_map(location = 'Guangxi', zoom = 6))+
  geom_point(data=data, aes(x=lon,y=lan),colour = 'red',alpha=0.7)

