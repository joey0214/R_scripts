library(maptools)
library(ggplot2)
library(gpclib)

setwd("/home/zhongxf/research/rProject/tmp0517/R-ggplot2-data")
sport <- readShapePoly("london_sport.shp")
names(sport)

p<- ggplot(sport@data,aes(Partic_Per,Pop_2001))

p+geom_point()

p+geom_point(colour="red", size=2)

p+geom_point(aes(colour=Partic_Per, size=Pop_2001))

p+geom_point(aes(colour=Partic_Per, size=Pop_2001))+geom_text(size=2, aes(label=name))

gpclibPermit()

sport_geom <- fortify(sport, region="ons_label")

sport_geom<- merge(sport_geom, sport@data, by.x="id",
                   by.y="ons_label")


head(sport_geom)

Map<- ggplot(sport_geom,aes(long, lat, group=group, fill=Partic_Per))+ geom_polygon()+ coord_equal()+ labs(x="Easting (m)", y="Northing (m)", fill= "% Sport Partic.")+ ggtitle("London Sports Particsipation")

Map

Map + scale_fill_gradient(low="blue", high="red")