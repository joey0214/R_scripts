#this is want map HMFD infection pop data into the china map.  use ggplot2 to draw china map 
#version:2013-06-30
#author:joey

library(ggplot2)
library(plyr)
library(maptools)

#read map shape file
provincemap <- readShapePoly("./bou2_4p.shp")

x <- provincemap@data
xs <- data.frame(x,id=seq(0:924)-1)

#convert to dataframe
china.map1 <-fortify(provincemap)

#join two dataframe
china.mapdata <- join(china.map1,xs,type="full")

NAME<-pop2012[,2]

province.pop.dataframe <-data.frame(NAME,province.pop)

china.infect <-join(china.mapdata,province.pop.dataframe)

ggplot(china.mapdata, aes(x = long, y = lat, group = group,fill=province.pop.dataframe))+
  geom_polygon( )+
  geom_path(colour = "grey40")+
  scale_fill_manual(values=colours(),guide=FALSE)

