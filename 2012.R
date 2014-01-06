library(maptools)
library(maps)

provincemap <- readShapePoly("./bou2_4p.shp")

getColor=function(mapdata,provname,provcol,othercol)
{
  f=function(x,y) ifelse(x %in% y,which(y==x),0);
  # iconv(mapdata@data$NAME,from="CP936",to="UTF-8")  is to set convert province name
  # in map data to UTF8, so province names can recogine in UBUNTU
  colIndex=sapply(iconv(mapdata@data$NAME,from="CP936",to="UTF-8"),f,provname);
  col=c(othercol,provcol)[colIndex+1];
  return(col);
}
#read in source data in csv format
pop2012 <-read.csv("./2012year.infect.csv",header=TRUE)

#get province name
province.name <-pop2012[,1]

#get infection pop
province.pop <- pop2012[,2]
province.col=rgb(red=1-province.pop/max(province.pop),green=0,blue=0)

#draw map 
plot(provincemap,col=getColor(provincemap,province.name,province.col,"white"),xlab="",ylab="")
