library(maps)
library(mapdata)

map("world")

ev71world <-read.csv("ev71seqinWorld.csv'")
country <-ev71world[,1]
pop <-ev71world[,2]

getColor=function(mapdata,provname,provcol,othercol)
{
  f=function(x,y) ifelse(x %in% y,which(y==x),0);
  colIndex=sapply(mapdata@data$NAME,f,provname);
  col=c(othercol,provcol)[colIndex+1];
  return(col);
}

provcol=rgb(red=1-pop/max(pop)/2,green=1-pop/max(pop)/2,blue=0);

