#this is want map HMFD infection pop data into the china map 
#version:2013-06-27
#author:joey

library(maptools)
library(maps)

#read map shape file
provincemap <- readShapePoly("./bou2_4p.shp")

#define a function how get color in map
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
pop2012 <-read.csv("./2012年.csv",header=FALSE)

#get province name
province.name <-pop2012[,1]

#get infection pop
province.pop <- pop2012[,2]
###or can do as this 
#pop2012.rescale <- rescale(pop2012[,2],to=c(0,1))
#province.col.rescale=rgb(red=1-pop2012.rescale,green=0,blue=0)
##似乎还是不能解决多图中值的颜色衡量？？？


#set province color based pop
province.col=rgb(red=1-province.pop/max(province.pop),green=0,blue=0)

#draw map 
plot(provincemap,col=getColor(provincemap,province.name,province.col,"white"),xlab="",ylab="")

#=============================
#build a matrix,row is month, col is year
row.names <- c("January","February","March","April","May","June","July","August","September","October","November","December","All")
col.manes <- c("2005","2006","2007","2008","2009","2010","2011","2012")
infection.matrix <-matrix(,nrow=13,ncol=8,dimnames=list(row.names,col.manes))

#read 8 year infection from source data in csv
infection2005 <-read.csv("./2005year.infect.csv",header=TRUE)
infection2006 <-read.csv("./2006year.infect.csv",header=TRUE)
infection2007 <-read.csv("./2007year.infect.csv",header=TRUE)
infection2008 <-read.csv("./2008year.infect.csv",header=TRUE)
infection2009 <-read.csv("./2009year.infect.csv",header=TRUE)
infection2010 <-read.csv("./2010year.infect.csv",header=TRUE)
infection2011 <-read.csv("./2011year.infect.csv",header=TRUE)
infection2012 <-read.csv("./2012year.infect.csv",header=TRUE)

#cat year data into a list
infect.list <-list(infection2005,infection2006,infection2007,infection2008,infection2009,infection2010,infection2011,infection2012)


layout(matrix(1:104,13,8))



for (mydata in infect.list){print mydata}
