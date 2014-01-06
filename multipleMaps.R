library(maptools)
library(maps)

#read map data from source file
provincemap <- readShapePoly("./bou2_4p.shp")

#set the function draw province color
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
pop2011 <-read.csv("./2011year.infect.csv",header=TRUE)
pop2010 <-read.csv("./2010year.infect.csv",header=TRUE)
pop2009 <-read.csv("./2009year.infect.csv",header=TRUE)
pop2008 <-read.csv("./2008year.infect.csv",header=TRUE)
pop2007 <-read.csv("./2007year.infect.csv",header=TRUE)
pop2006 <-read.csv("./2006year.infect.csv",header=TRUE)
pop2005 <-read.csv("./2005year.infect.csv",header=TRUE)

datalist = c("pop2005","pop2006","pop2007","pop2008","pop2009","pop2010","pop2011","pop2012")
monthlist <- c("January","February", "March"," April",  "May",  "June", "July", "August", "September", "October", "November", "December",   "All")
max.month=0
max.year =0

#for loop to get the maximum of month and year
for (yearpop in datalist){
  yearpop <- eval(as.name(yearpop))
  
  if(max.month < max(yearpop[,2:13])) {
    max.month <- max(yearpop[,2:13])}
      
  if(max.year < max(yearpop[,14])) {
    max.year <- max(yearpop[,14])}
}
plot.new() 
# mar=rep(0,4) can solve "Error in frame() : figure margins too large"
par(mfcol=c(13,8),mar=rep(0,4))
#draw map of every year and monthly 
for (yearpop in datalist){
  #assign(yearpop)
  yearpop <- eval(as.name(yearpop))
  for (i in 1:length(monthlist)){
    province.name <-yearpop[,1]
    province.pop <- yearpop[,i+1]
    if (i==13){province.col=rgb(red=1-province.pop/max.year,green=0,blue=0)}
    else {province.col=rgb(red=1-province.pop/max.month,green=0,blue=0)}
    
    plot(provincemap,col=getColor(provincemap,province.name,province.col,"white"),xlab="",ylab="",titlt=monthlist[i])
  }
}

# #get province name
# province.name <-pop2012[,1]
# 
# #get infection pop
# province.pop <- pop2012[,2]
# province.col=rgb(red=1-province.pop/max(province.pop),green=0,blue=0)
# 
# #draw map 
# plot(provincemap,col=getColor(provincemap,province.name,province.col,"white"),xlab="",ylab="")


par(mfrow=c(4,2),mar=rep(0,4))
#draw map of every year and monthly 
for (yearpop in datalist){
  #assign(yearpop)
  yearpop <- eval(as.name(yearpop))
  province.name <-yearpop[,1]
  province.pop <- yearpop$All
  
  province.col=rgb(red=1-province.pop/max.year,green=0,blue=0)
  
  plot(provincemap,col=getColor(provincemap,province.name,province.col,"white"),xlab="",ylab="",titlt=monthlist[i])
  
}



for (index in test1){
  index
}

ceiling(province.pop/(max.year/1024))