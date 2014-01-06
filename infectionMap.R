# 设置工作路径
setwd("/home/zhongxf/research/rProject/july")

#加载所需的包
library(maptools)
library(maps)

#读取地图信息
#read map data from source file
provincemap <- readShapePoly("./bou2_4p.shp")

##设置绘制地图的颜色函数
#set the function draw province color
getColor=function(mapdata,provname,provcol,othercol)
{
  f=function(x,y) ifelse(x %in% y,which(y==x),0);
  # iconv(mapdata@data$NAME,from="CP936",to="UTF-8")  is to set convert province name
  # in map data to UTF8, so province names can recogine in UBUNTU
  #为了正确显示中文省名，添加iconv(mapdata@data$NAME,from="CP936",to="UTF-8")
  colIndex=sapply(iconv(mapdata@data$NAME,from="CP936",to="UTF-8"),f,provname);
  col=c(othercol,provcol)[colIndex+1];
  return(col);
}

#读取每年全国各省HMFD感染数据
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
max.month=0  #年统计中省感染统计中最大值
max.year =0  #月统计中省感染统计中的最大值

#for循环获得两个最大值
#for loop to get the maximum of month and year
for (yearpop in datalist){
  yearpop <- eval(as.name(yearpop))
  
  if(max.month < max(yearpop[,2:13])) {
    max.month <- max(yearpop[,2:13])}
      
  if(max.year < max(yearpop[,14])) {
    max.year <- max(yearpop[,14])}
}

#plot.new() 
# mar=rep(0,4) can solve "Error in frame() : figure margins too large"
#par(mfcol=c(13,8),mar=rep(0,4))
#draw map of every year and monthly 
#for循环绘制地图，每年的数据绘制一张地图，并输出到指定路径
for (yearpop in datalist){
  year.range<- c(2005,2006,2007,2008,2009,2010,2011,2012)
month.range <- c(01,02,03,04,05,06,07,08,09,10,11,12)

  year <- substr(yearpop,4,8)  #年份信息

  #assign(yearpop)
  yearpop <- eval(as.name(yearpop))  #从字符串转换成变量名
  color.year.region <- palette(topo.colors(1024, alpha = 1))  #配置调色板
  for (i in 1:length(monthlist)){
    province.name <-yearpop[,1]
    province.pop <- c(yearpop[,i+1])
    if (i==13){  #此判断为年统计信息
      plot.new()  #为绘制新图形借宿当前图形窗口，防止绘制成网格图
      #filename <- "./year/year.range[i].svg"
      filename <- paste("./year/",year,".svg",separate="",collapse = NULL)  #设置输出文件的文件名
      svg(filename) #生成输出文件
      province.col=color.year.region[abs((ceiling(province.pop/(max.month/1024)))-1024)]  #计算各省的颜色
      plot(provincemap,col=getColor(provincemap,province.name,province.col,"white"),xlab="",ylab="",main<-year)  #绘制地图
      dev.off()  #关闭输出设备
    }
    else {  #此为月统计信息
      month <- month.range[i]
      plot.new()
      #filename <- "./year/year.range[i].svg"
      filename <- paste("./month/",year,month,".svg",separate="")
      svg(filename)
      province.col=color.year.region[abs((ceiling(province.pop/(max.month/1024)))-1024)]
      plot(provincemap,col=getColor(provincemap,province.name,province.col,"white"),xlab="",ylab="",main <-paste(year,month,separate=""))
      dev.off()
    }
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


# par(mfrow=c(4,2),mar=rep(0,4))
# #draw map of every year and monthly 
# for (yearpop in datalist){
#   #assign(yearpop)
#   yearpop <- eval(as.name(yearpop))
#   province.name <-yearpop[,1]
#   province.pop <- yearpop$All
  
#   province.col=rgb(red=1-province.pop/max.year,green=0,blue=0)
  
#   plot(provincemap,col=getColor(provincemap,province.name,province.col,"white"),xlab="",ylab="",titlt=monthlist[i])
  
# }



# for (index in test1){
#   index
# }

# ceiling(province.pop/(max.year/1024))
