library(maptools)
library(ggplot2)
library(gpclib)

input<- read.csv("ambulance_assault.csv")

head(input)

p_ass<- ggplot(input, aes(x=assault_09_11))

p_ass+ geom_histogram()

p_ass+geom_histogram(binwidth=10)+ geom_density(fill=NA, colour="black")

p2_ass<- ggplot(input, aes(x=assault_09_11, y=..density..))

p2_ass +geom_histogram() + geom_density(fill=NA,colour="red")

p2_ass +geom_histogram(binwidth=10) + geom_density(fill=NA,colour="red")

input[which(input$assault_09_11>750), ]

p3_ass<- ggplot(input, aes(x=Bor_code, y=assault_09_11))

p3_ass+geom_boxplot()

p3_ass<- ggplot(input, aes(x=Bor_Code, y=assault_09_11))

p3_ass+geom_boxplot()

p3_ass +geom_boxplot()+ coord_flip()

p3_ass + geom_histogram()+ facet_wrap(~Bor_Code)
