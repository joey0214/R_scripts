##this script wrote for calculating correlation between site variation and symptom

setwd("~/research/enterovirus71/ev71/2013dec/88genomes_fsm")

#sourceData <- read.table("./fsm_sites_variation_filtered_splited.csv", header=FALSE, sep=",")
sourceData <- read.table("./fsm_sites_variation_2.csv", header=TRUE, sep=",")


fatalSevere <- sourceData$fatal +sourceData$severe
severeMild <- sourceData$severe + sourceData$mild
fatal <- sourceData$fatal

sourceData.2 <-cbind(sourceData, fatalSevere, severeMild, fatal)

sourceData.t = t(sourceData)
columnLen = length(sourceData.t[,1])
rowLen = length(sourceData.t[1,])

pp1 <- c(12/88,35/88,41/88)   # fatal---severe---mild
pp2 <- c(41/88, 47/88)        # mild---fatal+severe
pp3 <-c(12/88,76/88)          #fatal---severe+mild
pp1.result<- numeric()
pp2.result<- numeric()
pp3.result<- numeric()
for (i in 1:rowLen){
  
    pp1.result <-c(pp1.result, chisq.test(sourceData.2[i,2:4],p=pp1,simulate.p.value = TRUE)$p.value)
    pp2.result <-c(pp2.result, chisq.test(sourceData.2[i,4:5],p=pp2,simulate.p.value = TRUE)$p.value)
    pp3.result <-c(pp3.result, chisq.test(sourceData.2[i,6:7],p=pp3,simulate.p.value = TRUE)$p.value)

}
result <- t(rbind(pp1.result,pp2.result,pp3.result))
write.csv(result, "pvalue2.csv",eol="\n")
write.csv(sourceData.2, "sites.csv",eol="\n")