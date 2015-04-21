setwd("~/research/my_predictedORFs/whole_genome_prediction/")

plotData <- read.table("plot_size_2.csv", header=FALSE, sep="\t")
plotData2 <- read.table("out_file.csv", header=FALSE, sep="\t")
barplot(plotData$V1,plotData$V2)

barplot(table(plotData$V2,plotData$V1), main="Car Distribution by Gears and VS",
        xlab="Number of Gears", col=c("darkblue","red"),
        legend = rownames(plotData$V3))


out_file.csv