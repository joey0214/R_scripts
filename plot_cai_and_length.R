##setwd("~/research/s.cere/cai/")

##single operation
#cai_len <- read.table("./cai_orf_output_90-300.csv", header=FALSE, sep=",")
##plot in whole genome
#plot(cai_len$V1, cai_len$V2, type="p",main="Plot CAI-length in whole genome", xlab="altORFs length", ylab="codon adaptation index")


##for batch operation
##read args from command line
args <- commandArgs(TRUE)
cai_len <- read.table(args[1], header=FALSE, sep=",")

chr_name <- substr(args[1], 0, nchar((args[1]))-4)

##open a new plot window
plot.new()
##set up out file name
file_name <- paste(chr_name, ".png")
png(file_name)
##plot
plot(cai_len$V1, cai_len$V2, type="p",main=paste("Plot CAI-length in ", chr_name), xlab="altORFs length", ylab="codon adaptation index")
##turn off the plot device
dev.off()
