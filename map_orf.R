setwd("~/research/s.cere/")
setwd("~/research/s.cere/chromosome/")

sourceData <- read.table("./Coordinates_in_orfs_map.csv", header=FALSE, sep=",")

print length(sourceData.t[1,])

getColor<-function(x){
  if (x == -1){
    return ("blue")
  }
  else if (x == 1){
    return ("red")
  } 
}
