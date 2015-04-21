setwd("~/research/smithData/pca/")

altorf <- read.table("47altorf.csv", header=TRUE, sep="\t")

# Pricipal Components Analysis
# entering raw data and extracting PCs 
# from the correlation matrix 
fit <- princomp(altorf[,1-2], cor=TRUE)
summary(fit) # print variance accounted for 
loadings(fit) # pc loadings 
plot(fit,type="lines") # scree plot 
fit$scores # the principal components
biplot(fit)

# Maximum Likelihood Factor Analysis
# entering raw data and extracting 3 factors, 
# with varimax rotation 
fit <- factanal(altorf[,1-2], 3, rotation="varimax")
print(fit, digits=2, cutoff=.3, sort=TRUE)
# plot factor 1 by factor 2 
load <- fit$loadings[,1:2] 
plot(load,type="n") # set up plot 
text(load,labels=names(altorf[,1-2]),cex=.7) # add variable names

library(psych)
install.packages("psych")
fit <- principal(altorf[,1-2], nfactors=6, rotate="varimax")
fit # print results

library(nFactors)
ev <- eigen(cor(altorf[,1-2])) # get eigenvalues
ap <- parallel(subject=nrow(altorf[,1-2]),var=ncol(altorf[,1-2]),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)


library(FactoMineR)
result <- PCA(altorf[,1-2]) # graphs generated automatically

# Simple CFA Model
library(sem)
mydata.cov <- cov(mydata)
model.mydata <- specify.model() 
F1 ->  X1, lam1, NA
F1 ->  X2, lam2, NA 
F1 ->  X3, lam3, NA 
F2 ->  X4, lam4, NA 
F2 ->  X5, lam5, NA 
F2 ->  X6, lam6, NA 
X1 <-> X1, e1,   NA 
X2 <-> X2, e2,   NA 
X3 <-> X3, e3,   NA 
X4 <-> X4, e4,   NA 
X5 <-> X5, e5,   NA 
X6 <-> X6, e6,   NA 
F1 <-> F1, NA,    1 
F2 <-> F2, NA,    1 
F1 <-> F2, F1F2, NA

mydata.sem <- sem(model.mydata, mydata.cov, nrow(mydata))
# print results (fit indices, paramters, hypothesis tests) 
summary(mydata.sem)
# print standardized coefficients (loadings) 
std.coef(mydata.sem)
