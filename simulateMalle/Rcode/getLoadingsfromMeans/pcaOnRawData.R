#RUN AFTER getRaw.R and fullSimulated.R
library(readr)
library(psych)
library(stats)
performPCA <- function(data){
  pca <- principal(data,nfactors=3,rotate="varimax")
  return(pca)
}

fullData <- read.csv("simulatedDataMalle.csv")
fullData <- fullData[,3:ncol(fullData)]
fullData <- as.matrix(fullData)
fullData <- matrix(as.numeric(fullData),ncol=ncol(fullData))
fullData <- t(fullData)
pca <- performPCA(fullData)
pca$loadings
loadings <- c(pca$loadings)
loadings <- matrix(loadings,nrow=40,ncol=3)
items <- as.data.frame(read.csv("simulatedDataMalle.csv")[,2])
colnames(items) <- c("items")
loadingsSimulated <- cbind(items,loadings)
write.csv(loadingsSimulated,"loadingsSimulated.csv")

#compare simulated loadings to actual loadings
realLoadings <- read.csv("malle loadings.csv")
realLoadings <- as.matrix(realLoadings)
realLoadings <- t(realLoadings)
realLoadings <- t(realLoadings)
comparison <- realLoadings - loadings
write.csv(comparison,"comparingLoadings.csv")
