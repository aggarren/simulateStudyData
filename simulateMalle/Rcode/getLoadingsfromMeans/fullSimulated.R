#RUN AFTER getRaw.R
library(readr)

getData <- function(fileName) {
  file <- read.csv(fileName)
  file <- as.matrix(file)
  file <- matrix(as.numeric(file),ncol=ncol(file))
  colnames(file) <- paste0(fileName," participant ",seq(ncol(file)))
  return(file)
}

items <- read.csv("malle averages.csv")[,1]
fullData <- cbind(getData("3robot.csv"),getData("5child.csv"))
fullData <- cbind(fullData,getData("7adult.csv"))
fullData <- as.data.frame(fullData)
fullData <- cbind(items,fullData)
write.csv(fullData,"simulatedDataMalle.csv")

#next run pacOnRawData.R
