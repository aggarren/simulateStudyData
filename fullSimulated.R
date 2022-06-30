#RUN AFTER getRaw.R
library(readr)

getData <- function(fileName) {
  file <- read.csv(fileName)
  file <- as.matrix(file)
  file <- matrix(as.numeric(file),ncol=ncol(file))
  colnames(file) <- paste0(fileName," participant ",seq(ncol(file)))
  return(file)
}

items <- read.csv("dweck averages.csv")[,1]
fullData <- cbind(getData("3stapler.csv"),getData("5car.csv"))
fullData <- cbind(fullData,getData("7computer.csv"))
fullData <- cbind(fullData,getData("9robot.csv"))
fullData <- cbind(fullData,getData("11microbe.csv"))
fullData <- cbind(fullData,getData("13beetle.csv"))
fullData <- cbind(fullData,getData("15fish.csv"))
fullData <- cbind(fullData,getData("17bluejay.csv"))
fullData <- cbind(fullData,getData("19frog.csv"))
fullData <- cbind(fullData,getData("21mouse.csv"))
fullData <- cbind(fullData,getData("23goat.csv"))
fullData <- cbind(fullData,getData("25dog.csv"))
fullData <- cbind(fullData,getData("27bear.csv"))
fullData <- cbind(fullData,getData("29dolphin.csv"))
fullData <- cbind(fullData,getData("31elephant.csv"))
fullData <- cbind(fullData,getData("33chimp.csv"))
fullData <- cbind(fullData,getData("35fetus.csv"))
fullData <- cbind(fullData,getData("37pvs.csv"))
fullData <- cbind(fullData,getData("39infant.csv"))
fullData <- cbind(fullData,getData("41child.csv"))
fullData <- cbind(fullData,getData("43adult.csv"))
fullData <- as.data.frame(fullData)
fullData <- cbind(items,fullData)
write.csv(fullData,"simulatedDataDweck.csv")

#next run pacOnRawData.R
