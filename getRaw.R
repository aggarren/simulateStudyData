#NEED "dweck averages.csv" in working directory
library(readr)
library(faux)
library(MCMCglmm)

standardDeviation <- function(mean, n, high) {
  sd <- (sqrt(n)*(high-mean))/1.96
  return(sd)
}

getN <- function(c){
  n <- 0
  if(c== 3) { n <-17}
  if(c== 5) { n <-23 }
  if(c== 7) { n <-22 }
  if(c== 9) { n <-22 }
  if(c== 11) { n <-20 }
  if(c== 13) { n <-19 }
  if(c== 15) { n <-20 }
  if(c== 17) { n <-20 }
  if(c== 19) { n <-21 }
  if(c== 21) { n <-18 }
  if(c== 23) { n <-20 }
  if(c== 25) { n <-24 }
  if(c== 27) { n <-20 }
  if(c== 29) { n <-22 }
  if(c== 31) { n <-21 }
  if(c== 33) { n <-21 }
  if(c== 35) { n <-20 }
  if(c== 37) { n <-21 }
  if(c== 39) { n <-19 }
  if(c== 41) { n <-20 }
  if(c== 43) { n <-21 }
  return(n)
}

character = 3
avgs <- read.csv("dweck averages.csv")
avgs <- data.matrix(avgs)
avgs <- matrix(as.numeric(avgs),ncol=ncol(avgs))
while(character <=43) {
  n <- getN(character)
  means <- avgs[,character]
  sds <- c()
  for(i in 1:40){
    sds <- c(sds, standardDeviation(means[i],n,avgs[i,character+1]))
  }
  if(character == 43) { corr <- cor(avgs[,character],avgs[,character-2])}
  if(character != 43) { corr <- cor(avgs[,character],avgs[,character+2]) }
  matrix <- rnorm_multi(n=n,mu=means,sd=sds,r=corr)
  matrix <- t(matrix)
  newMatrix <- c()
  for(i in 1:n){
    newMatrix <- cbind(newMatrix, norm2trunc(matrix[,i],min=0,max=6))
  }
  matrix <- newMatrix
  matrix <- round(matrix,0)
  c <- character
  if(c== 3) { write.table(matrix,"3stapler.csv",row.names=FALSE,sep=",")}
  if(c== 5) {write.table(matrix,"5car.csv",row.names=FALSE,sep=",")}
  if(c== 7) {write.table(matrix,"7computer.csv",row.names=FALSE,sep=",")}
  if(c== 9) {write.table(matrix,"9robot.csv",row.names=FALSE,sep=",")}
  if(c== 11) {write.table(matrix,"11microbe.csv",row.names=FALSE,sep=",") }
  if(c== 13) { write.table(matrix,"13beetle.csv",row.names=FALSE,sep=",")}
  if(c== 15) { write.table(matrix,"15fish.csv",row.names=FALSE,sep=",") }
  if(c== 17) { write.table(matrix,"17bluejay.csv",row.names=FALSE,sep=",") }
  if(c== 19) {write.table(matrix,"19frog.csv",row.names=FALSE,sep=",")}
  if(c== 21) { write.table(matrix,"21mouse.csv",row.names=FALSE,sep=",") }
  if(c== 23) { write.table(matrix,"23goat.csv",row.names=FALSE,sep=",") }
  if(c== 25) { write.table(matrix,"25dog.csv",row.names=FALSE,sep=",") }
  if(c== 27) { write.table(matrix,"27bear.csv",row.names=FALSE,sep=",")}
  if(c== 29) { write.table(matrix,"29dolphin.csv",row.names=FALSE,sep=",") }
  if(c== 31) { write.table(matrix,"31elephant.csv",row.names=FALSE,sep=",") }
  if(c== 33) {write.table(matrix,"33chimp.csv",row.names=FALSE,sep=",")}
  if(c== 35) { write.table(matrix,"35fetus.csv",row.names=FALSE,sep=",") }
  if(c== 37) { write.table(matrix,"37pvs.csv",row.names=FALSE,sep=",")}
  if(c== 39) { write.table(matrix,"39infant.csv",row.names=FALSE,sep=",") }
  if(c== 41) { write.table(matrix,"41child.csv",row.names=FALSE,sep=",")}
  if(c== 43) { write.table(matrix,"43adult.csv",row.names=FALSE,sep=",") }
  
    character = character + 2
  
}
#next run fullSimulated.R