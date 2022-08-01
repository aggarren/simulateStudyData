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
  if(c== 3) { n <-138}
  if(c== 5) { n <-138 }
  if(c== 7) { n <-139 }
  return(n)
}

character = 3
avgs <- read.csv("malle averages.csv")
avgs <- data.matrix(avgs)
avgs <- matrix(as.numeric(avgs),ncol=ncol(avgs))
while(character <=7) {
  n <- getN(character)
  means <- avgs[,character]
  sds <- c()
  for(i in 1:16){
    sds <- c(sds, standardDeviation(means[i],n,avgs[i,character+1]))
  }
  if(character == 7) { corr <- cor(avgs[,character],avgs[,character-2])}
  if(character != 7) { corr <- cor(avgs[,character],avgs[,character+2]) }
  matrix <- rnorm_multi(n=n,mu=means,sd=sds,r=corr)
  matrix <- t(matrix)
  newMatrix <- c()
  for(i in 1:n){
    newMatrix <- cbind(newMatrix, norm2trunc(matrix[,i],min=0,max=6))
  }
  matrix <- newMatrix
  matrix <- round(matrix,0)
  c <- character
  if(c== 3) { write.table(matrix,"3robot.csv",row.names=FALSE,sep=",")}
  if(c== 5) {write.table(matrix,"5child.csv",row.names=FALSE,sep=",")}
  if(c== 7) {write.table(matrix,"7adult.csv",row.names=FALSE,sep=",")}
  
    character = character + 2
  
}
#next run fullSimulated.R