#study3
avgs <- read.csv("dweckStudy3Averages.csv")
avgs <- data.matrix(avgs)
avgs <- matrix(as.numeric(avgs),ncol=ncol(avgs))

standardDeviation <- function(mean, n, high) {
  sd <- (sqrt(n)*(high-mean))/1.96
  return(sd)
}
forChar <- function(character){
  n <- 400
  means <- avgs[,character]
  sds <- c()
  for(i in 1:40){
    sds <- c(sds, standardDeviation(means[i],n,avgs[i,character+1]))
  }
  if(character == 3) { corr <- cor(avgs[,character],avgs[,character-2])}
  if(character == 1) { corr <- cor(avgs[,character],avgs[,character+2]) }
  matrix <- rnorm_multi(n=n,mu=means,sd=sds,r=corr)
  matrix <- t(matrix)
  newMatrix <- c()
  for(i in 1:n){
    newMatrix <- cbind(newMatrix, norm2trunc(matrix[,i],min=0,max=6))
  }
  matrix <- newMatrix
  matrix <- round(matrix,0)
  return(matrix)
}
robotData <- forChar(1)
beetleData <- forChar(3)
write.csv(robotData,"robotRaw.csv")
write.csv(beetleData,"beetleRaw.csv")