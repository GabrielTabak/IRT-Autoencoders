## Creating replicas for the first simulation

library(mirt)
library(ggplot2)
library(ggplot2)
library(tidyr)

MakeParams <- function(nItems, nExaminees){
  set.seed(nExaminees+nItems+1) 
  
    ax <- runif(nItems-1,0.9,2.1)
    a <- exp(log(ax) - mean(log(ax))) #prod = 1
    a <- c(a,1) # Last Item = 1
    
    by <- runif(nItems-1, -3,3)
    b <- by - mean(by) # sum = 0
    b <- c(b,0) # Last Item = 0
    
    thetas <- rnorm(nExaminees) # Normal(0,1) 
    thetas <- (thetas - mean(thetas))/sd(thetas)
  
  return(list("a" = a,
              "b" = b,
              "thetas" = thetas))
}

MakeData <- function(Replica, nItems, nExaminees, seed, file){
  
  set.seed(seed)
  U <- simdata(Replica$a,Replica$b,
               itemtype = "dich",
               Theta = Replica$thetas)
  
  write.csv(U, paste("DataEvo/",
                     file,
                     sep = ""),row.names = F)
  
}


# Simulate data
nItems <- 50
nExaminees <- 1000

Replica1 <- MakeParams(nItems, nExaminees)

for (i in 1:100) { # Replicas
  file <- paste("Data_", i, ".csv",sep = "")
  MakeData(Replica1, c, b, 3*i+15, file)
}
 
rm(file); rm(i)



