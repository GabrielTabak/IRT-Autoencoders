#######################################
library(mirt)
library(tidyr)
library(dplyr)
library(ggplot2)
library(rstan)
save.image("Results.rdata")

rm(list = ls())

MakeParams <- function(nItems, nExaminees){
  set.seed(nExaminees+nItems+1) 
  
  ax <- runif(nItems-1,0.9,2.1)
  a <- exp(log(ax) - mean(log(ax))) #prod = 1
  a <- c(a,1) # Last Item = 1
  
  by <- runif(nItems-1, -3,3)
  b <- by - mean(by) # sum = 0
  b <- c(b,0) # Last Item = 0
  
  thetas <- rnorm(nExaminees) # Normal(0,1) 
  #    thetas <- (thetas - mean(thetas))/sd(thetas)
  
  return(list("a" = a,
              "b" = b,
              "thetas" = thetas))
}


Replica1 <- MakeParams(50, 1000)

nItems <- 50
nExaminees <- 1000



##############################
# Population we are dealing with

ggplot() +
  geom_histogram(mapping = aes(Replica1$thetas)) + 
  theme_bw() + xlab("Thetas") + ylab("Count") +
  ggtitle("Population")

#ggsave("Pops.png", g, dpi = 600)
#Replica1$b

############################## 
# Mirt

thetasMirt <- matrix(0,nrow = nExaminees,ncol = 100)
aMirt <- matrix(0,nrow = nItems,ncol = 100)
bMirt <- matrix(0,nrow = nItems,ncol = 100)

for (i in 1:100) {
  file1 <- paste("DataEvo/Data_", i, ".csv",sep = "")
  U <- read.csv(file1)
  modelo <- mirt(U,1,"2PL")
  
  thetasMirt[,i] <- fscores(modelo)
  
  aMirt[,i] <- coef(modelo, simplify=TRUE)$items[,1]
  bMirt[,i] <- coef(modelo, simplify=TRUE)$items[,2]
}

rm(modelo); rm(U)

############################## 
# Stan


thetasStan <- matrix(0,nrow = nExaminees,ncol = 100)
aStan <- matrix(0,nrow = nItems,ncol = 100)
bStan <- matrix(0,nrow = nItems,ncol = 100)

stan_file <- "2PL.stan"  

for (i in 1:100) {
  file1 <- paste("DataEvo/Data_", i, ".csv",sep = "")
  U <- read.csv(file1)
  
  data_list <- list(
    N = nrow(U),
    I = ncol(U),
    y = U
  )
  
  init_fun <- function() {
    list(
      theta = rnorm(data_list$N, 0, 1),  # Normal distribution for theta
      alpha = runif(data_list$I, 0.1, 2), # Uniform distribution for alpha, ensuring positive values
      beta = rnorm(data_list$I, 0, 1)     # Normal distribution for beta
    )
  }
  
  fit <- stan(
    file = stan_file,
    data = data_list,
    iter = 1000,
    chains = 2,
    init = init_fun
  )
  
  parameters <- summary(fit)
  
  thetasStan[,i] <- as.numeric(parameters$summary[1:1000, "mean"])
  
  aStan[,i] <- as.numeric(parameters$summary[1001:1050, "mean"])
  bStan[,i] <- as.numeric(parameters$summary[1051:1100, "mean"])
}


############################## 
# Networks


LerD <- function(fileA, fileB, fileTh, LI = 0){
  
  a <- read.csv(fileA, sep = ";")
  b <- read.csv(fileB, sep = ";")
  thetas <- read.csv(fileTh, sep = ";")
  
  a$X <- NULL
  b$X <- NULL
  thetas$X <- NULL
  
  if(LI == 1){
    a <- rbind(a,1)
    b <- rbind(b,0)
  }  
  return(list("a" = a,
              "b" = b,
              "thetas" = thetas))
}


fileA <- "OutputsEvo/aProduct.csv"
fileB <- "OutputsEvo/bProduct.csv"
fileTh <- "OutputsEvo/thetasProduct.csv"
Prod <- LerD(fileA,fileB,fileTh,0)



fileA <- "OutputsEvo/aLastItem.csv"
fileB <- "OutputsEvo/bLastItem.csv"
fileTh <- "OutputsEvo/thetasLastItem.csv"
Li <- LerD(fileA,fileB,fileTh,1)


fileA <- "OutputsEvo/aVAE.csv"
fileB <- "OutputsEvo/bVAE.csv"
fileTh <- "OutputsEvo/thetasVAE.csv"
Vae <- LerD(fileA,fileB,fileTh,0)


fileA <- "OutputsEvo/aIWAVE.csv"
fileB <- "OutputsEvo/bIWAVE.csv"
fileTh <- "OutputsEvo/thetasIWAVE.csv"
IWAVE <- LerD(fileA,fileB,fileTh,0)

rm(fileA); rm(fileB); rm(fileTh)
rm(LerD)

for (i in 1:ncol(IWAVE$a)) {
  if(IWAVE$a[1,i] < 0){
    IWAVE$a[,i] <- -IWAVE$a[,i]
    IWAVE$thetas[,i] <- -IWAVE$thetas[,i]
  }
}


###########################################################################

BoxAGGp <- function(matriz,aR,name){
  DF <- round(matriz - aR,2)
  DF <- as.data.frame(DF)
  colnames(DF) <- paste("X",1:100,sep = "")
  DF$aR <- round(aR,2)
  
  DF <- DF %>% 
    pivot_longer(
      cols = starts_with("X"), 
      names_to = "Run", 
      values_to = "Th", 
      names_prefix = "X"
    )
  
  custom_labels <- ifelse(round(aR,2) %in% c("0.6", "1", "1.37"),
                          as.character(round(aR,2)), "")
  
  return(ggplot(DF,aes(as.factor(round(aR,2)),Th)) +
           geom_boxplot() + geom_hline(yintercept = 0,
                                       lty = 2, col = 'red',
                                       alpha = 0.8) +
           lims(y = c(-1,1))+
           theme_bw() + xlab("Real Parameter") + ylab("Bias") +
           theme(axis.text.x = element_text(size = 4))   +
           scale_x_discrete(breaks = round(aR,2), labels = custom_labels) + 
           ggtitle(name))
}


g1 <- BoxAGGp(Li$a,Replica1$a,"a - Last Item")
g2 <- BoxAGGp(Prod$a,Replica1$a,"a - Product")
g3 <- BoxAGGp(aMirt,Replica1$a,"a - Mirt")
g4 <- BoxAGGp(aStan,Replica1$a,"a - Stan")
g5 <- BoxAGGp(IWAVE$a,Replica1$a,"a - IWAVE")
g6 <- BoxAGGp(Vae$a,Replica1$a,"a - VAE")



g <- gridExtra::grid.arrange(g1,g2,g3,g4,g5,g6)

#name <- paste("Out",File,".png", sep = "")
#ggsave("BiasA.png",g, dpi = 600, width = 8, height = 4)


#View(Li$a)
####################
BoxAGGp <- function(matriz,aR,name){
  DF <- round(matriz - aR,2)
  DF <- as.data.frame(DF)
  colnames(DF) <- paste("X",1:100,sep = "")
  DF$aR <- round(aR,2)
  
  DF <- DF %>% 
    pivot_longer(
      cols = starts_with("X"), 
      names_to = "Run", 
      values_to = "Th", 
      names_prefix = "X"
    )
  
  custom_labels <- ifelse(round(aR,2) %in% c("-2.67", "0.00", "2.78"),
                          as.character(round(aR,2)), "")
  
  return(ggplot(DF,aes(as.factor(round(aR,2)),Th)) +
           geom_boxplot() + geom_hline(yintercept = 0,
                                       lty = 2, col = 'red',
                                       alpha = 0.8) +
           lims(y = c(-1,1))+
           theme_bw() + xlab("Real Parameter") + ylab("Bias") +
           theme(axis.text.x = element_text(size = 4))   +
           scale_x_discrete(breaks = round(aR,2), labels = custom_labels) + 
           ggtitle(name))
}


g1 <- BoxAGGp(Li$b,Replica1$b,"b - Last Item")
g2 <- BoxAGGp(Prod$b,Replica1$b,"b - Product")
g3 <- BoxAGGp(bMirt,Replica1$b,"b - Mirt")
g4 <- BoxAGGp(-aStan*bStan,Replica1$b,"b - Stan")
g5 <- BoxAGGp(IWAVE$b,Replica1$b,"b - IWAVE")
g6 <- BoxAGGp(Vae$b,Replica1$b,"b - VAE")

g <- gridExtra::grid.arrange(g1,g2,g3,g4,g5,g6)

#ggsave("BiasB.png",g, dpi = 600, width = 8, height = 4)

########################################################
BoxThetasGGp <- function(matriz,thetasR,name){
  DF <- round(matriz - thetasR,1)
  DF <- as.data.frame(DF)
  colnames(DF) <- paste("X",1:100,sep = "")
  DF$thetasR <- round(thetasR,1)
  
  DF <- DF %>% 
    pivot_longer(
      cols = starts_with("X"), 
      names_to = "Run", 
      values_to = "Th", 
      names_prefix = "X"
    )
  
  return(ggplot(DF,aes(as.factor(thetasR),Th)) +
           geom_boxplot() + geom_hline(yintercept = 0,
                                       lty = 2, col = 'red',
                                       alpha = 0.8) +
           lims(y = c(-3,3))+
           theme_bw() + xlab("Real Parameter") + ylab("Bias") +
           theme(axis.text.x = element_text(size = 5,angle = 45))   +
           ggtitle(paste("Thetas -", name)))
}

g1 <- BoxThetasGGp(Prod$thetas,Replica1$thetas,"Product")
g2 <- BoxThetasGGp(Li$thetas,Replica1$thetas,"Last Item")
g3 <- BoxThetasGGp(thetasMirt,Replica1$thetas,"Mirt")
g4 <- BoxThetasGGp(thetasStan,Replica1$thetas,"Stan")
g5 <- BoxThetasGGp(IWAVE$thetas,Replica1$thetas,"IWAVE")
g6 <- BoxThetasGGp(Vae$thetas,Replica1$thetas,"VAE")

g <- gridExtra::grid.arrange(g1,g2,g3,g4,g5,g6)

#name <- paste("The",File,".png", sep = "")
#ggsave("BiasThetas.png",g, dpi = 600, width = 8, height = 4)



################################################################################################################

#### a


rmseProd <- c()
rmseLi <- c()
rmseMirt <- c()
rmseStan <- c()
rmseIWAVE <- c()
rmseVAE <- c()

for (i in 1:nItems) {
  rmseProd[i]         <- mean(abs(as.numeric(Prod$a[i,] - Replica1$a[i])))  
  rmseLi[i]           <- mean(abs(as.numeric(Li$a[i,] - Replica1$a[i])))  
  rmseVAE[i]          <- mean(abs(as.numeric(Vae$a[i,] - Replica1$a[i])))  
  rmseStan[i]          <- mean(abs(as.numeric(aStan[i,] - Replica1$a[i])))  
  rmseIWAVE[i]          <- mean(abs(as.numeric(IWAVE$a[i,] - Replica1$a[i])))  
  rmseMirt[i]   <- mean(abs(as.numeric(aMirt[i,] - Replica1$a[i])))  
}


mean(rmseMirt)
mean(rmseLi)
mean(rmseProd)
mean(rmseIWAVE)
mean(rmseStan)
mean(rmseVAE)


#### b

rmseProd <- c()
rmseLi <- c()
rmseMirt <- c()
rmseStan <- c()
rmseIWAVE <- c()
rmseVAE <- c()

for (i in 1:nItems) {
  rmseProd[i]         <- mean(abs(as.numeric(Prod$b[i,] - Replica1$b[i])))  
  rmseLi[i]           <- mean(abs(as.numeric(Li$b[i,] - Replica1$b[i])))  
  rmseVAE[i]          <- mean(abs(as.numeric(Vae$b[i,] - Replica1$b[i])))  
  rmseStan[i]          <- mean(abs(as.numeric(-aStan[i,]*bStan[i,] - Replica1$b[i])))  
  rmseIWAVE[i]          <- mean(abs(as.numeric(IWAVE$b[i,] - Replica1$b[i])))  
  rmseMirt[i]   <- mean(abs(as.numeric(bMirt[i,] - Replica1$b[i])))  
}


mean(rmseMirt)
mean(rmseLi)
mean(rmseProd)
mean(rmseIWAVE)
mean(rmseStan)
mean(rmseVAE)


#### thetas

rmseProd <- c()
rmseLi <- c()
rmseMirt <- c()
rmseStan <- c()
rmseIWAVE <- c()
rmseVAE <- c()

for (i in 1:nItems) {
  rmseProd[i]         <- mean(abs(as.numeric(Prod$thetas[i,] - Replica1$thetas[i])))  
  rmseLi[i]           <- mean(abs(as.numeric(Li$thetas[i,] - Replica1$thetas[i])))  
  rmseVAE[i]          <- mean(abs(as.numeric(Vae$thetas[i,] - Replica1$thetas[i])))  
  rmseStan[i]          <- mean(abs(as.numeric(thetasStan[i,] - Replica1$thetas[i])))  
  rmseIWAVE[i]          <- mean(abs(as.numeric(IWAVE$thetas[i,] - Replica1$thetas[i])))  
  rmseMirt[i]   <- mean(abs(as.numeric(thetasMirt[i,] - Replica1$thetas[i])))  
}


mean(rmseMirt)
mean(rmseLi)
mean(rmseProd)
mean(rmseIWAVE)
mean(rmseStan)
mean(rmseVAE)


library(readr)
LossLI <- read_delim("OutputsEvo/LossLastItem.csv", 
                     delim = ";", 
                     escape_double = FALSE,
                     trim_ws = TRUE,col_names = F,
                    )


LossProd <- read_delim("OutputsEvo/LossProduct.csv", 
                     delim = ";",
                     escape_double = FALSE,
                     trim_ws = TRUE, col_names = F,
                     )

LossLI <- LossLI[-1,]
LossProd <- LossProd[-1,]

g1 <- ggplot() + geom_boxplot(mapping = aes(y = LossLI$X2)) +
  theme_bw() + ylab("Loss") + ggtitle("Last Item")
g2 <- ggplot() + geom_boxplot(mapping = aes(y = LossProd$X2)) +
  theme_bw()+ ylab("Loss") + ggtitle("Product")


library(gridExtra)

g3 <- grid.arrange(g1,g2, nrow = 1)



ggsave("LossRepl.png", g3,
       dpi = 600)

