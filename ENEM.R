library(dplyr)
library(stringr)
library(ggplot2)
library(rstan)



ENEM <- read.csv("ENEM.csv")
ENEM$X <- NULL

set.seed(1510)
index <- sample(1:nrow(ENEM), 5000)

ENEM <- ENEM[index,]

comp <- function(Vetor1, Vetor2){
  return(str_split_1(Vetor1, "") == str_split_1(Vetor2, ""))
}



K1 <- apply(as.matrix(ENEM$TX_RESPOSTAS_CH), 1, comp,
          Vetor2 = ENEM$TX_GABARITO_CH[1])

K2 <- apply(as.matrix(ENEM$TX_RESPOSTAS_MT), 1, comp,
           Vetor2 = ENEM$TX_GABARITO_MT[1])

K3 <- apply(as.matrix(ENEM$TX_RESPOSTAS_CN), 1, comp,
           Vetor2 = ENEM$TX_GABARITO_CN[1])

K <- rbind(K1,K2,K3)

K <- as.matrix(K)

K <- K+0

K <- t(K)

K <- as.data.frame(K)
K$V67 <- NULL



write.csv(K, "Respostas.csv", row.names = F)


Qmat <- diag(1,3)
vec <- rep(1,45)
Qmat <- Qmat %x% vec
Qmat <- Qmat[-67,]
write.csv(Qmat, "Qmat.csv", row.names = F)

########################################################

library(gdata)

keep(Qmat, ENEM, sure = T)
U <- read.csv("Respostas.csv")


## Mirt model - with Q-matrix
library(mirt)

U <- as.data.frame(U)

ModelSpec <- mirt.model(Qmat)
model <- mirt(U,ModelSpec,'2PL')

aMirt <- coef(model, simplify = T)$items

#######
# remove all items with almost zero discrimination 
# or negative discrmination
index <- which(aMirt[1:45,1] < 0.2)
index <- c(index,which(aMirt[46:89,2] < 0.2))
index <- c(index,which(aMirt[90:134,3] < 0.2))

index <- as.numeric(str_remove(names(index), pattern = "V"))
index <- ifelse(index > 67, index-1, index)

U <- U[,-index]

write.csv(U, "Respostas.csv", row.names = F)

## Rearrange Qmat
Qmat <- diag(1,3)
vec <- rep(1,45)
Qmat <- Qmat %x% vec
Qmat <- Qmat[-67,]
Qmat <- Qmat[-index,]
write.csv(Qmat, "Qmat.csv", row.names = F)




############
# Mirt
ModelSpec <- mirt.model(Qmat)
model <- mirt(U,ModelSpec,'2PL')

aMirt <- coef(model, simplify = T)$items[,1:3]
bMirt <- coef(model, simplify = T)$items[,4]

############
# Stan

data_list <- list(
  N = nrow(U),
  I = ncol(U),
  y = U,
  D = 3,  
  Q = Qmat
)

init_function <- function() {
  list(
    theta = matrix(rnorm(nrow(U) * 3), nrow(U), 3),
    alpha = matrix(abs(rnorm(ncol(U) * 3)), ncol(U), 3),
    beta = rnorm(ncol(U))
  )
}

fit <- stan(
  file = "2PL.stan",
  data = data_list,
  iter = 1000,
  chains = 2,
  init = init_function,
  cores = 2,
)

############
# Network

aProd <- read.csv("EnemDiscr.csv")
aProd$X <- NULL
bProd <- read.csv("EnemDiff.csv")
bProd$X <- NULL


sum(aProd[,1] != 0)
sum(aProd[,2] != 0)

cor(aProd[1:44,1],aMirt[1:44,1])
cor(aProd[c(45:51,53:80),2],aMirt[c(45:51,53:80),2])
cor(aProd[c(81,82,85:112),3],aMirt[c(81,82,85:112),3])


cor(bProd$X0[-c(52,83,84)],bMirt[-c(52,83,84)])



g1 <- ggplot() + geom_point(mapping = aes(aProd[1:44,1],
                                          aMirt[1:44,1]))+
  theme_bw() + xlab("Autoencoder Estimation") + ylab("Mirt Estimation") +
  ggtitle("Discrimination parameter \n1st Dimension")

g2 <- ggplot() + geom_point(mapping = aes(aProd[c(45:51,53:80),2],
                                          aMirt[c(45:51,53:80),2]))+
  theme_bw() + xlab("Autoencoder Estimation") + ylab("Mirt Estimation") +
  ggtitle("Discrimination parameter \n2nd Dimension")

g3 <- ggplot() + geom_point(mapping = aes(aProd[c(81,82,85:112),3],
                                          aMirt[c(81,82,85:112),3])) +
  theme_bw() + xlab("Autoencoder Estimation") + ylab("Mirt Estimation") +
  ggtitle("Discrimination parameter \n3rd Dimension")



g4 <- ggplot() + geom_point(mapping = aes(bProd$X0[-c(52,83,84)],bMirt[-c(52,83,84)])) +
  theme_bw() + xlab("Autoencoder Estimation") + ylab("Mirt Estimation") +
  ggtitle("Difficulty parameter")


g <- gridExtra::grid.arrange(g1,g2,g3,g4)


ggsave("Enem.png",g, dpi = 600, width = 8, height = 4)



