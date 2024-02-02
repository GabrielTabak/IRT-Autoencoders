library(dplyr)
library(tidyr)
library(ggplot2)


# What parameter? a, b
# What method? LI, Product, VAE
name <- "b - Product"

Df <- read.csv("Outs/bIC.csv")
Df$X <- NULL
Df$Th <- c(1:50)


BoxChart <- function(DF, name){
  DF <- DF %>% 
    pivot_longer(
      cols = starts_with("column"), 
      names_to = "Run", 
      values_to = "Values", 
      names_prefix = "X"
    )
  
  DF$Th <- as.factor(DF$Th)
  
  custom_labels <- ifelse(DF$Th %in% c("1", "10", "25", "40", "49"),
                          as.character(DF$Th), "")

print(ggplot(DF,aes(Th,Values)) +
    geom_boxplot() + 
    theme_bw() + xlab("Item") + ylab("Estimated Value") +
#    theme(axis.text.x = element_text(size = 5))   +
    scale_x_discrete(breaks = DF$Th, labels = custom_labels) + 
    ggtitle(name))
}

BoxChart(Df, name)

ggsave("bLI.png", dpi = 600, width = 8, height = 4)


Df <- t(Df)
Df <- as.data.frame(Df)

ggplot(mapping = aes(1:50,sapply(Df[-31,], var))) + 
  geom_point() + 
  theme_bw() + xlab("Item") + ylab("Variance") +
  ggtitle("Variances of each Item - b - Product")

ggsave("bLIVar.png", dpi = 600, width = 8, height = 4)





#### Name

Df <- read.csv("Outs/thetasVAEIC.csv")
Df$X <- NULL


Df <- round(Df,2)
#Df <- Df[order(Df$column_0),]

DF <- Df
DF$Th <- rowMeans(DF)
DF <- DF[order(DF$Th),]
DF <- round(DF,2)

DF <- DF %>% 
  pivot_longer(
    cols = starts_with("column"), 
    names_to = "Run", 
    values_to = "Values", 
    names_prefix = "X"
  )

DF$Th <- as.factor(DF$Th)


custom_labels <- ifelse(DF$Th %in% c("-2.55", "-0.88", "0.29", "0.86", "2.81"),
                        as.character(DF$Th), "")

print(ggplot(DF,aes(Th,Values)) +
        geom_boxplot() + 
        theme_bw() + xlab("Mean Estimated Trait") + ylab("Estimated Value") +
        #    theme(axis.text.x = element_text(size = 5))   +
        scale_x_discrete(breaks = DF$Th, labels = custom_labels) + 
        ggtitle("thetas - VAE"))

ggsave("thetasVAE.png", dpi = 600, width = 8, height = 4)


Df <- t(Df)
Df <- as.data.frame(Df)

ggplot(mapping = aes(1:1000,sapply(Df, var))) + 
  geom_point() + 
  theme_bw() + xlab("Individual") + ylab("Variance") +
  ggtitle("Variances of each Trait - theta - VAE")

ggsave("thetasVarVAE.png", dpi = 600, width = 8, height = 4)



