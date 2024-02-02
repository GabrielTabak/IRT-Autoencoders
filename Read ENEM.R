
if(!require(data.table)){install.packages('data.table')}


ENEM <- data.table::fread(input='microdados_ENEM_2022.csv',
                               integer64='character',
                               skip=0,  
                               nrow=-1, 
                               na.strings = "", 
                               showProgress = TRUE)



library(dplyr)

table(ENEM$CO_PROVA_CH)
table(ENEM$CO_PROVA_MT)
table(ENEM$CO_PROVA_CN)

ENEM_2022 <- ENEM %>% filter(CO_PROVA_CH == 1056,
                                  CO_PROVA_MT == 1078,
                                  CO_PROVA_CN == 1087)

table(ENEM_2022$CO_PROVA_CH)
table(ENEM_2022$CO_PROVA_MT)
table(ENEM_2022$CO_PROVA_CN)



set.seed(10)
ENEM_2022 <- ENEM_2022 %>% select(NU_NOTA_CH,
                                  NU_NOTA_MT,
                                  NU_NOTA_CN,
                                  TX_RESPOSTAS_CH,
                                  TX_GABARITO_CH,
                                  TX_RESPOSTAS_MT,
                                  TX_GABARITO_MT,
                                  TX_RESPOSTAS_CN,
                                  TX_GABARITO_CN)

X <- sample(1:nrow(ENEM_2022), 50000)

ENEM_2022 <- ENEM_2022[X,]

write.csv(ENEM_2022, "ENEM.csv")
