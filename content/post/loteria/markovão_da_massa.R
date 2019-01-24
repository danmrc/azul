library(readr)
library(dplyr)
library(markovchain)

loteria = read_csv("Cava.csv", skip = 3)
loteria$X1 = NULL
loteria$numero = loteria$Nº
loteria$Nº = NULL

loteria$evento1 = factor(loteria$`Evento 1`)
loteria$`Evento 1` = NULL

loteria$evento2 = factor(loteria$`Evento 2`)
loteria$`Evento 2` = NULL

loteria$evento3 = factor(loteria$`Evento 3`)
loteria$`Evento 3` = NULL


trans.matrix <- function(X, prob=T)
 {
  tt <- table( c(X[,-ncol(X)]), c(X[,-1]) )
  if(prob) tt <- tt / rowSums(tt)
  tt
 }

markovcaralho = loteria[2:3] %>% 
  as.matrix() %>%
  trans.matrix() %>%
  round(digits = 2)


mc1 = loteria[2] %>% markovchainFit()
mc2 = loteria[3] %>% markovchainFit()
mc3 = loteria[4] %>% markovchainFit()
