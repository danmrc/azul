####### EM TRABALHO ############

library(dplyr)
library(ggplot2)
library(gridExtra)

set.seed(1234)

n = 4

bases = list()

for(i in 1:n) {
  
  bases[[i]] = rnorm(10 * 10 ** i)
}

names(bases) = letters[1:n]

for(i in letter[1:n]) {
  
  
  
}





for(i in 1:4) {
  

  ggplot(aes(x = paste("am", i))) +
    geom_histogram(aes(y = ..density..), 
                   binwidth = .1, 
                   fill = "#0273be") +
    
        stat_function(fun = dnorm, 
                      args = list(mean = 0, sd = 1), 
                      size = 2, 
                      color = "dark blue")
    
      xlab("") +
    ylab("Densidade") +
    
  
    )
}

gg1 = am %>%
  ggplot(aes(x = am1))+
  geom_histogram(aes(y=..density..), binwidth = .1, fill = "#0273be")+
  xlab("")+
  ylab("Densidade")+
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), size = 2, color = "dark blue")

gg2 = am %>%
  ggplot(aes(x = am2))+
  geom_histogram(aes(y=..density..), binwidth = .1, fill = "#0273be")+
  xlab("")+
  ylab("Densidade")+
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), size = 2, color = "dark blue")

gg3 = am %>%
  ggplot(aes(x = am3))+
  geom_histogram(aes(y=..density..), binwidth = .1, fill = "#0273be")+
  xlab("")+
  ylab("Densidade")+
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), size = 2, color = "dark blue")

gg4 = am %>%
  ggplot(aes(x = am4))+
  geom_histogram(aes(y=..density..), binwidth = .1, fill = "#0273be")+
  xlab("")+
  ylab("Densidade")+
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), size = 2, color = "dark blue")


grid.arrange(gg1, gg2, gg3, gg4)
