


for(i in 1:4){
  
  assign(paste("am",i, sep = ""),
         rnorm(10 * 10 ** i))

str(paste("am", i, sep=""))
}
am1 = rnorm(10)
am2 = rnorm(100)
am3 = rnorm(1000)
am4 = rnorm(10000)


am = data.frame(am1, am2, am3, am4)

library(dplyr)
library(ggplot2)
library(gridExtra)


for(i in 1:4) {
  
  assign(paste("gg", i, sep = ""),  
         am %>%
    ggplot(aes(x = paste("am", i)))+
    geom_histogram(aes(y=..density..), binwidth = .1, fill = "#0273be")+
    xlab("")+
    ylab("Densidade")+
    stat_function(fun = dnorm, args = list(mean = 0, sd = 1), size = 2, color = "dark blue")
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


grid.arrange(gg1,gg2,gg3,gg4)
