library(BETS)
library(forecast)
library(seasonal)

rm(list=ls())

busca <- BETSsearch("capacidade",lang ="pt")

cap <- BETSget(1344, from = "2000-01-01")

x13_des <- seas(cap, x11 = "", transform.function = "none", outlier = NULL, regression.aictest = NULL)

x13_des <- final(x13_des)

summary(x13_des)

des_dum <- tslm(cap ~ season)

plot(fitted(des_dum))

des_dum_final <- residuals(des_dum) + mean(cap)

lsup <- max(des_dum_final,x13_des)
linf <- min(des_dum_final,x13_des)

plot(des_dum_final, ylim = c(linf,lsup))
lines(x13_des,col = 2)

plot((x13_des - des_dum_final))


################################3
# Energia?
#########################

busca <- BETSsearch("energia",lang ="pt")

energ <- BETSget(1406, from = "2002-01-01")

plot(energ)

detrend_energ <- tslm(energ ~ trend)

detrend_energ <- residuals(detrend_energ)

plot(detrend_energ)

x13_des <- seas(detrend_energ, x11 = "", transform.function = "none", outlier = NULL, regression.aictest = NULL)

x13_des_final <- final(x13_des)

dum_des <- tslm(detrend_energ ~ season)

dum_des_final <- residuals(dum_des) + mean(detrend_energ)

plot(dum_des_final)
lines(x13_des, col = 2)

mse <- sum((dum_des_final - x13_des_final)^2)/length(dum_des_final)

