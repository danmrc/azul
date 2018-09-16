rm(list=ls())

library(BETS)
library(dynlm)
library(vars)
library(expm)

ipca <- BETSget(13522, from="2003-01-01", to = "2017-12-31")
ibc_br <- BETSget(24364, to = "2017-12-31")
selic <- BETSget(4189,from="2003-01-01", to = "2017-12-31")

def <- par()

par(mfrow = c(3,1))

plot(ibc_br, main = "IBC-Br")
grid(col = "grey")
plot(ipca, main = "IPCA")
grid(col = "grey")
plot(selic, main = "Selic")
grid(col = "grey")

dibc <- diff(log(ibc_br))
ipca <- window(ipca, start = c(2003,02))
selic <- window(selic, start = c(2003,02))

par(def)

plot(dibc)

X <- cbind(selic,ipca,dibc)

VARselect(X)

#VAR Usando Choleski
#Estimaremos as equações 1 por 1 para deixar o procedimento transparente
#Veja que Chosleki == afirmar que não existe simultaneade
#Vamos colocar dois lags pois AIC e HQC dão lags demais no VAR select
#Vamos considerar que o IBC só depende de valores defasados
#IPCA depende contemporaneamente do ibc
#Selic depende contemporaneamente do ibc e do ipca

eq1 <- dynlm(dibc ~ L(dibc,1:2) + L(ipca,1:2) + L(selic,1:2))
eq2 <- dynlm(ipca ~ dibc + L(dibc,1:2) + L(ipca,1:2) + L(selic,1:2))
eq3 <- dynlm(selic ~ dibc + ipca + L(dibc,1:2) + L(ipca,1:2) + L(selic,1:2))

eq2aux <- dynlm(ipca ~ L(dibc,1:2) + L(ipca,1:2) + L(selic,1:2))
eq3aux <- dynlm(selic ~ L(dibc,1:2) + L(ipca,1:2) + L(selic,1:2))


#Vamos construir as matrizes B e C

B <- diag(1,ncol = 6, nrow = 6)
B[2,1] <- -coef(eq2)[2]
B[3,1:2] <- -coef(eq3)[2:3]

A <- matrix(0, ncol = 6, nrow = 6)
A[4,1] <- 1
A[5,2] <- 1
A[6,3] <- 1
A[1,c(1,4,2,5,3,6)] <- coef(eq1)[2:7]
A[2,c(1,4,2,5,3,6)] <- coef(eq2)[3:8]
A[3,c(1,4,2,5,3,6)] <- coef(eq3)[4:9]

C <- solve(B)%*%A
max(abs(eigen(C)$values))

#função para obter a irf

irf_em_t <- function(C,B,h){
  require(expm)
  return((C%^%h)%*%B)
}

fri <- function(C,B,t_max){
  FRI <- array(0,dim=c(nrow(C),ncol(C),(t_max+1)))
  for(j in 1:(t_max+1)){
    FRI[,,j] <- irf_em_t(C,B,(j-1))
  }
  return(FRI)
}

resposta <- fri(C,solve(B),10)

plot(0:10,resposta[1,1,1:11], type = "l")
lines(0:10,rep(0,11),col=2)
plot(0:10,resposta[1,2,1:11], type = "l")
lines(0:10,rep(0,11),col=2)
plot(0:10,resposta[1,3,1:11], type = "l")
lines(0:10,rep(0,11),col=2)
plot(0:10,resposta[2,3,1:11], type = "l")
lines(0:10,rep(0,11),col=2)

var1 <- VAR(X,p=2)
plot(vars::irf(var1))

#Agora vamos tentar usar Blanchard-Quah

C_aux <- C[1:3,1:3]
AUX <- diag(1,ncol = ncol(C_aux),nrow = nrow(C_aux))- C_aux
AUX_inv <- solve(AUX)
S <- var(resid(var1))
P <- AUX_inv%*%S%*%t(AUX_inv)
aux <- chol(P)
A <- AUX%*%P
A_aux <- matrix(0,ncol=6,nrow=6)
A_aux[1:3,1:3] <- A


respostabq <- fri(C,A_aux,10)
plot(0:10,respostabq[2,3,1:11], type = "l")
lines(0:10,rep(0,11),col=2)
