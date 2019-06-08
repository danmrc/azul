dist <- function(x,lambda,btrue,varu){
  sqrt(lambda/(2*pi*varu))*abs(1-btrue)/(1-x)^2*exp(-lambda/(2*varu)*((x-btrue)/(1-x))^2)
}

xx <- seq(-1,2,by = 0.01)
yy <- dist(xx,1,0,1)
yy2 <- dist(xx,1,0.6,1)

plot(xx,yy2, type = "l")
lines(xx,yy,col=2)
legend("topleft", legend = c(TeX('$\\beta = 0.6$'),TeX('$\\beta = 0$')),lty=1,col=c(1,2))

       