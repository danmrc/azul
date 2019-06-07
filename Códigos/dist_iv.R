dist <- function(x,lambda,btrue,varu){
  sqrt(lambda/(2*pi*varu))*abs(1-btrue)/(1-x)^2*exp(-lambda/(2*varu)*((x-btrue)/(1-x))^2)
}

xx <- seq(0,2,by = 0.01)
yy <- dist(xx,0.1,0.6,1)
plot(xx,yy)
plot(xx,yy, type = "l")