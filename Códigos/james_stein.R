library(purrr)
library(tidyr)
library(ggplot2)
library(tibble)

simul_one_estimators <- function(true_par,n,reg,k){
  
  x <- matrix(rnorm(n*k),ncol = k)
  true_par <- rep(true_par,k)
  y <- x%*%true_par + rnorm(n)
  
  res <- lm(y ~ x)
  
  ols <- coef(res)
  shrink_js <- n*as.vector(t(ols[-1])%*%cov(x)%*%ols[-1])
  stein <- ols[-1] - reg/shrink_js*ols[-1]
  stein <- c(ols[1],stein)
  
  return(rbind(ols,stein))
  
}


simul_estimators <- function(true_par,n,reg,p,k){
  
  input <- replicate(p,true_par,simplify = F)
  
  result <- map(input,simul_one_estimators, n = n, reg = reg,k=k)
  
  mat_true_cof <- rbind(c(0,rep(true_par,k)),c(0,rep(true_par,k)))
  
  loss <- map(result,function(x){(x - mat_true_cof)^2 %>% rowSums()}) 
  
  loss <- do.call(rbind,loss) %>% colMeans()
  
  return(loss)
  
}

simul_various_pars <- function(true_pars,n,reg,p,k){
  
  input <- as.list(true_pars)
  
  result <- map(input,simul_estimators,n=n,reg=reg,p=p,k=k)
  
  results <- do.call(rbind,result)
  
  return(results)
  
}



simul_estimators(1,100,1,1000,3)

tt <- simul_various_pars(seq(0,4,by=0.1),100,4,2000,5)

plot(seq(0,4,by=0.1),tt[,1]*100,type="l",ylim = c(6,7))
lines(seq(0,4,by=0.1),tt[,2]*100,col=2)
abline(h=4)

reg_pars <- as.list(seq(1,3,by=0.1))

td <- as_tibble(tt)

td <- bind_cols(grid = seq(0,4,by=0.1),td) 

td_tidy <- pivot_longer(td,cols = c(ols,stein))

ggplot(td_tidy, aes(x = grid, y = value,col = name)) + geom_line() + theme_light() + labs(x = "Coeficiente", y = "Risco", color = "Estimador")

plot(diff_regs[[1]][,1], type="l")
lines(diff_regs[[11]][,2],col=2)
lines(diff_regs[[1]][,2],col=3)
lines(diff_regs[[6]][,2],col=4)
lines(diff_regs[[15]][,2],col=2,lty=2)
lines(diff_regs[[21]][,2],col=3,lty=2)

###############################################################################################

simul_various_regs <- function(true_par,regs,n,k){
  
  x <- matrix(rnorm(n*k),ncol = k)
  y <- x%*%rep(true_par,k) + rnorm(n)
  
  res <- lm(y ~ x)
  
  ols <- coef(res)
  shrink_js <- n*as.vector(t(ols[-1])%*%cov(x)%*%ols[-1])
  ols_mat <- t(replicate(length(regs),ols[-1]))
  
  stein <- ols_mat - as.matrix(regs)%*%ols[-1]/shrink_js
  stein <- cbind(ols[1],stein)
  
  rownames(stein) <- regs
  
  return(rbind(ols,stein))
  
}

monte_carlo_loss <- function(true_par,regs,n,k,p){
  
  input <- replicate(p,true_par,simplify = F)
  output <- map(input,simul_various_regs,regs = regs,n = n,k = k)
  
  cofs <- c(0,rep(true_par,k))
  
  true_par_mat <- t(replicate(length(regs)+1,c(cofs)))
  
  loss <- map(output,function(x){(x-true_par_mat)^2 %>% rowSums()})
  
  loss <- do.call(rbind,loss) %>% colMeans()
  
  return(loss)
  
}

monte_carlo_pars <- function(true_pars,regs,n,k,p){
  
  input <- as.list(true_pars)
  output <- map(input,monte_carlo_loss,regs = regs,n=n,k=k,p=p)
  
  result <- do.call(rbind,output)
  rownames(result) <- true_pars
  
  
  return(result)
  
}

simul_various_regs(1,seq(1,5,by=0.5),100,5)

monte_carlo_loss(1,seq(2,6,by=0.5),100,5,3000)

final <- monte_carlo_pars(seq(0,1,by=0.1),3,100,5,1000)

xx <- seq(0,1,by = 0.1)

plot(xx,final[,1],type = "l",ylim = c(0.02,0.08))
lines(xx,final[,2],col=2)
lines(xx,final[,3],col=3)
lines(xx,final[,4],col=4)
lines(xx,final[,5],col=6)#,lty=2)
lines(xx,final[,6],col=3,lty=2)
lines(xx,final[,7],col=4,lty=2)
lines(xx,final[,8],col=5,lty=2)

final_tibble <- as_tibble(final) %>% bind_cols(par = seq(0,1,by=0.1)) %>% pivot_longer(!par)

ggplot(final_tibble,aes(x=par,y=value,color = name)) + geom_line() + scale_color_brewer(palette = "Dark2") + theme_light()
