simple_rule1 <- function(periodo,cutoff,media0,media1){
  
  if (periodo < cutoff){
    if(periodo %% 2 == 0){
      return(c(0,TRUE))
    } else{
      return(c(1,TRUE))
    }
    
  } else {
    if(media1 > media0){
      return(c(1,FALSE))
    } else{
      return(c(0,FALSE))
    }
  }
}

dgp <- function(prob0,prob1,n0,n1,media0,media1,decision,update){
  
  if(decision == 0){
    
    suc <- rbinom(1,1,prob0)
    
    if(update){
    
      media0 <- (n0*media0 + suc)/(n0+1)
      n0 <- n0 + 1
    }
    
    return(c(suc,n0,n1,media0,media1))
    
  } else if(decision == 1){
    
    suc <- rbinom(1,1,prob1)
    
    if(update){
      
      media1 <- (n1*media1 + suc)/(n1+1)
      n1 <- n1 + 1
    }
    
    return(c(suc,n0,n1,media0,media1))
    
  }
  
  
  
}

simulacao <- function(N,prob0,prob1,rule,...){
  
  media0 <- 0
  media1 <- 0
  n0 <- 0
  n1 <- 0
  payoff <- 0
  
  for(i in 1:N){
    
    decision <- rule(periodo = i, media0 = media0, media1 = media1,...)
    
    if(decision[1] == 0){
      
      result <- dgp(prob0,prob1,n0,n1,media0,media1,decision[1],decision[2])
      n0 <- result[2]
      media0 <- result[4]
      payoff <- result[1] + payoff
      
    } else if(decision[1] == 1){
      
      result <- dgp(prob0,prob1,n0,n1,media0,media1,decision[1],decision[2])
      n1 <- result[3]
      media1 <- result[5]
      payoff <- result[1] + payoff
      
    } else{
      stop("Decisão deve ser 0 ou 1")
    }
    
  }
  return(c(payoff,decision[1]))
}

simple_rule2 <- function(periodo,t_1,t_2,threshold,media0,media1){
  
  if(periodo < t_1){
    
    if(periodo %% 2 == 0){
      return(c(0,TRUE,FALSE))
    } else{
      return(c(1,TRUE,FALSE))
    }
    
  } else if (periodo < t_2){
    
    if(abs(media0 - media1) < threshold){
      
      if(periodo %% 2 == 0){
        return(c(0,TRUE,FALSE))
      } else{
        return(c(1,TRUE,FALSE))
      }
      
    } else{
      
      if(media0 > media1){
        return(c(0,FALSE,TRUE))
      } else{
        return(c(1,FALSE,TRUE))
      }
      
    }
    
    
  } else{
    
    if(media0 > media1){
      return(c(0,FALSE,FALSE))
    } else{
      return(c(1,FALSE,FALSE))
    }
  }
  
}

simulacao2 <- function(N,prob0,prob1,t_1,t_2,threshold){
  
  media0 <- 0
  media1 <- 0
  n0 <- 0
  n1 <- 0
  payoff <- 0
  early_stop <- FALSE
  
  for(i in 1:N){
    
    decision <- simple_rule2(i,t_1,t_2,threshold,media0,media1)
    
    if(decision[1] == 0){
      
      result <- dgp(prob0,prob1,n0,n1,media0,media1,decision[1],decision[2])
      n0 <- result[2]
      media0 <- result[4]
      payoff <- result[1] + payoff
      
      early_stop <- ifelse(early_stop,TRUE,ifelse(decision[3],TRUE,FALSE))
      
    } else if(decision[1] == 1){
      
      result <- dgp(prob0,prob1,n0,n1,media0,media1,decision[1],decision[2])
      n1 <- result[3]
      media1 <- result[5]
      payoff <- result[1] + payoff
      
      early_stop <- ifelse(early_stop,TRUE,ifelse(decision[3],TRUE,FALSE))
      
    } else{
      stop("Decisão deve ser 0 ou 1")
    }
    
  }
  return(c(payoff,decision[1],early_stop))
}

dgp <- function(prob0,prob1,n0,n1,media0,media1,decision,update){
  
  if(decision == 0){
    
    suc <- rbinom(1,1,prob0)
    
    if(update){
      
      media0 <- (n0*media0 + suc)/(n0+1)
      n0 <- n0 + 1
    }
    
    return(c(suc,n0,n1,media0,media1))
    
  } else if(decision == 1){
    
    suc <- rbinom(1,1,prob1)
    
    if(update){
      
      media1 <- (n1*media1 + suc)/(n1+1)
      n1 <- n1 + 1
    }
    
    return(c(suc,n0,n1,media0,media1))
    
  }
  
}

library(purrr)
library(abind)

input <- replicate(1000,30,simplify = FALSE)

output <- map(input,simulacao2, prob0 = 0.5,prob1 = 0.7,t_1 = 10, t_2 = 15, threshold = 0.5)

output <- do.call(rbind,output)

colnames(output) <- c("Payoff","Proporção decisão 1", "Proporção Early Stop")

colMeans(output)

simulacao_threshold <- function(N,prob0,prob1,t_1,t_2,thresholds){
  
  thresholds_list <- as.list(thresholds)
  results <- map(thresholds_list,simulacao2,N = N,prob0 = prob0,prob1 = prob1,t_1 = t_1,t_2 = t_2)
  results <- do.call(rbind,results)
  rownames(results) <- thresholds
  colnames(results) <- c("Payoff","Proporção Tratamento 1","Proporção Early Stop")
  
  return(results)
  
}

input <- replicate(2000,c(0.2,0.3,0.4,0.5,0.7),simplify = FALSE)
output <- map(input,simulacao_threshold,N = 30,prob0 = 0.5,prob1 = 0.7, t_1 = 10,t_2 = 15)

output <- do.call(abind,list(output,along = 3))
result <- apply(output,c(1,2),mean)
result
