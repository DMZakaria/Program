################################################################################################# 
# Name of file           Utils.R                                                                #
# Creation Date          05/05/2014                                                             #
# last modification      29/05/2014                                                             #
# Version                2.0                                                                    # 
# @Description           this file contains some usefull functions to preprocess sensor files   #                                                   #
# @author                Djedou Zakaria                                                         #
#################################################################################################


synchro <- function(aut_size,data_size)
{

 y<-(data_size*2000)/aut_size
 return (y)
}

decouper <- function(x)
{
  l <- list()
  i <- 1
  taille <- length(x)
  delta <- 10
  j <- i + delta
  z <- 1:(delta + 1)
  indice <- 1
  
  while(j < taille)
  {
    tmp <- x[i:j]
    m <- mean(tmp)
    rl <- lm(tmp~z)
    a <- coef(rl)[2]
    b <- coef(rl)[1]
    bloc <- new(Class="Bloc", a=a, b=b, centre=m)
    l[[indice]] <- bloc
    i <- j
    j <- i + delta
    indice <- indice + 1
  }
  
  return(l)
  
}


spliter <- function(x, delta)
{
  l <- list()
  i <- 1
  taille <- length(x)
  
  j <- i + delta
  z <- 1:(delta + 1)
  indice <- 1
  
  while(j < taille)
  {
    tmp <- x[i:j]
    l[[indice]] <- tmp
    i <- j
    j <- i + delta
    indice <- indice + 1
  }
  
  return(l)
  
}



