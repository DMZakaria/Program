################################################################################################# 
# Name of file           Utils.R                                                                #
# Creation Date          05/05/2014                                                             #
# last modification      29/05/2014                                                             #
# Version                2.0                                                                    # 
# @Description           this file contains some usefull functions to preprocess sensor files   #                                                   #
# @author                Djedou Zakaria                                                         #
#################################################################################################

################################################################
# @Description   add experimentation to the data base          # 
#                                                              #
# @param         sujet                                         #
# @param         expNum                                        #
# @param         activity                                      #
# @param         temperature                                   #
# @param         parasymp                                      #
# @param         sympa                                         #
################################################################

addExperimentation<- function (sujet,expNum,activity,temperature,parasymp,symp)
{
  
  #open a connexion
  channel <- odbcConnect(dsn="RSQL",uid="root",pwd="toor")
  s <- sujet
  numExp <- expNum
  label<-activity
  temp<-temperature
  para<-parasymp
  sympa<-symp
  requetesql <- paste("INSERT INTO `record_data`  (`Sujet`,`Nume_Exp`, `activity_label`,`Temperature`,`ParaSymp`,`Symp`) VALUES ('",s,"', '",numExp,"', '",label,"','",temp,"','",para,"','",sympa,"');")
  
  #send the query
  sqlQuery(channel,requetesql)
  paste (requetesql)
  
  #colse the connexion
  close(channel)
  
}

##############################################################
# @Description   compute the average of the activity level   # 
#                                                            #
# @param         data file solts (X,Y,Z)                     #
# @return        data frame                                  # 
##############################################################

activityMean <- function(x)
{
  x.x <- decouper(x["X"]["val"])
  x.y <- decouper(x["Y"]["val"])
  x.z <- decouper(x["Z"]["val"])
  x.xx <- spliter(x.x,100)
  x.yy <- spliter(x.y,100)
  x.zz <- spliter(x.z,100)
  Activity_Label <- unlist(lapply(x.xx,activite1))
  x.ay <- unlist(lapply(x.yy,activite1))
  x.az <- unlist(lapply(x.zz,activite1))
  return(
    as.data.frame(Activity_Label) +
      as.data.frame(x.ay) +
      as.data.frame(x.az)
  ) * 4 /3
  
}

########################################################
# @Description   compute the average of the aut file   # 
#                                                      #
# @param         aut Slots (Sympa,ParaSymp)            #
# @return        data frame                            # 
########################################################

autMean<- function(aut)
{
  e<-data.frame(aut)
  
  #grouping variable for every 10 lines                             
  grp<-(seq.int(nrow(e))-1) %/% 10 + 1
  
  #use aggregate to calculate mean for groups
  aut2<-aggregate(.~grp,e, mean)
  aut2<-aut2[,-1]
  return (as.data.frame(aut2))
  
}

###############################################################
# @Description   assign a value to activity                   # 
#                                                             #
# @param         Activity data frame                          #
###############################################################

LabelActivity <- function(x)
{
  
  ifelse(x<5,'null',ifelse(x>=5 & x<10,'faible',ifelse(x>=10 & x<30,'moyenne','importante')))
  
}

########################################################
# @Description   compute the average of the aut file   # 
#                                                      #
# @param         aut Slots (Sympa,ParaSymp)            #
# @return        data frame                            #
########################################################

TemperatureMean <- function (temp)
{
  
  e<-data.frame(temp)
  
  #grouping variable for every 100 lines                             
  grp<-(seq.int(nrow(e))-1) %/% 1000 + 1
  
  #use aggregate to calculate mean for groups
  temperature<-aggregate(.~grp,e, mean)
  temperature<-temperature[,-1]
  return (as.data.frame(temperature))
}

decouper <- function(x)
{
  l <- list()
  i <- 1
  taille <- length(x)
  delta <- 100 # (changer) 1 bloc pour 0.5 secondes
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


activite1 <- function(l)
{
  s <- 0
  for(i in 1:length(l))
  {
    s <- s + abs(l[[i]]["a"])
  }
  return(s)
}

