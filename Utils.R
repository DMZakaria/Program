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

addExperimentation<- function (sujet,expNum,time,activity,temperature,parasymp,symp)
{
  
  #open a connexion
  channel <- odbcConnect(dsn="RSQL",uid="root",pwd="toor")
  s <- sujet
  numExp <- expNum
  t<-time
  label<-activity
  temp<-temperature
  para<-parasymp
  sympa<-symp
  requetesql <- paste("INSERT INTO `record_data`  (`Sujet`,`Num_Night`, `Time`,`Body_activity`,`Temperature`,`ParaSymp`,`Symp`) VALUES ('",s,"', '",numExp,"','",t,"', '",label,"','",temp,"','",para,"','",sympa,"');")
  
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

activityMean <- function(x,synchroVar)
{
  x.x <- decouper(x["X"]["val"])
  x.y <- decouper(x["Y"]["val"])
  x.z <- decouper(x["Z"]["val"])
  x.xx <- spliter(x.x,synchroVar/10)
  x.yy <- spliter(x.y,synchroVar/10)
  x.zz <- spliter(x.z,synchroVar/10)
  x.ax <- unlist(lapply(x.xx,activite1))
  x.ay <- unlist(lapply(x.yy,activite1))
  x.az <- unlist(lapply(x.zz,activite1))
  return(
    as.data.frame(x.ax) +
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
  
  ifelse(x<5,'null',ifelse(x>=5 & x<10,'weak',ifelse(x>=10 & x<30,'medium','high')))
  
}

########################################################
# @Description   compute the average of the aut file   # 
#                                                      #
# @param         aut Slots (Sympa,ParaSymp)            #
# @return        data frame                            #
########################################################

TemperatureMean <- function (temp,synchroVar)
{
  
  e<-data.frame(temp)
  
  #grouping variable for every synchroVar lines                             
  grp<-(seq.int(nrow(e))-1) %/% synchroVar + 1
  
  #use aggregate to calculate mean for groups
  temperature<-aggregate(.~grp,e, mean)
  temperature<-temperature[,-1]
  return (as.data.frame(temperature))
}

TemperatureMean1 <- function (temp)
{
  
  e<-data.frame(temp)
  
  #grouping variable for every synchroVar lines                             
  grp<-(seq.int(nrow(e))-1) %/% 2000 + 1
  
  #use aggregate to calculate mean for groups
  temperature<-aggregate(.~grp,e, mean)
  temperature<-temperature[,-1]
  return (as.data.frame(temperature))
}


synchro <- function(aut_size,data_size)
{

 y<-(data_size*2000)/aut_size
 return (y)
}

RrMean <- function (rr)
{
  
  e<-data.frame(rr)
  
  #grouping variable for every 20 lines                             
  grp<-(seq.int(nrow(e))-1) %/% 20 + 1
  
  #use aggregate to calculate mean for groups
  rr1<-aggregate(.~grp,e, mean)
  rr1<-rr1[,-1]
  return (as.data.frame(rr1))
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


activite1 <- function(l)
{
  s <- 0
  for(i in 1:length(l))
  {
    s <- s + abs(l[[i]]["a"])
  }
  return(s)
}


LabelTemp <- function(x)
{
  
  ifelse(x<29,'Tlow',ifelse(x>=30 & x<32,'Tmeduim','Tnormal'))
  
}

