################################################################################################# 
# Name of file           DataClass.R                                                            #
# Creation Date          05/05/2014                                                             #
# last modification      29/05/2014                                                             #
# Version                2.0                                                                    # 
# @Description           Aut calss contains 4 signals (X,Y,Z,Temperature) and (Sujet,numnight)    #
# @author                Djedou Zakaria                                                         #
#################################################################################################

.Data.valid <- function(object){ return(TRUE)}
setClass (
  Class ="Data",
  representation= representation(numnight="numeric",X="Signal",Y="Signal",Z="Signal",Temp="Signal",Sujet="numeric",body_act="numeric"),
  validity =.Data.valid
)
rm (.Data.valid )

################################################################
# @Description   getter                                        # 
#                                                              #
# @return        X                                             #
# @return        Y                                             #
# @return        Z                                             #
# @return        Temp                                          #
# @return        Sujet                                         #
# @return        numnight                                        #
################################################################

setMethod( f ="[",signature ="Data",
           definition = function(x,i,j,drop){
             switch(EXPR=i,
                    "Num_Night"={return(x@numnight)},
                    "X"={return(x@X)},
                    "Y"={return(x@Y)},
                    "Z"={return(x@Z)},
                    "Temp"={return(x@Temp)},
                    "Sujet"={return(x@Sujet)},
                    stop("cet attribut n'existe pas!")
             )
           }
)

################################################################
# @Description   Load data file from csv                       # 
#                                                              #
# @param         file_path                                     #
# @return        created object (class instantiation)          #
################################################################

LoadData <- function(file_path)          
{
  e <- read.table(file_path,header=TRUE,sep=",",col.names = c("X","Y","Z","Temp"))
  taille <- dim(e)[1]
  s <- e[1:(taille-2),]
  x <- new(Class="Signal", val=as.numeric(unlist(s["X"])))
  y <- new(Class="Signal", val=as.numeric(unlist(s["Y"])))
  z <- new(Class="Signal", val=as.numeric(unlist(s["Z"])))
  temp <- new(Class="Signal", val=as.numeric(unlist(s["Temp"])))
  return(new(Class="Data",X=x, Y=y, Z=z, Temp = temp))
  
}



##############################################################
# @Description   compute the average of the activity level   # 
#                                                            #
# @param         data file solts (X,Y,Z)                     #
# @return        data frame                                  # 
##############################################################

activityMean <- function(x,synchro.var)
{
  x.x <- decouper(x["X"]["val"])
  x.y <- decouper(x["Y"]["val"])
  x.z <- decouper(x["Z"]["val"])
  x.xx <- spliter(x.x,synchro.var)
  x.yy <- spliter(x.y,synchro.var)
  x.zz <- spliter(x.z,synchro.var)
  x.ax <- unlist(lapply(x.xx,activite1))
  x.ay <- unlist(lapply(x.yy,activite1))
  x.az <- unlist(lapply(x.zz,activite1))
  return(
    as.data.frame(x.ax) +
      as.data.frame(x.ay) +
      as.data.frame(x.az)
  ) * 4 /3
  
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



###############################################################
# @Description   assign a value to activity                   # 
#                                                             #
# @param         Activity data frame                          #
###############################################################

LabelActivity <- function(x)
{
  
  ifelse(x<100,'null',ifelse(x>=100 & x<300,'medium','high'))
  
}

########################################################
# @Description   compute the average of the aut file   # 
#                                                      #
# @param         aut Slots (Sympa,ParaSymp)            #
# @return        data frame                            #
########################################################
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


# LoadData <- function(file_path)
# {
#   # use readLines() to get file line-by-line 
#   e <- readLines(file_path)
#   
#   # filter out "error"
#   e <- e[grep("Erreur", e, invert=TRUE)]
#   
#   # turn structure into a string we can pass to textConnection
#   e <-read.csv(textConnection(paste(e, collapse="\n")), header=TRUE)
#   s <- e
#   x <- new(Class="Signal", val=as.numeric(unlist(s["X"])))
#   y <- new(Class="Signal", val=as.numeric(unlist(s["Y"])))
#   z <- new(Class="Signal", val=as.numeric(unlist(s["Z"])))
#   temp <- new(Class="Signal", val=as.numeric(unlist(s["Temp"])))
#   return(new(Class="Data",X=x, Y=y, Z=z, Temp = temp))
#   
# }
