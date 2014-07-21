################################################################################################# 
# Name of file           AutClass.R                                                             #
# Creation Date          05/05/2014                                                             #
# last modification      29/05/2014                                                             #
# Version                2.0                                                                    # 
# @Description           Aut calss contains two signals (ParaSymp,Symp)                         #
# @author                Djedou Zakaria                                                         #
#################################################################################################

.Aut.valid <- function(object){ return(TRUE)}
setClass (
  Class ="Aut",
  representation= representation(ParaSymp="Signal",Symp="Signal"),
  validity =.Aut.valid
)
rm (.Aut.valid )

################################################################
# @Description   getter                                        # 
#                                                              #
# @return        ParaSymp                                      #
# @return        Symp                                          #
################################################################

setMethod( f ="[",signature ="Aut",
           definition = function(x,i,j,drop){
             switch(EXPR=i,
                    "ParaSymp"={return(x@ParaSymp)},
                    "Symp"={return(x@Symp)},
                    stop("cet attribut n'existe pas!")
             )
           }
)



################################################################
# @Description   Load Aut file from csv                        # 
#                                                              #
# @param         file_path                                     #
# @return        created object (class instantiation)          #
################################################################

LoadAut <- function(file_path)
{
  # use readLines() to get file line-by-line 
  e <- readLines(file_path)
  
  # filter out "error"
  e <- e[grep("Erreur", e, invert=TRUE)]
  
  # turn structure into a string we can pass to textConnection
  e <-read.csv(textConnection(paste(e, collapse="\n")), header=TRUE)

  s <- e
  parasymp <- new(Class="Signal", val=as.numeric(unlist(s["ParaSymp"])))
  symp <- new(Class="Signal", val=as.numeric(unlist(s["Symp"])))
  Temp <- new(Class="Signal", val=as.numeric(unlist(s["Temp"])))
  return(new(Class="Aut",ParaSymp = parasymp, Symp = symp))
  
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
  grp<-(seq.int(nrow(e))-1) %/% 10 
  
  #use aggregate to calculate mean for groups
  aut2<-aggregate(.~grp,e, mean)
  aut2<-aut2[,-1]
  return (as.data.frame(aut2))
  
}

Cutaut <- function (x,y)
{
  z <- data.frame(LFHFratio= numeric(0))
  
  for (i in 1:length(x))
  {
    z<-x/y  
  }
  return (z)
}

LabelHRV <- function(x)
{
  
  ifelse(x<1,'PSNS','SNS')
  
}