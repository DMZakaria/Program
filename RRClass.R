################################################################################################# 
# Name of file           RRClass.R                                                              #
# Creation Date          05/05/2014                                                             #
# last modification      29/05/2014                                                             #
# Version                2.0                                                                    # 
# @Description           RR calss contains two signals (RR,HR)                                  #
# @author                Djedou Zakaria                                                         #
#################################################################################################

.RR.valid <- function(object){ return(TRUE)}
setClass (
  Class ="RR",
  representation= representation(RR="Signal",HR="Signal"),
  validity =.RR.valid
)
rm (.RR.valid )

################################################################
# @Description   getter                                        # 
#                                                              #
# @return        RR                                            #
# @return        HR                                            #
################################################################

setMethod( f ="[",signature ="RR",
           definition = function(x,i,j,drop){
             switch(EXPR=i,
                    "RR"={return(x@RR)},
                    "HR"={return(x@HR)},
                    stop("cet attribut n'existe pas!")
             )
           }
)

################################################################
# @Description   Load RR file from csv                         # 
#                                                              #
# @param         file_path                                     #
# @return        created object (class instantiation)          #
################################################################

LoadRR <- function(file_path)
{
  e <- read.csv(file_path, col.names = c("RR","HR"))
  taille <- dim(e)[1]
  s <- e[10:(taille-20),]
  rr <- new(Class="Signal", val=as.numeric(unlist(s["RR"])))
  hr <- new(Class="Signal", val=as.numeric(unlist(s["HR"])))
  return(new(Class="RR",RR = rr, HR = hr))
  
}

