################################################################################################# 
# Name of file           AutClass.R                                                             #
# Creation Date          05/05/2014                                                             #
# last modification      29/05/2014                                                             #
# Version                2.0                                                                    # 
# @Description           Aut calss contains two signals (ParaSymp,Symp)                         #
# @author                Djedou Zakaria                                                         #
#################################################################################################


#####################
#Mother class signal#
#####################

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
  e <- read.csv(file_path, col.names = c("Time","ParaSymp","Symp"))
  taille <- dim(e)[1]
  s <- e[10:(taille-20),]
  parasymp <- new(Class="Signal", val=as.numeric(unlist(s["ParaSymp"])))
  symp <- new(Class="Signal", val=as.numeric(unlist(s["Symp"])))
  Time <- new(Class="Signal", val=as.numeric(unlist(s["Time"])))
  return(new(Class="Aut",ParaSymp = parasymp, Symp = symp))
  
}
