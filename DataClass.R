################################################################################################# 
# Name of file           DataClass.R                                                            #
# Creation Date          05/05/2014                                                             #
# last modification      29/05/2014                                                             #
# Version                2.0                                                                    # 
# @Description           Aut calss contains 4 signals (X,Y,Z,Temperature) and (Sujet,numnight)    #
# @author                Djedou Zakaria                                                         #
#################################################################################################


#####################
#Mother class signal#
#####################

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


# # rapide description
# setMethod( f ="show",signature ="Data",
#            def = function(object)
#            {
#              s <- paste("Exeperience:",object@numnight," Sujet:",object@Sujet,"\n")
#              cat(s)
#            }
# )
