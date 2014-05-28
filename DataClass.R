################################################################################################# 
# Name of file           DataClass.R                                                            #
# Creation Date          05/05/2014                                                             #
# last modification      29/05/2014                                                             #
# Version                2.0                                                                    # 
# @Description           Aut calss contains 4 signals (X,Y,Z,Temperature) and (Sujet,numExp)    #
# @author                Djedou Zakaria                                                         #
#################################################################################################


#####################
#Mother class signal#
#####################

.Data.valid <- function(object){ return(TRUE)}
setClass (
  Class ="Data",
  representation= representation(numExp="numeric",X="Signal",Y="Signal",Z="Signal",Temp="Signal",Sujet="numeric"),
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
# @return        numExp                                        #
################################################################

setMethod( f ="[",signature ="Data",
           definition = function(x,i,j,drop){
             switch(EXPR=i,
                    "numExp"={return(x@numExp)},
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

LoadData <- function(file_path, NumExp,sujet)
{
  e <- read.table(file_path,header=TRUE,sep=",",col.names = c("X","Y","Z","Temp"))
  taille <- dim(e)[1]
  s <- e[1:(taille-2),]
  x <- new(Class="Signal", val=as.numeric(unlist(s["X"])))
  y <- new(Class="Signal", val=as.numeric(unlist(s["Y"])))
  z <- new(Class="Signal", val=as.numeric(unlist(s["Z"])))
  temp <- new(Class="Signal", val=as.numeric(unlist(s["Temp"])))
  return(new(Class="Data", numExp=NumExp,
             X=x, Y=y, Z=z, Temp = temp, Sujet = sujet))
  
}

# # rapide description
# setMethod( f ="show",signature ="Data",
#            def = function(object)
#            {
#              s <- paste("Exeperience:",object@numExp," Sujet:",object@Sujet,"\n")
#              cat(s)
#            }
# )
