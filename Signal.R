################################################################################################# 
# Name of file           RRClass.R                                                              #
# Creation Date          05/05/2014                                                             #
# last modification      29/05/2014                                                             #
# Version                2.0                                                                    # 
# @Description           RR calss contains two signals (RR,HR)                                  #
# @author                Djedou Zakaria                                                         #
#################################################################################################

.Signal.valid <- function(object){ return(TRUE)}
setClass (
  Class ="Signal",
  representation= representation(val="numeric"),
  validity =.Signal.valid
)
rm (.Signal.valid )

## Getteurs
setMethod( f ="[",signature ="Signal",
           definition = function(x,i,j,drop){
             switch(EXPR=i,
                    "val"={return(x@val)},
                    stop("this attribute does not exist!")
             )
           }
)

## Setteurs
setReplaceMethod(
  f ="[",
  signature ="Signal",
  definition = function(x,i,j,value){
    switch(EXPR=i,
           "val"={x@val <-value},
           stop("this attribute does not exist!")
    )
    validObject (x)
    return(x)
  }
)

