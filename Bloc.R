####   Classe Bloc   ####
.Bloc.valid <- function(object){ return(TRUE)}
setClass (
  Class ="Bloc",
  representation= representation(a="numeric",b="numeric",centre="numeric"),
  validity =.Bloc.valid
)
rm (.Bloc.valid )

## Getteurs
setMethod( f ="[",signature ="Bloc",
           definition = function(x,i,j,drop){
             switch(EXPR=i,
                    "a"={return(x@a)},
                    "b"={return(x@b)},
                    "centre"={return(x@centre)},
                    "tendance"={return(x@tendance)},
                    stop("cet attribut n'existe pas!")
             )
           }
)

