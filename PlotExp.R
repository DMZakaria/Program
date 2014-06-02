setGeneric( name ="plotAll",
            def = function(x ){standardGeneric("plotAll")}
)


# method
#imprime toutes les valeurs d'une data
setMethod( f ="plotAll",signature ="data",
           def = function(x)
           {
             par(mfrow=c(3,2))
             plot.ts(x["X"]["val"])
             plot.ts(x["Y"]["val"])
             plot.ts(x["Z"]["val"])
             plot.ts(x["Temp"]["val"])
           }
)

# imprime X, Y et Z
setMethod( f ="plot",signature =c(x="data",y="ANY"),
           def = function(x,y)
           {
             par(mfrow=c(2,2))
             plot.ts(x["X"]["val"], ylab="X")
             plot.ts(x["Y"]["val"], ylab="Y")
             plot.ts(x["Z"]["val"], ylab="Z")
           }
)

# imprime un graphique à l'écran
setMethod( f ="plot",signature ="Signal",
           def =function(x)
           {
             plot.ts(x["val"])
           }
)