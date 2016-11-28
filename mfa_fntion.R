setClass(
  Class = "mfa",
  representation = representation(F="matrix")
)

# print
setMethod(
  "print",
  signature = "mfa",
  function(x, ...){
    cat('object "mfa"\n\n')
    cat("Dimensions of raw data (K):\n")
    cat("K  rows  columns ")
    cat(x@k, x@rows, x@cols)
    cat("First principle component of each table: \n")
    print(x@fpcs) # fpcs will be a k by 1 array
    cat("The first two principle components for the grand matrix: (Delta): \n")
    print(x@Delta) # will be a 2 by 1 array, 2 is rank of the grand matrix
  }
  
)
# plot 1: common factor
plotFactor= function(x,...){
  Fmat=x@F
  xcor=Fmat[,1]
  ycor=Fmat[,2]
  colors=c("black","red","blue")
  plot(xcor,ycor)
  points(xcor,ycor,cex=1,col=colors,pch=12)
  title("Factor score")
}
setGeneric("plotFactor")

plotEig=function(x,...){
  Fmat=x@F
  xcor=Fmat[,1]
  ycor=Fmat[,2]
  colors=c("black","red","blue")
  hist(xcor,main="eigenvalues histogram")
}
setGeneric("plotEig")

plotPfs=function(x,...){
  Fmat=x@F
  xcor=Fmat[,1]
  ycor=Fmat[,2]
  colors=c("black","red","blue")
  plot(xcor,ycor)
  points(xcor,ycor,cex=1,col=colors,pch=12)
  title("Partial Factor score")
}
setGeneric("plotPfs")
plotLoad=function(x,...){
  Fmat=x@F
  xcor=Fmat[,1]
  ycor=Fmat[,2]
  colors=c("black","red","blue")
  plot(xcor,ycor)
  points(xcor,ycor,cex=1,col=colors,pch=12)
  title("Loadings")
}
setGeneric("plotLoad")
