#' @title Plot of object mfa
#' @description Plots the common factor scores and partial factor scores
#' @param x an object of class \code{"mfa"}
#' @param cfs whether to plot common factor scores
#' @param pfs whether to plot partial factor scores
#' @param pfl whether to plot factor loadings
#' @param num number of data table/block to plot
#' @param \dots arguments to be passed to/from other methods
#' @export
#' @examples
#'  \dontrun{
#'  # create a \code{"mfa"} and plot its common factor scores
#'  a <- MFA()
#'
#'  plot(a)
#'  }
#'

plot.mfa <- function(x, cfs = TRUE, pfs = FALSE, pfl = FALSE, num=1,eig=FALSE,...) {
  plot(x$cfs[,1:2],xaxt="n", yaxt="n",
       xlab="1st component",
       ylab="2nd component",
       bty="n",cex=1.5)
  # pch = c(rep(15,4), rep(16,4), rep(17,4))
  axis(side=1,pos = 0)
  axis(side=2,pos = 0)
  abline(v=0,h=0,col = "gray70", lwd = 1.5)
  # text(GSVD.factor.score[,1],GSVD.factor.score[,2],labels = label ,cex=0.8,pos=3)
  title(main="Common Facor scores")
  # legend("bottomright",pch = c(15, 16, 17),col = c("red", "green","blue"),
  #        c("Canada","France","New Zealand"))

  if(pfs && pfl){
    pfs_scale <- rescale(x$pfs,sqrt(x$eigen))
    loading_scale <- rescale(x$pfl,sqrt(x$eigen))
    biplot(pfs_scale[[num]][,1:2],loading_scale[[num]][,1:2],xlab="1st component",ylab="2nd component")
    abline(v=0,h=0)
    title(main=paste("Partial Facor scores and variable loadings (No.", num," data table)"))
  }
  else if(pfs){
    plot(x$pfs[[num]][,1:2],xaxt="n", yaxt="n",xlab="1st component",ylab="2nd component",
         bty="n",cex=1.5)
    axis(side=1,pos = 0)
    axis(side=2,pos = 0)
    abline(v=0,h=0)
    # text(pfs[[num]][,1],pfs[[num]][,2],cex=0.8,pos=3)
    title(main=paste("Partial facor scores (No.", num," data table)" ))
  }
  else if(pfl){
    plot(x$pfl[[num]][,1:2],xaxt="n", yaxt="n",xlab="1st component",ylab="2nd component",
         bty="n",cex=1.5)
    axis(side=1,pos = 0)
    axis(side=2,pos = 0)
    abline(v=0,h=0)
    # text(pl[[num]][,1],pl[[num]][,2],cex=0.8,pos=3)
    title(main=paste("Variable loadings (No.", num," data table)" ))
  }
  else if(eig){
    plot_eig(x)
  }
}

plot_eig <- function(x,...){
  colors=c("black","red","blue","yellow","pink","orange","green","purple")
  barplot(x$eigen,main="eigenvalues histogram",col = colors,
          names.arg =paste(x$eigen) )
  
}
# auxiliary functions for plot.mfa() method

# get scale factor for pfs and loadings so that their variance is equal to the singular value
scale_factor <- function(d,s){
  f1 <- sqrt(apply(d, 2, var)[1]/s[1])
  f2 <- sqrt(apply(d, 2, var)[2]/s[2])
  scale_factors <- c(f1,f2)
  return(scale_factors)
}

# rescale  pfs and loadings so that their variance is equal to the singular value
rescale <- function(d,s){
  rescale <- list()
  for(i in 1:length(d)){
    rescale[[i]] <- scale(d[[i]][,1:2],center=FALSE,scale=scale_factor(d[[i]],s))}
  return(rescale)
}
