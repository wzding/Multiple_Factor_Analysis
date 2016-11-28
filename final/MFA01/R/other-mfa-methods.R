#' @title summary method
#' @description Summarizes information about the obtained eigenvalues
#' @param x an R object
#' @param digit number of decimal digit in print output
#' @export
ev.summary <- function(x) UseMethod("ev.summary")

#' @export
ev.summary.mfa <- function(x,digit=3, ...) {
  options(digits=digit)
  Eigenvalue <- x$eigen
  Component <- 1:length(Eigenvalue)
  SingularValue <- sqrt(Eigenvalue)
  CumulativeEigenvalue <- cumsum(Eigenvalue)
  Inertia <- Eigenvalue/sum(Eigenvalue) * 100
  CumulativeInertia <-cumsum(Inertia)
  ev <- t(data.frame(Component, SingularValue, Eigenvalue,
                     CumulativeEigenvalue,Inertia,CumulativeInertia))
  print(ev)
  invisible(x)
}


#' @title contribution method
#' @description Summarizes contribution of an observation to a dimension
#' @param x an R object
#' @param obs which observation to analyze
#' @param comp which component to analyze
#' @export
ctr.obs <- function(x) UseMethod("ctr.obs")

#' @export
ctr.obs.mfa <- function(x,obs=NULL,comp=NULL,  ...) {
  t <- x$cfs^2/nrow(x$cfs)
  ctr <- sweep(t,2,colSums(t),'/')
  if (is.null(obs) & is.null(comp)){
    return(ctr)
  } else if (!is.null(obs) & is.null(comp) ){
      return(ctr[obs,])
  } else if (is.null(obs) & !is.null(comp) ){
      return(ctr[,comp])
  }
    else{
      return(ctr[obs,comp])
  }
}

#' @title contribution method
#' @description Summarizes contribution of a variable to a dimension
#' @param x an R object
#' @param var which variable to analyze
#' @param comp which component to analyze
#' @export
ctr.var <- function(x) UseMethod("ctr.var")

#' @export
ctr.var.mfa <- function(x, var=NULL,comp=NULL,  ...) {
  t <- x$fl^2
  ctr <- sweep(t,1,x$weights,'*')
  if (is.null(var) & is.null(comp)){
      return(ctr)
  } else if(!is.null(var) & is.null(comp) ){
      return(ctr[var,])
  }
    else if(is.null(var) & !is.null(comp)){
      return(ctr[,comp])
  }
    else{
      return(ctr[var,comp])
  }
}


#' @title contribution method
#' @description Summarizes contribution of a table/block to a dimension
#' @param x an R object
#' @param var which table/block to analyze
#' @param comp which component to analyze
#' @export
ctr.table <- function(x) UseMethod("ctr.table")

#' @export
ctr.table.mfa <- function(x, table = NULL,comp=NULL,...) {
  t <- ctr.var(x)
  idx <- x$index_lists
  ctr.slt <- split(data.frame(t),idx)
  ctr.table <- list()
  for (i in 1:length(ctr.slt)){
    ctr.table[[i]]  <- apply(ctr.slt[[i]], 2,sum)}

  if (is.null(table) & is.null(comp)){
      return(ctr.table)
  } else if(!is.null(table) & is.null(comp) ){
      return(ctr[[table]])
  }
    else if(!is.null(table) & !is.null(comp)){
      return(ctr.table[[table]][comp])
    }
}

