#' @title Multiple Factor Analysis
#' @description Creates an object of class \code{"mfa"}
#' @param data data set (matrix or data frame)
#' @param sets list of vector indicating the sets of variables (i.e. the blocks)
#' @param ncomps integer indicating how many number of components (i.e. factors) are to be extracted
#' @param center either a logical value or a numeric vector of length equal to the number of active variables in the analysis
#' @param scale either a logical value or a numeric vector of length equal to the number of active variables in the analysis
#' @return an object of class \code{"mfa"} with the following elements:
#' @return \item{eigen}{vector of eigenvalues}
#' @return \item{cfs}{matrix of common factor scores}
#' @return \item{fl}{matrix of loadings (a.k.a. factor loadings)}
#' @return \item{pfs}{matrix of partial factors scores}
#' @export
#' @examples
#' # default
#' mfa1 <- MFA()
#'
#' # another mfa
#' mfa2 <- MFA(ncomps=2)
#'

MFA <- function(data = get(load("data/wine.rda")),
                sets =list(2:7,8:13,14:19,20:24,25:30,31:35,36:39,40:45,46:50,51:54),
                ncomps = NULL, center=TRUE,scale=TRUE) {
  # check whether each table is numeric
  data <- scale_data(d = data,s= sets,ctr = center, sc=scale)
  # check ncomps
  check_n(n = ncomps)
  # subset data
  aj <- subset_data(d = data, s = sets)
  assessor <- aj [[1]] ; j <- aj[[2]] # a : assessor ; j: length of each element in a
  index_list <- rep(1:length(j),times=j)
  #SVD
  weight <- SVD(a=assessor)
  #GSVD - return list(eigen,cfs,fl)
  gsvd <- GSVD(d=data,s=sets,w=weight,n=ncomps)
  # return list(pfl, pfs)
  pf <- PFS(a= assessor ,idx = index_list,w= weight,fl = gsvd[[3]])

  object <- list(
    assessors = assessor,
    index_lists = index_list,
    weights = weight,
    eigen= gsvd[[1]], # vector
    cfs = gsvd[[2]], # matrix
    fl = gsvd[[3]], # matrix
    pfl = pf[[1]],
    pfs = pf[[2]])  # list
  class(object) <- "mfa"
  object
}


# private function to scale data
scale_data <- function(d,s,ctr=TRUE,sc=TRUE) {
  data <- data.matrix(d)
  t <- try(scale(data[,unlist(s)]),silent = T)
  if("try-error" %in% class(t)) {
    stop("\neach table/block must be numeric")
  }
  # sum of the square values of all its elements is equal to 1
  data[,unlist(s)] <- scale(data[,unlist(s)],center = ctr, scale = sc)/((nrow(data)-1)^0.5)
  return(data)
}



# private function to check number of components
check_n <- function(n){
  if (!is.null(n)){
    if (n <= 0 | !is.numeric(n)) {
    stop("\nargument 'ncomps' must be a positive integer")
    }
    else{TRUE}
  } else {
  TRUE
  }
}


# private function to subset data (by column)
subset_data <- function(d, s) {
  d <- data.matrix(d)
  a <- list()
  j <- list()
  for(i in 1:length(s)){
    a[[i]] <- d[,s[[i]]]
    j[[i]] <- ncol(a[[i]])
  }

  newList <- list(a,j)
  return(newList)
}


# private function to get SVD, input should be assessor (list)
SVD <- function(a){
  svd_data <- list()
  weight <- c()
  # sgl <- list()
  # fs <- list()
  for(i in 1:length(a)){
    svd_data[[i]] <- svd(a[[i]])
    # sgl[[i]] <- diag(svd[[i]]$d,length(svd[[i]]$d),length(svd[[i]]$d))
    # fs[[i]] <-  ((-1) * svd[[i]]$u) %*% sgl[[i]]
    weight <- c(weight,rep(1/svd_data[[i]]$d[1]^2,ncol(a[[i]])))
  }
  return(weight)
}


# private function to get GSVD, input should be whole data
GSVD <- function(d,s,w,n=2){
  # get constrains
  d <- as.matrix(d[,unlist(s)])
  len <- nrow(d)
  m <-  diag(1/len,len,len)
  m_0.5 <- diag((1/len)^(-0.5),len,len)
  weight.mx <- diag(w,length(w),length(w))
  weight_0.5.mx <-  diag(w^(-0.5),length(w),length(w))
  # GSVD matrix
  y <- sqrt(m) %*% d %*%sqrt(weight.mx)
  GSVD <- svd(y)
  if(is.null(n)){
    left <- m_0.5 %*% GSVD$u
    sgl <- GSVD$d
    #loading - table 3 loadings (Q)
    fl <- weight_0.5.mx %*% GSVD$v
    # common factor scores - each row is a wine and each column is a component
    cfs <- left %*% diag(sgl,ncol(GSVD$u),ncol(GSVD$u))
  } else {
    left <- m_0.5 %*% GSVD$u[,1:n]
    sgl <- GSVD$d[1:n]
    fl <- weight_0.5.mx %*% GSVD$v[,1:n]
    # common factor scores - each row is a wine and each column is a component
    cfs <- left %*% diag(sgl,n,n)
  }
    # eigenvalue and inertia
    eigen <- sgl^2
    newList <- list(eigen,cfs,fl)
    return(newList)
}


# private function to get particial factor scores,input should include both assessor and factor loading
PFS <- function(a,idx,w,fl){
  pfl <- list()
  pfs <- list()
  for(i in 1:length(a)){
    pfl[[i]] <- split(data.frame(fl),idx)[[i]]  #subset by row
    pfs[[i]] <- length(a)* w[i]* as.matrix(a[[i]]) %*% as.matrix(pfl[[i]])
  }
  newList <- list(pfl,pfs)
  return(newList)
}

#' @export
print.mfa <- function(x, ...) {
  cat('object "mfa"\n\n')
  cat("Number of tables/blocks for analysis:", length(x$assessors),"\n")
  cat("Weight of tables:", "\n",unique(x$weights), "\n")
  cat("First two eigenvalues: ", x$eigen[1:2], "\n")
  cat("First component of common factor scores: ","\n",x$cfs[,1], "\n")
  invisible(x)
}

#' @rdname MFA
#' @param x an R object
#' @export
is.mfa <- function(x) {
is(x, "mfa")
}


############### Below is function testing
# source('MFA01/R/plot-mfa.R')
# a <- MFA()
# a$pfs
# ev.summary(a)
# plot(a,cfs = FALSE, pfs = TRUE)
