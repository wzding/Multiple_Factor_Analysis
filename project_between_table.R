# load packages
library(MASS)

# RV
RV <- function(table1, table2){
  m1 <- as.matrix(table1)
  m2 <- as.matrix(table2)
  m1m <- m1 %*% t(m1)
  m2m <- m2 %*% t(m2)
  res1 <- sum(diag( m1m %*% m2m))
  res2 <- sqrt( sum(diag( m1m %*% m1m)) * sum(diag( m2m %*% m2m)) )
  res1 / res2
}

RV_table <- function(dataset, sets = list(1:3, 4:5, 6:10)){
  K <- length(sets)
  res <- matrix(1, K, K)
  for(i in 1:K){
    for(j in i:K){
      res[i,j] <- RV(dataset[, sets[[i]]], dataset[, sets[[j]]])
      res[j,i] <- res[i,j]
    }
  }
  res
}

# Lg
Lg <- function(table1, table2){
  m1 <- as.matrix(table1)
  m2 <- as.matrix(table2)
  m1m <- m1 %*% t(m1)
  m2m <- m2 %*% t(m2)
  svd1 <- svd(m1)
  svd2 <- svd(m2)
  alpha1 <- 1 / max(svd1$d)^2
  alpha2 <- 1 / max(svd2$d)^2
  res <- sum(diag( m1m %*% m2m)) * alpha1 * alpha2
  res
}

Lg_table <- function(dataset, sets = list(1:3, 4:5, 6:10)){
  K <- length(sets)
  res <- matrix(1, K, K)
  for(i in 1:K){
    for(j in i:K){
      res[i,j] <- Lg(dataset[, sets[[i]]], dataset[, sets[[j]]])
      res[j,i] <- res[i,j]
    }
  }
  res
}

