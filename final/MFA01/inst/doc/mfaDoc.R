## ----eval=FALSE----------------------------------------------------------
#  source("install.R")

## ----eval=FALSE----------------------------------------------------------
#  library(MFA01)
#  mfa_obj=MFA()

## ----eval=FALSE----------------------------------------------------------
#  mfa_obj=MFA(data='your_data_set')

## ----eval=FALSE----------------------------------------------------------
#  plot_eig(mfa_obj)

## ----eval=FALSE----------------------------------------------------------
#  mfa_obj$cfs

## ----eval=FALSE----------------------------------------------------------
#  attributes(mfa_obj)$names

## ----eval=FALSE----------------------------------------------------------
#  mfa_obj

## ----eval=FALSE----------------------------------------------------------
#  plot(mfa_obj)

## ----eval=FALSE----------------------------------------------------------
#  plot(mfa_obj,FALSE,pfl=TRUE,pfs=TRUE,num=2)

## ----eval=FALSE----------------------------------------------------------
#  ev.summary(mfa_obj)

## ----eval=FALSE----------------------------------------------------------
#  ctr.obs(mfa_obj)
#  ctr.var(mfa_obj)
#  ctr.table(mfa_obj)

## ----eval=FALSE----------------------------------------------------------
#  table1=mfa_obj$assessors[[1]]
#  table2=mfa_obj$assessors[[2]]
#  RV(table1,table2)

## ----eval=FALSE----------------------------------------------------------
#  data=cbind(mfa_obj$assessors[[1]],mfa_obj$assessors[[2]],mfa_obj$assessors[[3]])
#  RV_table(data)
#  Lg(table1,table2)
#  Lg_table(data)

## ----eval=FALSE----------------------------------------------------------
#  bootStrap(mfa_obj)

