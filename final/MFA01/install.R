#install.packages(c("devtools", "roxygen2", "testthat", "knitr"))


# setwd('/Users/Shiying/Dropbox/BERKELEY_study/2016_Fall/02_STAT243/243_proj/Stats-243/final')

setwd('C:/Users/Emma/Downloads/stats-243/final/')

library(devtools)

pkg <- 'MFA01'

devtools::document(pkg = pkg)
devtools::check_man(pkg = pkg)
devtools::test(pkg = pkg)
devtools::build_vignettes(pkg = pkg)
devtools::build(pkg = pkg)
devtools::install(pkg = pkg)

library(MFA01)


# --------------------below is function testing
a <- MFA()
a

a <- MFA(ncomps = 2)
a
# x <- matrix(1:100, nrow=10, ncol=10)
# s1 <- list(1:3,5:6,7:10)
# MFA(x,s1)
