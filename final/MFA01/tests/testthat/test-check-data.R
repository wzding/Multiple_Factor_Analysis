context("Mfa arguments")

test_that("scale_data with ok vectors", {
  x <- matrix(1:100, nrow=10, ncol=10)
  s1 <- list(1:3,5:6,7:10)
  s2 <- list(2:4,5:7,8:10)
  y <- x; z<-x
  y[,unlist(s1)] <- scale(x[,unlist(s1)])/((nrow(x)-1)^0.5)
  z[,unlist(s2)] <- scale(x[,unlist(s2)])/((nrow(x)-1)^0.5)

  expect_equal(scale_data(x,s1),y)
  expect_equal(scale_data(x,s2),z)
})

test_that("scale_data fails with invalid lengths", {
  x <- matrix(1:100, nrow=10, ncol=10)
  expect_error(scale_data(x,list("a","b","c")))
})


test_that("scale_data fails with invalid types", {
  expect_error(scale_data(c('one', 'two', 'three'), c(1,2,3)))
})

context("Validation of number of component arguments")

test_that("check_n with ok vectors", {
  expect_true(check_n(1))
  expect_true(check_n(NULL))
})


test_that("check_n fails with invalid number", {
  expect_error(check_n(-5))
})

test_that("check_n fails with invalid types", {
  expect_error(check_n("a"))
})

#
context("Between-Table Structure arguments")
test_that("RV_table fails with invalid lengths", {
  x <- matrix(1:100, nrow=10, ncol=10)
  expect_error(RV_table(x,list("a","b","c")))
})


test_that("RV_table fails with invalid types", {
  expect_error(RV_table(c('one', 'two', 'three'), c(1,2,3)))
})

test_that("Lg_table fails with invalid lengths", {
  x <- matrix(1:100, nrow=10, ncol=10)
  expect_error(Lg_table(x,list("a","b","c")))
})

test_that("Lg_table fails with invalid types", {
  expect_error(Lg_table(c('one', 'two', 'three'), c(1,2,3)))
})

