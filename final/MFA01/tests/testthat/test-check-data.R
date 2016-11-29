context("Mfa arguments")

test_that("scale_data with ok vectors", {
  x <- matrix(1:100, nrow=10, ncol=10)
  expect_true(scale_data(x,list(1:3,5:6,7:10)))
  expect_true(scale_data(x,list(2:4,5:7,8:10)))
})

test_that("scale_data fails with invalid lengths", {
  x <- matrix(1:100, nrow=10, ncol=10)
  expect_error(scale_data(x,list("a","b","c")))
})


test_that("scale_data fails with invalid types", {
  expect_error(scale_data(c('one', 'two', 'three'), c(1,2,3)))
})


test_that("subset_data with ok vectors", {
  x <- matrix(1:100, nrow=10, ncol=10)
  expect_true(subset_data(x,list(1:3,4:6,7:10)))
  expect_true(subset_data(x,list(2:4,5:7,8:10)))
})


test_that("subset_data fails with invalid lengths", {
  x <- matrix(1:100, nrow=10, ncol=10)
  expect_error(subset_data(x,c(1:3,4:6,9:13)))
})


test_that("subset_data fails with invalid types", {
  expect_error(subset_data(c('one', 'two', 'three'), c(1,2,3)))
})


context("Between-Table Structure arguments")
test_that("RV_table with ok vectors", {
  x <- matrix(1:100, nrow=10, ncol=10)
  expect_true(RV_table(x,list(1:3,5:6,7:10)))
  expect_true(RV_table(x,list(2:4,5:7,8:10)))
})

test_that("RV_table fails with invalid lengths", {
  x <- matrix(1:100, nrow=10, ncol=10)
  expect_error(RV_table(x,list("a","b","c")))
})


test_that("RV_table fails with invalid types", {
  expect_error(RV_table(c('one', 'two', 'three'), c(1,2,3)))
})

test_that("Lg_table with ok vectors", {
  x <- matrix(1:100, nrow=10, ncol=10)
  expect_true(Lg_table(x,list(1:3,5:6,7:10)))
  expect_true(Lg_table(x,list(2:4,5:7,8:10)))
})

test_that("Lg_table fails with invalid lengths", {
  x <- matrix(1:100, nrow=10, ncol=10)
  expect_error(Lg_table(x,list("a","b","c")))
})


test_that("Lg_table fails with invalid types", {
  expect_error(Lg_table(c('one', 'two', 'three'), c(1,2,3)))
})

