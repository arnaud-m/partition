
context("Testing utility functions")


test_that("PartEval works", {
  sizes <- seq(6)
  part <- rep(c(T,F), 3)
  expect_equal(PartEval(sizes, part),3)
  part[6] <- TRUE
  expect_equal(PartEval(sizes, part),9)
  part[4] <- TRUE
  expect_equal(PartEval(sizes, part),17)
  part[2] <- TRUE
  expect_equal(PartEval(sizes, part),21)
})

test_that("PartPrint prints", {
  sizes <- seq(6)
  part <- rep(c(T,F), 3)
  sys.out <- capture.output(PartPrint(sizes, part))
  expect_true( length( sys.out) > 0)
})


test_that("PartIsPerfect works for even sets", {
  sizes <- seq(4)
  part <- c(F,T,T,F)
  expect_true(PartIsPerfect(sizes, part))
  part[1] <- T
  expect_false(PartIsPerfect(sizes, part))
})

test_that("PartIsPerfect works for odd sets", {
  sizes <- seq(6)
  part <- c(F,F,F,T,F,T)
  expect_true(PartIsPerfect(sizes, part))
  part[1] <- T
  expect_true(PartIsPerfect(sizes, part))
  part[1] <- F
  part[2] <- T
  expect_false(PartIsPerfect(sizes, part))
})

test_that("small TODD instances are valid", {
  instances <- PartGenTodd(1:3)
  expect_equal( length(instances), 3)
  expect_true( all(sapply(instances, length) == 1:3))
  expect_equal(instances[[1]], 7)
  expect_true( all(instances[[2]] == c(21, 25)))
  expect_true( all(instances[[3]] == c(37, 41, 49)))
})



test_that("instances generation works", {
  n <- 10
  precision = 1000
  instances <- PartGenInt(n, cardinalities = n, precision = precision)
  expect_equal(length(instances), n)
  expect_true(all(sapply(instances, length) == n))
  expect_true(all(sapply(instances, function(x) all(x > 0 & x <= precision))))
})
