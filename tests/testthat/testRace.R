

context("Testing lpSolve")

test_that("LpSolve finds perfect partitions", {
  if(!require(lpSolve)) {
    skip("lpSolve not available")
  }
  for(i in 10:25) {
    sizes <- seq(i)
    part <- PartLpSolve(sizes)
    expect_true(PartIsPerfect(sizes, part))
  }
})
          

test_that("LpSolve finds optimal partitions", {
  if(!require(lpSolve)) {
    skip("lpSolve not available")
  }
  for(i in 10:25) {
    sizes <- 10 * seq(i)
    part <- PartLpSolve(sizes)
    expect_equal(PartIsPerfect(sizes, part), i %% 4 == 0 || i %% 4 == 3)
  }  
})


context("Testing race")

BeatMe <<- function(sizes) sample(c(TRUE,FALSE),length(sizes), replace=TRUE)
Empty  <<- function(sizes) logical(length(sizes))

test_that("BeatMe wins against Empty", {
  tasks <- PartGenInt( m = 20)
  candidates <- c("BeatMe", "Empty")
  results <- PartRace(candidates, tasks, interactive = FALSE)
  expect_equal(results$race$best, 1)
})


test_that("lpSolve wins against BeatMe", {
  if(!require(lpSolve)) {
    skip("lpSolve not available")
  }
  tasks <- PartGenTodd( 2:15)
  candidates <- c("PartLpSolve", "BeatMe")
  results <- PartRace(candidates, tasks, first.test = 10, interactive = FALSE)
  expect_equal(results$race$no.tasks, 10)
  expect_equal(results$race$best, 1)
})


test_that("lpSolve wins against 2 BeatMe", {
  tasks <- PartGenInt( m = 50)
  candidates <- c("PartLpSolve", "BeatMe", "BeatMe")
  results <- PartRace(candidates, tasks, first.test = 20, interactive = FALSE)
  expect_equal(results$race$no.tasks, 20)
  expect_equal(results$race$best, 1)
})


test_that("Check timeout", {
  tasks <- PartGenInt( m = 10)
  Timeout <<- function(sizes) {
      Sys.sleep(1)
      Sys.sleep(1)
      sample(c(TRUE,FALSE),length(sizes), replace=TRUE)
  }
  OneInt  <<- function(sizes) {
      part <- logical(length(sizes))
      if(length(part) > 0) part[1] <- TRUE
      return(part)
  }
  candidates <- c("OneInt", "Timeout")
  results <- PartRace(candidates, tasks, first.test = 5, interactive = FALSE, timeout = 1)
  expect_equal(results$race$best, 1)
})

rm(list = c("Empty", "BeatMe"), envir = globalenv())


