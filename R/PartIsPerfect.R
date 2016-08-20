#' Check if the partition is perfect.
#'
#' A partition is perfect if the difference between the sum of integers of S1 and S2 is equal to 0 (resp. 1) for a even (resp. odd) set.
#' 
#' @inheritParams PartEval
#'
#' @return \code{true} if the partition is perfect.
#' @export
#' @examples
#' PartIsPerfect(seq(4), c(FALSE,TRUE,TRUE,FALSE))
#' PartIsPerfect(seq(4), c(TRUE,TRUE,TRUE,FALSE))
PartIsPerfect <- function(sizes, part) PartEval(sizes, part) <= 1

