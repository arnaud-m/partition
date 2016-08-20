#' Evaluate the objective value of the partition 
#'
#' @param sizes a numeric vector which represents the integers (>0) in the set S.
#' @param part a logical vector which represents the indicator function of S1 in the partition S=S1+S2.
#' @return the absolute difference between the sum of integers in S1 and S2.
#'
#' @export
#' @examples
#' sizes <- c(13, 11, 8, 20, 20, 11, 16, 17, 4, 13)
#' part <-  c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE)
#' PartEval(sizes, part)
PartEval <- function(sizes, part) abs(sum(sizes[part])-sum(sizes[!part]))
