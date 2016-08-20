#' Generate Todd Subset Sum problem's instances.
#'
#' @param n numeric vector of integer set cardinalities
#' @return a list of integer sets (numeric vector).
#'
#' @section References:
#' See Knapsack Problems, Sylvano Martello and Paolo Toth, chapter 4, p. 128.
#'
#' @export
#' @examples
#' PartGenTodd(5)
#' PartGenTodd(2:4)
PartGenTodd <- function(n) {
  stopifnot(is.numeric(n), n > 0, n <= 45)
  ## Generate Todd problems
  GenTodd <- function(n) { 
    ## Generate Todd problem with n integers
    k <- floor(log2(n))
    sizes <- 2**(k + n + 1) + 2**(k + seq.int(n)) + 1
  }
  sapply(n, GenTodd) 
}
