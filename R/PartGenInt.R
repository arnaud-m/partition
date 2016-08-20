#' Generate a list of partition problem's instances
#'
#' For each parameter, generate m sets of integer.
#' The parameters are the cartesian product of the \code{cardinalities}, the \code{precisions}, and the multi-modal distributions \code{modes}.
#'
#' The mode of an integer is drawn with a a uniform distribution.
#' A mode is an integer such that :
#'  0 <= mode  : uniform distribution
#'  0< mode <1 : binomial with probability of success 'mode'
#'  mode >= 1  : binomial with random probability of success
#' @param m number of integer sets for each parameter
#' @param cardinalities cardinalities of the integer sets.
#' @param precisions maximum values of an integer. The minimal value is 1.
#' @param modes a distribution mode (numeric vector) or a list of distribution modes (multi-modal distribution).
#' @return a list of partition problem's instances (numeric vector of strictly positive integers). The list is changed into a vector if its length is 1.
#'
#' @importFrom stats rbinom
#' @importFrom stats runif 
#' @export
#' @examples
#' PartGenInt()
#' PartGenInt( m = 2, cardinalities = c(5,10), precisions = c(10, 100))
#' PartGenInt( modes = list(0, 1, c(0.2, 0.8)))
PartGenInt <- function(m = 1, cardinalities = 10, precisions = 100, modes = 0) {
  stopifnot(
    is.numeric(m), m > 0, length(m) == 1,
    is.numeric(cardinalities), cardinalities > 0,
    is.numeric(precisions), precisions > 1
    )
  ## Wrap single mode
  if(! is.list(modes) ) {
    modes <- list(modes)
  }
  stopifnot(sapply(modes, is.numeric))

  ## Generate paramaters configurations
  params <- expand.grid(precision=precisions, mode=modes, n=cardinalities)

  ## Generate m set of integers with the same parameters
  GenInt <- function(param) {
    ## Draw random probabilities of success for binomial distributions of some modes
    modes <- unname(unlist(param$mode))
    draw.probs <- which(modes >= 1)
    modes[draw.probs] <- runif(length(draw.probs))
    ## Generate a single integer following a mode
    GenOneInt <- function(mode) {
      ifelse( mode <= 0,
             sample.int(param$precision, 1),
             1 + rbinom(1, min(param$precision, 10**9)-1, mode)
             )
    }
    ## Draw the mode of each integer
    draw.modes <- sample(modes, param$n, replace=TRUE)
    return(replicate(m, sapply(draw.modes, GenOneInt), simplify = FALSE))
  }
  ## Generate m sets of integers for each param
  x <- unlist(apply(params, 1, GenInt), recursive = FALSE)
  # Postprocess the list of instances
  if(length(x) == 1) {
    ## Return a vector if there is only one instance.
    return(unlist(x))
  } else {
    ## Return a list of instances
    return(x)
  }
}

