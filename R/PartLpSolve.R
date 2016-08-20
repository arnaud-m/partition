#' Solve a subset sum integer program that models the partition problem's instance using lpsolve.
#'
#' Symmetry breaking : the sum of integers in S1 is lower than the one in S_2.
#' WARNING: the timeout does not work. It does not interrupt the lpsolve subprocess. 
#' WARNING: the results are wrong for high precision integers.
#'
#' @inheritParams PartEval
#'
#' @export
#' @examples
#' sizes <- c(13, 11, 8, 20, 20, 11, 16, 17, 4, 13)
#' part <-  PartLpSolve(sizes)
#' PartPrint(sizes, part)
PartLpSolve <- function(sizes) {
  stopifnot(is.numeric(sizes))
  if( requireNamespace('lpSolve', quietly = TRUE) ) {
    ## Build integer program
    obj <- sizes 
    con <- matrix(2*sizes, nrow=1)
    dir <- c("<=")
    rhs <- c(sum(sizes))
    lp.results <- lpSolve::lp("max",obj,con, dir, rhs, all.bin=TRUE)
    ## Extract partition from lp object
    return(as.logical(lp.results$solution[seq_along(sizes)]))
  } else {
    warning("Empty solution : lpSolve not available.")
    return(as.logical(length(sizes)))
  }
}
