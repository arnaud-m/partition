#' Pretty print and plot (optional) the partition
#'
#' @inheritParams PartEval
#' @param plot barplot of the partition
#'
#' @export
#' @examples
#' sizes <- c(13, 11, 8, 20, 20, 11, 16, 17, 4, 13)
#' part <-  c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE)
#' PartPrint(sizes, part)
PartPrint <- function(sizes, part, plot = FALSE) {
  stopifnot(
    is.numeric(sizes),
    is.logical(part),
    length(part) == length(sizes)
    )
  ## Console pretty print of the partition
  cat(
    sprintf(
      "S_1: %4d = %s\nS_2: %4d = %s\nDiff: %3d\n",
      sum(sizes[part]),
      paste(sizes[part], collapse = " + "),
      sum(sizes[!part]),
      paste(sizes[!part], collapse = " + "),
      PartEval(sizes, part)
      )
    )
  ## Bar plot of the partition
  if(plot) {
    ## Transform partition into a matrix 
    n1 <- sum(part)
    n2 <- sum(!part)
    mat <- matrix(0, nrow=max(n1,n2), ncol=2)
    colnames(mat) <- c("S1", "S2")
    mat[1:n1,1] <- sort(sizes[part], decreasing=TRUE)
    mat[-(1:n1),1] <- mat[n1,1]
    mat[1:n2,2] <- sort(sizes[!part], decreasing=TRUE)
    mat[-(1:n2),2] <- mat[n2,1]
    barplot(mat, main="Integers Partition", horiz=TRUE)
  }
}
