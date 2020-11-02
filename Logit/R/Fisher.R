#' @title Fisher information matrix
#'
#' @description \code{fisher} calculates the Fisher information matrix, which is 
#' a symmetric k x k matrix.
#'
#' @param X A numerical matrix of format n x k
#' @param p A numerical vector of format n x 1
#' @param ... optional arguments
#'
#' @return Fisher information matrix.
#'
#' @examples
#' set.seed(1)
#' X <- replicate(2, rnorm(100))
#' p <- replicate(100, 0.5)
#' fisher(X, p)
#'
#' @export

fisher <- function(X, p, ...) {
  a <- as.vector(p * (1 - p))
  a <- diag(a)
  # Fisher Matrix
  f <- t(X) %*% a %*% X
  # return Fisher matrix
  return(f)
}
