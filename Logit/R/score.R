#' @title Score vector
#'
#' @description \code{score} calculates the first derivative of the log likelihood function with respect to beta, given
#' some observed data \code{X} and \code{y}.
#'
#' @param X A numerical matrix of format n x k
#' @param y A numerical vector of format n x 1
#' @param beta A numerical vector of format k x 1
#' @param ... optional arguments
#'
#' @return A numerical vector containing the score.
#'
#' @examples
#' set.seed(1)
#' X <- replicate(2, rnorm(100))
#' y <- rbinom(100, 1, 0.5)
#' beta <- c(0, 0)
#' score(X, y, beta)
#'
#' @export

score <- function(X, y, beta, ...) {
  p <- logistic(X %*% beta)
  # Define residual
  r <- y - p
  # Score matrix
  m <- matrix(nrow = nrow(X), ncol = ncol(X))
  # multiply each element of X with corresponding r
  for (i in 1:nrow(X)) {
    m[i, ] <- r[i] * X[i, ]
  }
  # sum each column and return results as a vector
  s <- c()
  for (i in 1:ncol(m)) {
    s[i] <- sum(m[, i])
  }
  # return score vector
  return(s)
}
