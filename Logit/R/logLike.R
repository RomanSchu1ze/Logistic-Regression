#' @title Log likelihood function
#'
#' @description \code{logLike} calculates the log of the likelihood function.
#' The likelihood whether a specific event occurs (\code{y}=1) is given by a probabilty value which
#' follows from the logistic function given some observed data \code{X}.
#'
#' @param X A numerical matrix of format n x k
#' @param y A binary numerical vector of format n x 1
#' @param beta A numerical vector of format k x 1
#' @param ... optional arguments
#'
#' @return A scalar containing the log likelihood value.
#'
#' @examples
#' set.seed(1)
#' X <- replicate(2, rnorm(100))
#' y <- rbinom(100, 1, 0.5)
#' beta <- c(0, 0)
#' logLike(X, y, beta)
#'
#' @export

logLike <- function(X, y, beta, ...) {
  # predictor
  a <- X %*% beta
  #loglikelihood
  l <-  sum(y * log(logistic(a))  + (1 - y) * log(1 - logistic(a)))
  # return loglikelihood
  return(l)
}
