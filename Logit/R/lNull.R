#' @title Log likelihood of intercept model (Null model)
#'
#' @description \code{lNull} calculates the log of the likelihood for a logistic regression model containing
#' only the intercept.
#'
#' @param y A numerical vector of format n x 1
#' @param ... optional arguments
#'
#' @return A numerical scalar containing the log likelihood of the Null model.
#'
#' @examples
#' set.seed(1)
#' y <- rbinom(100, 1, 0.5)
#'
#' @export

lNull <- function(y, ...) {
  # probabily of Intercept model is success / all observations
  pNull <- sum((y)) / length(y)
  l <-  sum ( y * log(pNull)  + (1 - y) * log(1 - pNull))
  # return likelihood of null model
  return(l)
}
