#' @title Deviance residuals
#'
#' @description \code{dRes} calculates the deviance residuals given binary input \code{y} and
#' the corresponding probability value \code{p}.
#'
#' @param y A numerical vector of format n x 1
#' @param p A numerical vector of format n x 1
#' @param ... optional arguments
#'
#' @return A numerical vector containing the deviance residuals.
#'
#' @examples
#' set.seed(1)
#' y <- rbinom(100, 1, 0.5)
#' p <- runif(100, 0, 1)
#' dRes(y, p)
#'
#' @export
dRes <- function(y, p, ...) {
  d <- c()
  d <- ifelse(y == 1, sqrt(- 2 * log(p)), (- sqrt(- 2 * log(1 - p))))
  return(d)
}