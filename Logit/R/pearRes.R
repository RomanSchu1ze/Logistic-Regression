#' @title Pearson residuals
#'
#' @description \code{pRes} calculates the pearson residuals given binary input \code{y} and
#' the corresponding probability value \code{p}.
#'
#' @param y A numerical vector of format n x 1
#' @param p A numerical vector of format n x 1
#' @param ... optional arguments
#'
#' @return A numerical vector containing the pearson residuals.
#'
#' @examples
#' set.seed(1)
#' y <- rbinom(100, 1, 0.5)
#' p <- runif(100, 0, 1)
#'
#' @export

pRes <- function(y, p, ...) {
  res <- c()
  res <- (y - p) / (sqrt (p * (1 - p)))
  return(res)
}
