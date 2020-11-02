#' @title Accuracy
#'
#' @description \code{acc} calculates the accuracy, given a binary vector \code{y} and the corresponding
#'  probability \code{p}. Observations with probability values smaller or equal to 0.5 are classified
#' as zero, otherwise as 1.
#'
#' @param y A numerical vector of format n x 1
#' @param p A numerical vector of format n x 1
#' @param ... optional arguments 
#'
#' @return A numerical scalar telling the fraction of correctly specified predicitions.
#'
#' @examples
#' set.seed(1)
#' y <- rbinom(100, 1, 0.5)
#' p <- runif(100, 0, 1)
#'
#' @export

acc <- function(y, p, ...) {
  yhat <- ifelse(p <= 0.5, 0, 1)
  cor <- sum(yhat == y)
  return(cor / length(y))
}
