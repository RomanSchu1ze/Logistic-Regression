#' @title Logisitc function
#'
#' @description \code{logistic} transforms a domain of real numbers from negative infity to positive infinity
#' to real numbers lying between zero and one.
#'
#' @param x A numeric vector or scalar
#' @param ... optional arguments
#' 
#' @return A numeric vector or scalar after applying logistic function on a original vector.
#'
#' @examples
#' a <- -5:5
#' logistic(a)
#'
#' @export

logistic <- function(x, ...) {
  f <- exp(x) / (1 + exp(x))
  return(f)
}
