#' @title Confusion Matrix
#'
#' @description \code{conFus} calculates the confusion matrix given binary input \code{y} and
#' the corresponding probability value \code{p}. Additionally the parameter \code{t} is
#' the threshold value and can be chosen manually.
#' Probability values larger than \code{t} will be assigned to one.
#' Consequently values equal or smaller than \code{t} will take the value zero.
#'
#' @param y A numerical vector of format n x 1
#' @param p A numerical vector of format n x 1
#' @param t A numerical scalar taking a value between 0 and 1
#' @param ... optional arguments
#' 
#' @return A numerical vector containing 4 entries: 
#' \itemize{
#' \item true positive (tp)
#' \item false negative (fn) 
#' \item false positive (fp) 
#' \item true negative (tn)
#'}
#'
#' @examples
#' set.seed(1)
#' y <- rbinom(100, 1, 0.5)
#' p <- runif(100, 0, 1)
#' conFus(y, p)
#'
#' @export

conFus <- function(y, p, t = 0.5, ...) {
  # Construct list for each scenario (TRUE positive = tp, FALSE negative = fn...)
  res <- list(NULL, NULL, NULL, NULL)
  names(res) <- c("tp" , "fn", "fp", "tn")
  # construct a matrix and loop over rows to assign observation i to corresponsding
  # category
  m <- cbind(y, p)
  for(i in 1:nrow(m)) {
    if (m[i, 1] == 1 & m[i, 2] > t) {
      res$tp[[i]] <- 1
    } else if (m[i, 1] == 1 & m[i, 2] <= t) {
      res$fn[[i]] <- 1
    } else if (m[i, 1] == 0 & m[i, 2] > t) {
      res$fp[[i]] <- 1
    } else {
      res$tn[[i]] <- 1
    }
  }
  # Sum up assignments for each group & divide by length of y to get percentage 
  res <- sapply(res, sum, na.rm = TRUE) / length(y)
  #return results
  return(res)
}



