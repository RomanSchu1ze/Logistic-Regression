#' @title Receiver Operating Characteristic (ROC)
#'
#' @description \code{ggRoc} calculates the confusion matrices for different threshold values in the first
#' place. In the second place the true positive and false positive rate will be calculated
#' for each threshold value.
#'
#' @param y A numerical vector of format n x 1
#' @param p A numerical vector of format n x 1
#' @param ... optional arguments
#'
#' @return A dataframe containing true postivie and false positive values for each
#' threshold value.
#'
#' @examples
#' set.seed(1)
#' y <- rbinom(100, 1, 0.5)
#' p <- runif(100, 0, 1)
#' ggRoc(y, p)
#'
#' @export

ggRoc <- function(y, p, ...) {
  # sequence from 0.01 to 1.00 with 0.01 steps
  t <- seq(0, 1, 0.01)
  # Create an empty list to store confusion matrices for each threshold value
  res <- list()
  length(res) <- length(t)
  # For every threshold value construct confusion matrix and save in list
  for (i in 1:length(t)) {
    res[[i]] <- conFus(y, p, t = t[i])
  }

  # Calculate truePositive and falsePositives from list for each threshold value
  truePositive <- c()
  falsePositive <- c()
  for(i in 1:length(res)) {
    truePositive[i] <- res[[i]][1] /  (res[[i]][2] + res[[i]][1])
    falsePositive[i] <- res[[i]][3] / (res[[i]][4] + res[[i]][3])
  }
  # Save results in a dataframe
  list <- list()
  list[[1]] <- as.data.frame(cbind(truePositive, falsePositive, t))
  # return dataframe
  # define class for df
  class(list) <- "roc"
  return(list)
}

