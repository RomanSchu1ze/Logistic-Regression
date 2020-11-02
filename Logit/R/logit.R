#' @title Logistic Regression
#'
#' @description \code{logit} performs a logistic regression given a binary 
#' response variable and a set of regressors. 
#'
#' @param x An object of any class.
#' @param ... optional arguments
#' 
#' @importFrom stats model.frame
#' @importFrom stats model.matrix
#' @importFrom stats model.response
#'
#' @examples
#' logit(Survived ~ ., data = myData)
#'
#' @export

logit <- function(x, ...) UseMethod("logit")

#'Default method for the function logit.
#'
#' @rdname logit
#' 
#' @param y A binary vector of format n x 1
#' 
#' @return a list of class "logit" containing a set of model characteristica:
#' \itemize{
#' \item X
#' \item y
#' \item Log Likelihood for each iteration
#' \item Convergence
#' \item estimated parameter vector
#' \item Estimated probability for y=1
#' \item Deviance  
#' \item Deviance residuals 
#' \item Pearson resiudals
#' \item AIC for each iteration
#' \item Accuracy
#' \item Variance Covariance Matrix
#' \item McFadden R squared
#' \item Degrees of freedom
#' \item Degrees of freedom of Null model
#' }
#'
#' @export

logit.default <- function(x, y, ...) {
  # Convert inputs into appropiate format
  X <- as.matrix(x)
  # Make sure y is coded with zeroes and ones
  if (!(0 %in% y && 1 %in% y)) {
    y <- factor(y, labels = c(0, 1))
  }
  y <- as.numeric(as.character(y))
  # Make sure y is binary
  if (length(unique(y)) != 2) {
    stop("Response variable is expected to be binary")
  }
  # Apply gradient function
  res <- fScore(X, y)
  # Assign response variable and regressors to existing list res
  res$y <- y
  res$X <- X
  # Append list by additional elements
  ## Deviance residuals
  res$devianceResid <- dRes(y, res$prob)
  ## Deviance
  res$deviance <- sum(res$devianceResid ^ 2)
  ## Pearson residuals
  res$pearsonResid <- pRes(y, res$prob)
  # AIC
  res$AIC <- - 2 * res$logLikelihood + 2 * ncol(X)
  # Accuracy
  res$acc <- acc(y, res$prob)
  # Mc Faddens R^2
  res$mcFad <- 1 - (max(res$logLikelihood) / lNull(y))
  # Null Deviance
  res$NullDeviance <- (- 2) * lNull(y)
  # Null Model Degrees of freedom
  res$NullDf <- nrow(res$X) - 1
  # Variance-Covariance Matrix
  res$vcov <- solve(fisher(X, res$prob))
  # Degrees of freedom
  res$df <- nrow(X) - ncol(X)
  # Assign S3 class
  class(res) <- "logit"
  # Return object
  return(res)
}

#' @rdname logit
#' 
#' @param formula way of extracting formulae which have been included in other objects
#' @param data a list including all relevant variables
#' 
#' @export

logit.formula <- function(formula, data = list(), ...) {
  # model.frame and its methods return a data.frame with variables needed to use formula
  mf <- stats::model.frame(formula = formula, data = data)
  # Constructs a design matrix containing all regressors
  X <- stats::model.matrix(attr(mf, "terms"), data = mf)
  # Constructs a numeric vector with the response variable
  y <- stats::model.response(mf)
  # Using default method from above
  res <- logit.default(X, y, ...)
  # returns a call in which all of the specified arguments are specified by their full names
  res$call <- match.call()
  # Append output list by formula
  res$formula <- formula
  return(res)
}

mydata <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
mydata$rank <- factor(mydata$rank)
