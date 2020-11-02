#' @title Fisher Scoring algorithm
#'
#' @description \code{fScore} is a nonlinear optimization technique to determine the best estimator for a
#' set of unknown parameters. In case of logistic regression the parameter of interest is beta. 
#' Gradient descent is an iterative search process, where
#' one starts at a random point on the log likelihood function.  In this case the starting value(s) are chosen to be zero.
#' Further necessary inputs are a set of predictors \code{X} and a binary response \code{y}.
#' The user of this function can also choose the maximum number of iterations the algorithm
#' is searching for an optimum of beta. Additionaly the learning rate and the accuracy can be
#' manually determined.
#'
#' @param X A numerical matrix of format n x k
#' @param y A numerical vector of format n x 1
#' @param maxIter A scalar defining the maximum number of iterations
#' @param rate Learning rate
#' @param accuracy  tells when to stop the algorithm
#' @param ... optional arguments
#'
#' @return a list containing: 
#' \itemize{
#' \item vector of estimated coefficients
#' \item probability for each observation
#' \item number of fisher scoring iterations
#' \item convergence 
#' }
#' 
#' @examples
#' set.seed(1)
#' X <- replicate(2, rnorm(100))
#' # Add a vector of ones as first column to estimate Intercept
#' X <- cbind(1, X)
#' z <- 2 * X[, 2] + 3 * X[, 3]
#' # Use function logsitic and check help pages for further information
#' y <- rbinom(100, 1, logistic(z))
#' fScore(X, y)
#'
#' @export

fScore <- function(X, y, maxIter = 100, rate = 1, accuracy = 0.01, ...) {
  # Initial beta vector
  beta <- rep(0, ncol(X))
  # Starting value for change in beta
  delta <- 1
  #iteration counter
  iter <- 0
  # convergence
  convergence <- FALSE
  # probability
  p <- logistic(X %*% beta)
  # empty vecotr to store log-Likelihood
  l <- c()
  # Optimization using while loop with two conditions
  while (convergence == FALSE && iter <= maxIter) {
    # Update iteration
    iter <- iter + 1
    # Calculate log - Likelihood for each combination
    l[iter] <- logLike(X, y, beta)
    # update beta
    betaNew <- beta + rate * solve(fisher(X, p)) %*% score(X, y, beta)
    pnew <- logistic(X %*% betaNew)
    # Change in beta
    delta <- abs(betaNew - beta)
    #print(paste("The local maximum occurs at", beta))
    #print(paste("current number of iterations is", iter))
    beta <- betaNew
    p <- pnew
    # check convergence condition
    if (sum(delta) < accuracy) {
      convergence <- TRUE
      break
    }
  }
  # if no convergence...
  if (convergence == FALSE) {
    stop(paste(iter, "iterations reached without convergence. Increase maxIter?"))
  }
  # return list with results
  return(list(coefficients = beta,
              prob = as.vector(p),
              iterations = iter,
              logLikelihood = l,
              convergence = convergence))
}
