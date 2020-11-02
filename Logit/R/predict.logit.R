#' @title Predict method for objects of class "logit"
#'
#' @description \code{predict} method for objects of class "logit".
#' 
#' @param object An object of class "logit"
#' @param t A numerical scalar taking a value between 0 and 1
#' @param ... optional arguments
#' 
#' @details Observations with predicted probability smaller or equal to \code{t} will be
#' assigned to category zero. In reverse observations with predicted probability \code{p} larger 
#' than \code{t} will be be assigned to category one.
#' 
#' @return A matrix having three columns : 
#' \itemize{
#' \item Linear Predictor
#' \item Probability
#' \item Predicted class (zero or one)
#' }
#'   
#' @examples
#' mod <- logit(Survived ~ ., data = myData)
#' predict(mod)
#' 
#' @export

predict.logit <- function(object, t = 0.5, ...) {
  # linear predictor 
  pred <- object$X %*% object$coefficients
  # predicted probabilities
  prob <- logistic(pred)
  # predicted class
  class <- ifelse(prob > t, 1, 0)
  # store results in a matrix
  m <- cbind(pred, prob, class)
  # assign column names
  colnames(m) <- c("linear predictor", "probability", "predicted class")
  # return matrix
  return(m)
}

