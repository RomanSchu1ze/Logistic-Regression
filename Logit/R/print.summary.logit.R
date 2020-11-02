#' @title Print method for objects of class "summary.logit"
#'
#' @description Method to determine a \code{summary} table for objects of class "\code{logit}".
#' Gives information about the estimated parameters, their significance and further 
#' model characeristica.
#' 
#' @param x An object of class "summary.logit".
#' @param ... optional arguments
#' 
#' @importFrom stats printCoefmat
#' 
#' @return A summary table for objects of class "summary.logit".
#' 
#' @examples
#' mod <- logit(Survived ~ ., data = myData)
#' summary(mod)
#' @export

print.summary.logit <- function(x, ...) {
  # line break
  cat("Call:\n")
  # print function call
  print(x$call)
  # line break
  cat("\nDeviance Residuals:\n")
  # summary statistic of Deviance resiudals
  res <- round(summary(x$residuals)[-4], digits = 4L)
  names(res) <- c("Min", "1Q", "Median", "3Q", "Max")
  print(res)
  # line break
  cat("\nCoefficients:\n")
  # print coefficients and corresponding standard error, z-value, p value
  stats::printCoefmat(x$coefficients, P.value =  TRUE, has.Pvalue = TRUE, digits = 4L)
  # line break, print Deviance and degrees of freedom for Null model and estimated model
  cat("\nNull deviance:", round(x$NullDeviance, 2), "on", x$NullDf, "degrees of freedom")
  # line break
  cat("\nResidual deviance:", round(x$deviance, 2),  "on", x$df,  "degrees of freedom\n")
  # line break, print number of fisher scoring iterations
  cat("\nNumber of Fisher of Scoring Iterations:", x$iterations)
  # line break, AIC
  cat("\nAIC:", round(x$AIC, 2))
  # line break, Accuracy
  cat("\nAccuracy:", round(x$Accuracy, 2))
  # line break, McFadden R sqaured
  cat("\nMcFadden R-squared:", round(x$mcFad, 2))
}