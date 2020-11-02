#' @title Print method for objects of class "logit"
#'
#' @description \code{print} method for objects of class "logit"
#' 
#' @param x An object of class "logit".
#' @param ... optional arguments
#' 
#' @importFrom stats coef
#' 
#' @return A summary table for objects of class "logit", listing the 
#' function call, coefficients, degrees of freedom, Deviance, AIC and Accuracy. 
#' 
#' @examples
#' mod <- logit(Survived ~ ., data = myData)
#' print(mod)
#' 
#' @export

print.logit <- function(x, ...) {
  # line break
  cat("Call:\n")
  # print function call
  print(x$call)
  # line break 
  cat("\nCoefficients:\n")
  # coef(x)[, 1] in order to transpose vector
  print.default(format(stats::coef(x)[, 1], digits = 4L),
                print.gap = 1L, quote = FALSE, right = TRUE)
  # line break, print degrees of freedom from Null model and full model
  cat("\nDegrees of freedom:", 
      sprintf(" %1$1.0f Total (i.e. Null);  %2$1.0f Residual", x$NullDf, x$df))
  # line break, print Null Deviance
  cat("\nNull Deviance:", round(x$NullDeviance, 0))
  # line break, print Residual Deviance
  cat("\nResiudal Deviance:", round(x$deviance, 1))
  # line break, print AIC
  cat("\nAIC:", round(min(x$AIC), 1))
  # line break, print Accuracy
  cat("\nAccuracy:", round(x$acc, 1))
}