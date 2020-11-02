#' @title Summary method for objects of class "logit"
#'
#' @description Method to get a \code{summary} list for objects of class "logit".
#' Gives information about the estimated parameters, their significance and further 
#' model characeristica.
#' 
#' @param object An object of class "logit".
#' @param ... optional arguments
#' 
#' @importFrom stats coef
#' @importFrom stats pnorm
#' 
#' @return A list of class "summary.logit". It is a list containing the 
#' following elements:
#' \itemize{
#' \item \emph{coefficients}: Estimated coefficients
#' \item \emph{Std. Err}: Standard Erros of estimated coefficients
#' \item \emph{z value}: Test statisitic of estimated coefficients
#' \item \emph{{P(>|z|)}}: P-Values for estimated coefficients 
#' \item \emph{call}: Function call
#' \item \emph{NullDeviance}: Deviance of Intercept model
#' \item \emph{NullDf}: Degrees of freedom for Null model 
#' \item \emph{NullDf}: Degrees of freedom for Null model 
#' \item \emph{mcFad}: Mc Fadden R squared
#' \item \emph{y}: binary response variable
#' \item \emph{X}: Set of regressors
#' \item \emph{Accuracy}: Accuracy of estimated model
#' \item \emph{AIC}: AIC of estimated model
#' \item \emph{iterations}: Number of Fisher scoring iterations needed to converge
#' \item \emph{residuals}: Deviance residuals of estimated model
#' \item \emph{Pearson}: Pearson residuals of estimated model
#' }
#'
#' @examples
#' mod <- logit(Survived ~ ., data = myData)
#' str(mod) # list 
#' # To access the different elements of the summary the "\code$" can used, 
#' # e.g. taking a look at the coefficients:
#' mod$coefficients
#'
#' @export

summary.logit <- function(object, ...) {
  # Assing response varible and design matrix
  y <- object$y
  X <- object$X
  # Standard Errors of coefficients
  se <- sqrt(diag(object$vcov))
  zval <- stats::coef(object) / se
  # Coefficient matrix including stats
  out <- cbind(stats::coef(object), se, zval, 2 * stats::pnorm(abs(zval), lower.tail = FALSE))
  # Colnames of coefficient matrix
  colnames(out) <- c("Estimate", "Std.Err", "z.value", " P(>|z|)")
  # save in new list
  res <- list(call = object$call,
              coefficients = out,
              residuals = object$devianceResid,
              Pearson = object$pearsonResid,
              iterations = object$iterations,
              AIC = min(object$AIC),
              Accuracy = object$acc,
              y = object$y,
              X = object$X,
              mcFad = object$mcFad,
              NullDeviance = object$NullDeviance,
              NullDf = object$NullDf,
              df = object$df,
              deviance = object$deviance)
  # define class
  class(res) <- "summary.logit"
  # return list
  return(res)
}

