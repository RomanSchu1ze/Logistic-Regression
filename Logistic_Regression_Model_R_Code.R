######################## Logistic Regression Implementation in R using S3 class system ###################################


# PART I ----------------------------------------------------------------------
#  Optimization of beta ----

## 1. logistic function ----
logistic <- function(x, ...) {
  f <- exp(x) / (1 + exp(x))
  return(f)
}

## 2. log-Likelihood function ----
logLike <- function(X, y, beta, ...) {
  # predictor
  a <- X %*% beta
  #logLikelihood
  l <-  sum ( y * log(logistic(a))  + (1 - y) * log(1 - logistic(a)))
  # return logLikelihood
  return(l)
}

## 3. Score vector ----
score <- function(X, y, beta, ...) {
  p <- logistic(X %*% beta)
  # Define residual
  r <- y - p
  # Score matrix
  m <- matrix(nrow = nrow(X), ncol = ncol(X))
  # multiply each element of X with corresponding r
  for (i in 1:nrow(X)) {
    m[i, ] <- r[i] * X[i, ]
  }
  # sum each column and return results as a vector
  s <- c()
  for (i in 1:ncol(m)) {
    s[i] <- sum(m[, i])
  }
  # return score vector
  return(s)
}

## 4. Fisher information Matrix ----
fisher <- function(X, p, ...) {
  a <- as.vector(p * (1 - p))
  a <- diag(a)
  # Fisher Matrix
  f <- t(X) %*% a %*% X
  # return Fisher matrix
  return(f)
}

## 5. Implement gradient descent ----
grad <- function(X, y, maxIter = 100, ...) {
  # Initial beta vector
  beta <- rep(0, ncol(X))
  # Learning rate
  rate <- 1
  #This tells us when to stop the algorithm
  accuracy <- 0.01
  # Change in beta
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

# Further functions to derive additional model information ----

## 6. Pearson residuals ----
pRes <- function(y, p, ...) {
  res <- c()
  res <- (y - p) / (sqrt (p * (1 - p)))
  return(res)
}

## 7. Deviance residuals ----
dRes <- function(y, p, ...) {
  d <- c()
  d <- ifelse(y == 1, sqrt(- 2 * log(p)), (- sqrt(- 2 * log(1 - p))))
  return(d)
}

## 8. Confusion matrix ----
# Inputs: probability, y, and threshold
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
  # Sum up assignments for each group
  res <- sapply(res, sum, na.rm = TRUE)
  #return results
  return(res)
}

## 9. ROC curve ----
roc <- function(y, p, ...) {
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
  df <- as.data.frame(cbind(truePositive, falsePositive, t))
  # return dataframe
  return(df)
}

## 10. Accuracy ----
acc <- function(y, p, ...) {
  yhat <- ifelse(p <= 0.5, 0, 1)
  cor <- sum(yhat == y)
  return(cor / length(y))
}

## 11. log Likelihood for Null model ---- log odds(Josh Stamer - mcFadden R2)
lNull <- function(y, ...) {
  # probabily of Intercept model is just success / all observations
  pNull <- sum((y)) / length(y)
  l <-  sum ( y * log(pNull)  + (1 - y) * log(1 - pNull))
  # return lNull
  return(l)
}

# PART II ---------------------------------------------------------------------
# Implement logit model using S3 class System ----

## 1. Default method
logit <- function(x, ...) UseMethod("logit")

logit.default <- function(X, y, ...) {
  # Convert inputs into appropiate format
  X <- as.matrix(X)
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
  res <- grad(X, y, ...)
  # Assign response variable
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
  res$mcFad <- 1 - max(res$logLikelihood) / lNull(y)
  # Null Deviance 
  res$NullDeviance <- (- 2) * lNull(y)
  # Null Model Degrees of freedom 
  res$NullDf <- nrow(res$X) - 1
  # Variance-Covariance Matrix
  res$vcov <- solve(fisher(X, res$prob))
  # Degrees of freedom
  res$df <- nrow(X) - ncol(X)
  # Assign class to object
  class(res) <- "logit"
  # Return object
  return(res)
}

## 2. Print logit function ----
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

## 3. summary method ----
summary.logit <- function(object, ...) {
    # Assign response varible and design matrix
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

## 3.1 print summary method ----
print.summary.logit <- function(object, ...) {
    # line break
    cat("Call:\n")
    # print function call
    print(object$call)
    # line break
    cat("\nDeviance Residuals:\n")
    # summary statistic of Deviance resiudals
    res <- round(summary(object$residuals)[-4], digits = 4L)
    names(res) <- c("Min", "1Q", "Median", "3Q", "Max")
    print(res)
    # line break
    cat("\nCoefficients:\n")
    # print coefficients and corresponding standard error, z-value, p value
    stats::printCoefmat(object$coefficients, P.value =  TRUE, has.Pvalue = TRUE, digits = 4L)
    # line break, print Deviance and degrees of freedom for Null model and estimated model
    cat("\nNull deviance:", round(object$NullDeviance, 2), "on", object$NullDf, "degrees of freedom")
    # line break
    cat("\nResidual deviance:", round(object$deviance, 2),  "on", object$df,  "degrees of freedom\n")
    # line break, print number of fisher scoring iterations
    cat("\nNumber of Fisher of Scoring Iterations:", object$iterations)
    # line break, AIC
    cat("\nAIC:", round(object$AIC, 2))
    # line break, Accuracy
    cat("\nAccuracy:", round(object$Accuracy, 2))
    # line break, McFadden R sqaured
    cat("\nMcFadden R-squared:", round(object$mcFad, 2))
}

## 4. formula ----
logit.formula <- function(formula, data = list(), ...) {
  mf <- model.frame(formula = formula, data = data)
  X <- model.matrix(attr(mf, "terms"), data = mf)
  y <- model.response(mf)
  
  res <- logit.default(X, y, ...)
  res$call <- match.call()
  res$formula <- formula
  return(res)
}

## 5. Predict method ----
predict.logit <- function(object, t = 0.5, ...) {
  # linear predictor 
  pred <- object$X %*% object$coefficients
  # predicted probabilities
  prob <- logistic(pred)
  # predicted category
  category <- ifelse(prob > t, 1, 0)
  # store results in a matrix
  m <- cbind(pred, prob, category)
  # assign column names
  colnames(m) <- c("linear predictor", "probability", "predicted category")
  # return matrix
  return(m)
}

## 6. Plot method ----
plot.logit <- function(x, ...) {
  
  # Plot 1: Plot linear predictions against residuals
  # store predictions in a dataframe 
  df1 <- data.frame(LinearPredictor = x$X %*% x$coefficients)
  # Plot linear predictor against deviance residuals 
  df1$residuals <- x$devianceResid
  # Plot graph 
  a <- ggplot2::ggplot(data = df1, ggplot2::aes(x = df1$LinearPredictor, y = df1$residuals)) +
    ggplot2::geom_point(ggplot2::aes(colour = df1$residuals), alpha = 1/2) +
    ggplot2::geom_smooth(method = "loess") +
    ggplot2::ggtitle("Predicted values against residuals") +
    ggplot2::theme(legend.position = "none",
                   plot.title = ggplot2::element_text(hjust = 0.5)) + 
    ggplot2::labs(x = "Linear Predictor") + 
    ggplot2::labs(y = "Residuals") 
  
  # Plot 2 : Normal QQ Plot
  # store residuals in a dataframe and add index
  df2 <- as.data.frame(x$devianceResid)
  df2$index <- 1:length(x$devianceResid)
  names(df2) <- c("Residuals", "Index")
  # Plot residuals
  b <- ggplot2::ggplot(data = df2, ggplot2::aes(sample = df2$Residuals)) +
    ggplot2::stat_qq() +
    ggplot2::stat_qq_line(colour = "red") +
    ggplot2::ggtitle("Normal QQ Plot") +
    ggplot2::theme(legend.position = "none",
                   plot.title = ggplot2::element_text(hjust = 0.5))
  
  # Plot 3: Confusion Matrix
  ## Store Confusion Matrix in a dataframe
  Actual <- factor(c(1, 1, 0, 0))
  Predicted <- factor(c(1, 0, 1, 0))
  y <- conFus(x$y, x$prob)
  df3 <- data.frame(Actual, Predicted, y)
  # Plot Confusion Matrix using ggplot2
  d <- ggplot2::ggplot(data =  df3, ggplot2::aes(x = Actual, y = Predicted)) +
    ggplot2::geom_tile(ggplot2::aes(fill = df3$y), colour = "white") +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%1.0f", df3$y)), vjust = 1) +    
    ggplot2::scale_fill_gradient2(low = "red", mid = "white", high = "#badb33") +
    ggplot2::ggtitle("Confusion Matrix") +
    ggplot2::theme(legend.position = "none",
                   plot.title = ggplot2::element_text(hjust = 0.5)) 
  
  # Plot 4 : ROC
  # Store Roc curve in a dataframe
  df4 <- myLogit::roc(x$prob, x$y)
  # Plot ROC using ggplot2
  e <- ggplot2::ggplot(data =  df4, ggplot2::aes(x = falsePositive, y = truePositive)) +
    ggplot2::geom_point() +
    ggplot2::geom_line(color = "black") +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "blue", linetype = "dotted") +
    ggplot2::ggtitle("Receiver Operating Characteristic (ROC)") +
    ggplot2::theme(legend.position = "none",
                   plot.title = ggplot2::element_text(hjust = 0.5)) + 
    ggplot2::labs(x = "False Positive Rate") + 
    ggplot2::labs(y = "True Positive Rate") 
  
  # Save plot objects in a list and print eacmyLh individually 
  plots <- list(a, b, d, e)
  for(i in 1:length(plots)) {
    print(plots[[i]])
  }
}



# PART III --------------------------------------------------------------------
# Check functionality ----



# Estimate model --------------------------------------------

# Credit dataset
credit <- ISLR::Default
x <- logit(default ~ ., data = credit, maxIter = 10)
a <- glm(default ~ ., data = credit, family = binomial)





