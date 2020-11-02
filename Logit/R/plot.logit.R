#' @title Plot method for objects of class "logit"
#'
#' @description \code{plot} method for objects of class "logit".
#' 
#' @param x An object of class "logit".
#' @param ... optional arguments
#' 
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 stat_qq
#' @importFrom ggplot2 stat_qq_line
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 geom_abline
#' @importFrom ggplot2 geom_smooth
#' @importFrom ggplot2 scale_fill_gradient2
#' @importFrom ggplot2 geom_tile
#' 
#' @return Three plots for objects of class "logit" will be generated:
#' \enumerate{
#' \item Linear predictons against deviance residuals 
#' \item A Normal QQ Plot
#' \item A Confusion Matrix
#' } 
#' 
#' @examples
#' mod <- logit(Survived ~ ., data = myData)
#' plot(mod)
#' 
#' @export

plot.logit <- function(x, ...) {
  
  # Plot 1: Plot linear predictions against residuals
  # store predictions in a dataframe 
  df1 <- data.frame(LinearPredictor = x$X %*% x$coefficients)
  # Add deviance residuals to dataframe
  df1$Residuals <- x$devianceResid
  # Plot graph 
   a <- ggplot2::ggplot(data = df1, ggplot2::aes(x = df1$LinearPredictor, y = df1$Residuals)) +
    ggplot2::geom_point(ggplot2::aes(colour = df1$Residuals), alpha = 1/2) +
    ggplot2::geom_smooth(method = "loess") +
    ggplot2::ggtitle("Predicted values against residuals") +
    ggplot2::theme(legend.position = "none", plot.title = ggplot2::element_text(hjust = 0.5)) + 
    ggplot2::labs(x = "Linear Predictor") + 
    ggplot2::labs(y = "Residuals")
  
  # Plot 2: Normal QQ Plot
  # store residuals in a dataframe and add index
  df2 <- as.data.frame(x$devianceResid)
  df2$index <- 1:length(x$devianceResid)
  names(df2) <- c("Residuals", "Index")
  # Plot residuals
  b <- ggplot2::ggplot(data = df2, ggplot2::aes(sample = df2$Residuals)) +
    ggplot2::stat_qq() +
    ggplot2::stat_qq_line(colour = "blue") +
    ggplot2::ggtitle("Normal Q-Q Plot") +
    ggplot2::theme(legend.position = "none", plot.title = ggplot2::element_text(hjust = 0.5)) + 
    ggplot2::labs(x = "Theoretical Quantiles") + 
    ggplot2::labs(y = "Sample Quantiles") 
  
  # Plot 3: Confusion Matrix
  ## Store Confusion Matrix in a dataframe
  Actual <- factor(c(1, 1, 0, 0))
  Predicted <- factor(c(1, 0, 1, 0))
  res <- conFus(x$y, x$prob)
  df3 <- data.frame(Actual, Predicted, res)
  # Plot Confusion Matrix using ggplot2
  d <- ggplot2::ggplot(data =  df3, ggplot2::aes(x = Actual, y = Predicted, fill = res)) +
    ggplot2::geom_tile(color = "black") +
    ggplot2::geom_text(ggplot2::aes(label = paste0(sprintf("%.2f", res), "%")), size = 4) +    
    ggplot2::scale_fill_gradient2(low = "red", mid = "white",high = "steelblue") +
    ggplot2::ggtitle("Confusion Matrix") +
    ggplot2::theme(legend.position = "none", plot.title = ggplot2::element_text(hjust = 0.5)) 
  
  # Save plot objects in a list and print each individually 
  plots <- list(a, b, d)
  for(i in 1:length(plots)) {
    print(plots[[i]])
  }
}




