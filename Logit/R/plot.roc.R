#' @title Plot ROC curve  for objects of class "roc"
#'
#' @description \code{plot} method for objects of class "roc".
#' 
#' @param x A list of class "roc".
#' @param ... optional arguments
#' 
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 geom_abline
#' 
#' @return a plot containing a roc curve
#' 
#' @examples
#' mod <- logit(Survived ~ ., data = myData)
#' obj <- ggRoc(mod$y, mod$prob)
#' plot(obj)
#'
#' @export

plot.roc <- function(x, ...) {
# Plot ROC using ggplot2
ggplot2::ggplot(data = x[[1]], ggplot2::aes(x = x[[1]]$falsePositive, y = x[[1]]$truePositive)) +
  ggplot2::geom_line(colour = "steelblue") +
  ggplot2::geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dotted") +
  ggplot2::ggtitle("ROC curve") +
  ggplot2::theme(legend.justification = "top", plot.title = ggplot2::element_text(hjust = 0.5)) + 
  ggplot2::labs(x = "False Positive Rate") + 
  ggplot2::labs(y = "True Positive Rate") 
}
