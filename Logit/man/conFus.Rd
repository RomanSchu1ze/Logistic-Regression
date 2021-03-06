% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/conFus.R
\name{conFus}
\alias{conFus}
\title{Confusion Matrix}
\usage{
conFus(y, p, t = 0.5, ...)
}
\arguments{
\item{y}{A numerical vector of format n x 1}

\item{p}{A numerical vector of format n x 1}

\item{t}{A numerical scalar taking a value between 0 and 1}

\item{...}{optional arguments}
}
\value{
A numerical vector containing 4 entries: 
\itemize{
\item true positive (tp)
\item false negative (fn) 
\item false positive (fp) 
\item true negative (tn)
}
}
\description{
\code{conFus} calculates the confusion matrix given binary input \code{y} and
the corresponding probability value \code{p}. Additionally the parameter \code{t} is
the threshold value and can be chosen manually.
Probability values larger than \code{t} will be assigned to one.
Consequently values equal or smaller than \code{t} will take the value zero.
}
\examples{
set.seed(1)
y <- rbinom(100, 1, 0.5)
p <- runif(100, 0, 1)
conFus(y, p)

}
