% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pcls1.R
\name{pcls1}
\alias{pcls1}
\title{pcls1}
\usage{
pcls1(X, Y)
}
\arguments{
\item{X}{a list containing the explanatory variable}

\item{Y}{a list containing the response vector}
}
\value{
a list containing the optimal parameter and the Lagrange multiplier
}
\description{
Solve the positively constrained least squares problem with 1 explanatory variable
}
\examples{
results <- pcls1(X = data.frame(runif(5)), Y = data.frame(1:5))
}
