% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/loss_functions.R
\name{compute_identity}
\alias{compute_identity}
\title{Compute Identity}
\usage{
compute_identity(prediction, actual)
}
\arguments{
\item{prediction}{Numeric vector same length as actual}

\item{actual}{Numeric vector same length as prediction}
}
\value{
Numeric vector length one
}
\description{
Best is -1, worst is 0. Objective is to minimize.
}
\examples{
compute_identity(1:10, 1:10)
compute_identity(1:10, c(1:9, 1))
compute_identity(c("hey", "hello"), c("hey", "he"))
compute_identity(runif(10), sample(c(1,0), 10, replace=TRUE))

}

