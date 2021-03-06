% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_dens.R
\name{plot_dens}
\alias{plot_dens}
\title{Plot the projection estimator of a density}
\usage{
plot_dens(basis, data, D, pd = perfect_D(basis, data), resol = 300,
  dens = NULL, compare = FALSE)
}
\arguments{
\item{basis}{an object of class \code{\link{Basis}}}

\item{data}{a numeric vector containing a sample of i.i.d. real random variables with common density.}

\item{D}{a numeric value, the dimension of the subspace in which the estimation will be calculated.}

\item{pd}{a numeric value. Another dimension of a subspace in which the estimation can be calculated in.
The default is the dimension given by the model selection algorithm (see \code{\link{perfect_D}}).}

\item{resol}{a numeric value, specifying the quality of the plot (length.out of processed values). The default is 300.}

\item{dens}{a function, the true density of the distribution. Since the true density may not be known, the default is NULL.}

\item{compare}{logical value. If TRUE the projection estimator (see \code{\link{est_dens}}) using \code{D} is compared to (if default is used) the 
projection estimator of the dimension generated by the model selection algorithm (see \code{\link{perfect_D}}) and (if passed) the true density \code{dens}.
If FALSE (the default) only the estimated density is plotted.}
}
\value{
no return
}
\description{
Plot the estimated density function (see \code{\link{est_dens}}) for a given set of data using a given orthonormal basis
of some dimension. If known, compare the plot to the plot of the actual density function.
}
\examples{
trig_bas <- Trig_Basis$new(1000)
data <- rnorm(100, 0.5, 0.088)
plot_dens(trig_bas, data, 5)
\dontrun{
dens1 <- function(x) dnorm(x, 0.5, 0.088)
plot_dens(trig_bas, data, 5, dens = dens1, compare = TRUE)
}
}
