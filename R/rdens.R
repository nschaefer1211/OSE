#' @title Random density deviates
#'
#' @description This function generates random deviates of a given probability density analogously to \code{runif} and \code{rnorm} 
#' in case of the uniform and normal distribution respectively. 
#' @import stats
#' @param dens a probability density on the intervall \eqn{[0,1]}
#' @param n a natural number, which specifies the number of deviates to be generated
#' @export
#' @return a numeric vector of length \code{n} containing random deviates of \code{dens}
#' @examples
#' #defining a density on [0,1]
#' dens1 <- function(x) 1 + 0.5 * sin(2 * pi * x)
#' #generated 50 deviates
#' rdens(dens1, 50)
rdens <- function(dens, n) {
  stopifnot(class(dens) == "function")
  stopifnot(is.numeric(n)) 
  stopifnot(length(n) == 1)
  stopifnot(n >= 0)
  stopifnot(all.equal(integrate(dens, 0, 1)$value, 1) == TRUE)
  erf <- function(x) {
    if (x <= 0) {
      return(0)
    }
    else if (x >= 1) {
      return(1)
    }
    else {
      return(integrate(dens, 0, x)$value)
    }
  }
  erf <- Vectorize(erf)
  erf_inv <- function(x) {
    stopifnot(x >= 0 & x <= 1)
    uniroot(function(y) erf(y) - x, c(0, 1))$root
  }
  erf_inv <- Vectorize(erf_inv)
  dat <- runif(n)
  return(erf_inv(dat))
}