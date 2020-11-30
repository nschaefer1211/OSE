#' @title Simulation of data 
#'
#' @description Creates a data frame consisting of pairs (x, y) where the first component is distributed
#' according to a given probability density and y satisfies y = m(x) + e where e is a normally distributed error term. 
#' @import stats
#' @param dens a probability density on \eqn{[0,1]}
#' @param m a regression function
#' @param n a numeric value, specifying the number of pairs of values to be generated
#' @export
#' @return an \code{n} x 2-data.frame 
#' @examples
#' #define a density on [0,1]
#' dens1 <- function(x) 1 + 0.5 * sin(2 * pi * x)
#' #using the log-function as regression function
#' datasim1 <- datsim(dens1, log, 50)
#' datasim1
datsim <- function(dens, m, n) {
  stopifnot(class(dens) == "function")
  stopifnot(class(m) == "function")
  stopifnot(class(n) == "numeric")
  stopifnot(length(n) == 1)
  stopifnot(n >= 0)
  e <- rnorm(n, 0, 0.01)
  x <- rdens(dens, n)
  y <- m(x) + e
  return(data.frame(x, y))
}
