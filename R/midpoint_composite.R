#This is an internal helper function used below
#It calculates the estimations of the coefficients
#' @title Midpoint Composite rule
#' 
#' @description This function implements the midpoint composite rule for numerical integration
#' @param f a function
#' @param a the lower bound 
#' @param b the upper bound
#' @param n the amount of intervals we divide the interval [\code{a}, \code{b}] into
#' @keywords internal
#' @return an estimation of the integral of f over [\code{a}, \code{b}]
midpoint_composite <- function(f, a, b, n) {
  points <- seq(a, b, length = n + 1)
  h <- (b - a)/n
  
  area <- 0
  for (i in seq_len(n)) {
    area <- area + h * f((points[i] + points[i + 1])/2)
  }
  area
}


