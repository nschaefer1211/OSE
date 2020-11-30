#This is an internal helper function used below
#It calculates the estimations of the coefficients
#' @title One dimensional projection estimator coefficients
#' 
#' @description Given a sample of i.i.d. real rand. variables with a common density f and a basis of class \code{\link{Basis}}. 
#' This function calculates the coefficients of the projection estimator of the density f.
#' @param basis an object of class \code{\link{Basis}}
#' @param data a sample of i.i.d. real random variables with common density
#' @param j a natural number between 1 and the dimension of \code{basis}
#' @keywords internal
#' @return the jth estimated coefficient of the estimated density based on the \code{basis} and \code{data}
est_proj_coef <- function(basis, data, j) {
  stopifnot(any(class(basis) %in% "Basis"))
  stopifnot(is.numeric(data))
  stopifnot(is.numeric(j))
  stopifnot(length(j) == 1)
  stopifnot(j > 0)
  stopifnot(j <= basis$dimension)
  phi_j <- basis$get_function(j)(data)
  return(mean(phi_j))
}
