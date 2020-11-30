#This is an internal helper function used below
#It calculates the estimations of the coefficients
#' @title two dimensional projection estimator coefficients
#' 
#' @description Given a sample of i.i.d. real random bivariate vectors with a common density f and a basis of class \code{\link{Basis}}. 
#' This function calculates the coefficients of the two dimensional projection estimator of the density f.
#' @param basis an object of class \code{\link{Basis}}
#' @param data1 a sample of i.i.d. real random variables 
#' @param data2 a sample of i.i.d. real random variables with the same length as \code{data1}
#' @param j a natural number between 1 and the dimension of \code{basis}
#' @param k a natural number between 1 and the dimension of \code{basis}
#' @keywords internal
#' @return the (\code{j}, \code{k})-th estimated coefficient of the estimated density based on \code{basis}, \code{data1} and \code{data2}
est_proj2_coef <- function(basis, data1, data2, j, k) {
  phi_j_k <- basis$get_function(j)(data1) * basis$get_function(k)(data2)
  return(mean(phi_j_k))
}