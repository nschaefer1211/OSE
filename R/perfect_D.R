#' @title Adaptive estimator for the dimension 
#'
#' @description Given an orthonormal basis and a data sample, this function calculates a relevant dimension D, which represents the
#' dimension we want to calculate our projection estimator in (see \code{\link{est_dens}} for more details).
#' @importFrom stats integrate
#' @param basis an object of class \code{\link{Basis}}
#' @param data a numeric vector containing a sample of i.i.d. real random variables with common density
#' @param kappa a numeric constant which does not depend on the data. A value greater than 1 a suitable, the default is 2
#' @export
#' @return a numeric value, the dimension of a suitable subspace
#' @examples
#' \dontrun{
#' trig_bas <- Trig_Basis$new(1000)
#' data <- rnorm(100, 0.5, 0.088)
#' perfect_D(trig_bas, data)
#' }
perfect_D <- function(basis, data, kappa = 2) { 
  stopifnot(length(data) <= basis$dimension)
  stopifnot(is.numeric(data))
  stopifnot(any(class(basis) %in% "Basis"))
  n <- length(data)
  vec <- c()
  for (D in 1:n) {
    temp <- est_dens(basis, data, D)(data)
    norm <- midpoint_composite(est_dens_squared(basis, data, D), 0, 1, n = ceiling(1.02*D))
    y <- 2 / n * sum(temp)
    pen <- kappa * D / n
    vec <- c(vec, norm - y + pen)
  }
  return(which.min(vec))
}