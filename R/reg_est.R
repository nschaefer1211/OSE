#' @title Adaptive estimator for a regression problem
#' @description We observe a sample of i.i.d. real random variables \eqn{(X_{i}, Y_{i}), 1 \le i \le n} and consider the model
#' \eqn{Y_i = m(X_i) + \varepsilon_i}. The \eqn{\varepsilon_{i}} are i.i.d., centred, with common variance, the \eqn{X_i}
#' are i.i.d. with common density \eqn{f}. Moreover, the \eqn{(X_i)_{1 \le i \le n}} are independent. This function deduces an estimator of the
#' function m.
#' @param basis1 an object of class \code{\link{Basis}}
#' @param basis2 an object of class \code{\link{Basis}}
#' @param data a data frame consisting of two columns. The two columns represent an observed sample of i.i.d. real random variables 
#' \eqn{(X_{i}, Y_{i}), 1 \le i \le n}.
#' @details To compute the estimate of m, the quotient of the adaptive estimator of \eqn{l := m*f} and the adaptive estimator of \eqn{f}
#' is used (see \code{\link{est_dens}} and \code{\link{perfect_D}}). These two estimators can be calculated using two different bases.
#' @export
#' @return An estimate for \eqn{m}  
#' @examples
#' \dontrun{
#' trig_bas <- Trig_Basis$new(100)
#' dens1 <- function(x) dunif(x)
#' data <- datsim(den1, log, 150)
#' reg_m <- reg_est(trig_bas, trig_bas, data)
#' reg_m
#' }
reg_est <- function(basis1, basis2, data) {
  stopifnot(is.data.frame(data))
  stopifnot(ncol(data) == 2) 
  stopifnot(any(class(basis1) %in% "Basis")) 
  stopifnot(any(class(basis2) %in% "Basis"))
  D <- perfect_D(basis1, data[, 1])
  D_prime <- perfect_D(basis2, data[, 1])
  x <- data[,1]
  y <- data[,2]
  # coefficients for \hat{l_D}
  coef1 <- sapply(1:D, function(ind) {
    reg_coef(basis1, x, y, ind)
  })
  # coefficients for \hat{f_{D'}}
  coef2 <- sapply(1:D_prime, function(ind) {
    est_proj_coef(basis2, x, ind)
  })
  m_ddprime <- function(x) {
    # \hat{l_D}
    build_sum1 <- vapply(
      1:D,
      function(ind) {
        basis1$get_function(ind)(x) * coef1[ind]
      },
      1
    )
    # \hat{l_{D'}}
    build_sum2 <- vapply(
      1:D_prime,
      function(ind) {
        basis2$get_function(ind)(x) * coef2[ind]
      },
      1
    )
    return(sum(build_sum1) / sum(build_sum2))
  }
  m_ddprime <- Vectorize(m_ddprime)
  return(m_ddprime)
}
