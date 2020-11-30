#' @title Two dimensional projection estimator
#' 
#' @description Given a sample of i.i.d real random bivariate vectors with a common density f and a basis of
#' class \code{\link{Basis}}. This function calculates the two dimensional projection estimator of the destiny f.
#' @param basis an object of class \code{\link{Basis}}
#' @param data a data frame with two columns of samples of i.i.d. real random variables 
#' @param dim a vector of length two. If only one value is passed, this value is reproduced to attain a vector of length two
#' @export
#' @return The two dimensional projection estimator of the common density of the data stored in \code{data}, given \code{basis} 
#' @examples
#' # example with the trigonometric basis
#' d <- 10
#' d1 <- 8
#' d2 <- 5
#' trig_bas <- Trig_Basis$new(d)
#' x <- runif(100)
#' y <- runif(100)
#' estimated_density <- est_dens2(trig_bas, data = data.frame(x, y), dim = c(d1, d2))
est_dens2 <- function(basis, data, dim) {
  stopifnot(is.data.frame(data))
  stopifnot(ncol(data) == 2) 
  stopifnot(length(dim) <= 2)
  stopifnot(length(dim) > 0)
  stopifnot(is.numeric(dim))
  stopifnot(any(class(basis) %in% "Basis")) 
  if(length(dim) == 1){
    dim <- c(dim, dim)
  }
  D1 <- dim[[1]]
  D2 <- dim[[2]]
  stopifnot(dim[[1]] <= basis$dimension & dim[[2]] <= basis$dimension)
  stopifnot(dim[[1]] > 0, dim[[2]] > 0)
  data1 <- data[,1]
  data2 <- data[,2]
  coef <- matrix(nrow = D1, ncol = D2)
  for (k in 1:D2) {
    for (j in 1:D1) {
      coef[j, k] <- est_proj2_coef(basis, data1, data2, j, k)
    }
  }
  f_d2 <- function(x, y) {
    ma <- matrix(nrow = length(x), ncol = length(x))
    for (i in 1:length(x)) {
      for (l in 1:length(y)) {
        build_sum <- c()
        for (k in 1:D2) {
          for (j in 1:D1) {
            build_sum <- c(build_sum, basis$get_function(j)(x[i]) * basis$get_function(k)(y[l]) * coef[j, k])
          }
        }
        ma[i, l] <- sum(build_sum)
      }
    }
    return(ma)
  }
  return(f_d2)
}