#' @title Plot bivariate estimated density function
#'
#' @description This function plots the estimated bivariate density function for a given set of data using a given orthonormal basis
#' of some dimension.
#' @importFrom plotly plot_ly add_surface layout
#' @importFrom magrittr %>% 
#' @param basis an object of class \code{\link{Basis}} 
#' @param data a data frame with two columns, each containing a sample of i.i.d. real random variables with common density.
#' @param dim a vector of length two. If only one value is passed, this value is reproduced to attain a vector of length two.
#' @param resol a numeric value, specifying the quality of the plot(length.out of processed values). The default is 300.
#' @export
#' @return no return
#' @examples
#' \dontrun{
#' trig_bas <- Trig_Basis$new(1000)
#' dens <- function(x) dnorm(x, 0.5, 0.088)
#' data <- rdens(dens, 150)
#' p9 <- plot_dens2(trig_bas, data = data.frame(data, data), dim = c(8, 8))
#' }
plot_dens2 <- function(basis, data, dim, resol = 300){
  stopifnot(is.data.frame(data))
  stopifnot(ncol(data) == 2) 
  stopifnot(length(dim) <= 2)
  stopifnot(length(dim) > 0)
  stopifnot(is.numeric(dim))
  stopifnot(any(class(basis) %in% "Basis")) 
  if(length(dim) == 1){
    dim <- c(dim, dim)
  }
  x <- seq(0, 1, length.out = resol)
  y <- seq(0, 1, length.out = resol)
  z <- est_dens2(basis, data, dim)(x, y)
  p <- plot_ly(x = x, y = y, z = z) %>% add_surface() %>% layout(title = "Bivariate projection estimator")
  p
}