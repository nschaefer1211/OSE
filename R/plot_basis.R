#' @title Plotting basis functions
#'
#' @description Given an object of class \code{\link{Basis}}, this function plots the functions contained in this object.
#' @import ggplot2
#' @importFrom graphics plot
#' @param basis an object of class \code{\link{Basis}} 
#' @param ind a numeric atomic vector, containing information about which functions of the \code{basis} should be plotted
#' @param resol a numeric value, specifying the quality of the plot(length.out of processed values)
#' @export
#' @return no return
#' @examples
#'
#' hist <- Hist_Basis$new(10)
#' plot_basis(hist, c(1, 3, 7))
#' 
#' trig <- Trig_Basis$new(100)
#' #plot the first three basis functions
#' plot_basis(trig, seq(3))
#' 
plot_basis <- function(basis, ind, resol = 300) {
  stopifnot(any(class(basis) %in% "Basis"))
  stopifnot(is.numeric(ind))
  stopifnot(is.numeric(resol))
  stopifnot(length(resol) == 1)
  x <- seq(0, 1, length.out = resol)
  data <- data.frame(x)
  for (j in ind) {
    data[paste("ind", j, sep = "_")] <- basis$get_function(j)(x)
  }
  p <- ggplot(data, aes(x = x))
  for (j in ind) {
    p <- p + geom_line(aes_string(y = paste("ind", j, sep = "_")))
  }
  p <- p + labs(title = "Basis", y = "y")
  plot(p)
}
