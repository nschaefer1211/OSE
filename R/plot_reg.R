#' @title Plot regression estimator
#'
#' @description This function plots the regression estimator given by \code{\link{reg_est}}.
#' @import ggplot2 
#' @param basis1 an object of class \code{\link{Basis}}
#' @param basis2 an object of class \code{\link{Basis}}
#' @param data a data frame consisting of two columns. The two columns represent an observed sample of i.i.d. real random variables 
#' \eqn{(X_{i}, Y_{i}), 1 \le i \le n}.
#' @param f a function. This function (if passed) may be used to compare the estimate to a known function. The default ist NULL. 
#' @param compare a logical value. If TRUE (the default), the regression estimate is compared to \code{f} if \code{f} is passed.
#' If FALSE only the estimate is plotted
#' @details Note, that the graph only contains a scatter plot consisting of the data points passed by \code{data}. 
#' @export
#' @return no return
#' @examples
#' \dontrun{
#' hist_bas <- Hist_Basis$new(15)
#' dens1 <- function(x) dnorm(x, 0.5, 0.088)
#' datasim <- datsim(dens1, log, 15)
#' plot_reg(hist_bas, hist_bas, datasim, f = log)
#' }
plot_reg <- function(basis1, basis2, data, f = NULL, compare = TRUE) {
  stopifnot(is.data.frame(data))
  stopifnot(ncol(data) == 2) 
  stopifnot(any(class(basis1) %in% "Basis")) 
  stopifnot(any(class(basis2) %in% "Basis"))
  x <- seq(0, 1, length.out = nrow(data))
  df <- data.frame(x, data)
  df["reg_est"] <- reg_est(basis1, basis2, data)(x)
  if (is.null(f) == FALSE) {
    stopifnot(class(f) == "function")
    df["f"] <- f(x)
  }
  p <- ggplot(df, aes(x = x)) + geom_jitter(aes(x = data[,1], y = data[,2]), color = "black") 
  #save y.range and x.range
  yrange <- ggplot_build(p)$layout$panel_scales_y[[1]]$range$range
  xrange <- ggplot_build(p)$layout$panel_scales_x[[1]]$range$range
  p <- p + geom_line(aes_string(y = "reg_est", color = shQuote("estimated regression function")), size = 1) 
  p <- p + coord_cartesian(ylim = c(yrange[1], yrange[2]), xlim = c(xrange[1], xrange[2]))
  p <- p + labs(title = "Estimated regression function", y = "y")
  if (is.null(f) == FALSE && compare == TRUE) {
    p <- p + geom_line(aes_string(y = "f", color = shQuote("comparison function f")), size = 1)
    p <- p + scale_color_discrete(name = "regression functions", l = 40, c = 80)
  }
  else {
    p <- p + theme(legend.position = "none") + scale_color_discrete(l = 50) 
  }
  plot(p)
}
