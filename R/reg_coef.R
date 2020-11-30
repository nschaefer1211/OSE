#Internal helper function
#' @title Regression Coefficient Estimation
#' 
#' @description internal helper function
#' @export
#' @keywords internal
#' @return
#' @examples
#' \dontrun{
#' 
#' }
reg_coef <- function(basis, x, y, j) {
  stopifnot(length(x) == length(y))
  phi_j <- y * basis$get_function(j)(x)
  return(mean(phi_j))
}
