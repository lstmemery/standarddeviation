#' Get the standard deviation of a vector
#' 
#' @param x a numeric vector
#' @return The standard deviation of the numeric vector
#' 
#' @examples
#' standard_deviation(c(1, 2, 3))
#' @export
standard_deviation <- function(x) {
  n <- length(x)
  mean = sum(x) / n
  ssq <- sum((x-mean)^2)
  stddev = sqrt(ssq/n)
  return(stddev)
}