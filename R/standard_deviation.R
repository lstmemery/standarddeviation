#' Get the standard deviation of a vector
#' 
#' @param numbers a numeric vector
#' @return The standard deviation of the numeric vector 
standard_deviation <- function(x) {
  n <- length(x)
  mean = sum(x) / n
  ssq <- sum((x-mean)^2)
  stddev = sqrt(ssq/n)
  return(stddev)
}