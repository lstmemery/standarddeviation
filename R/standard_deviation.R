#' Get the standard deviation of a vector
#'
#' @param x a numeric vector
#' @return The standard deviation of the numeric vector
#'
#' @examples
#' standard_deviation(c(1, 2, 3))
#' 0.8164966
#' @export
standard_deviation <- function(x) {
  n <- length(x)
  mean = sum(x) / n
  ssq <- sum((x-mean)^2)
  stddev = sqrt(ssq/n)
  return(stddev)
}

#' Calculate the standard error of a vector
#' @param x: a numeric vector
#' @return: The standard error
#'
#' @examples
#' standard_error(c(1, 2, 3))
#' 0.4714045
#' @export
standard_error <- function(x) standard_deviation(x)/sqrt(length(x))
