library(assertthat)

#' Get the standard deviation of a vector
#'
#' @param x a numeric vector
#' @return The standard deviation of the numeric vector
#'
#' @import assertthat
#' @examples
#' standard_deviation(c(1, 2, 3))
#' 1
#' @export
standard_deviation <- function(x) {
  stopifnot(is.numeric(x))
  n <- length(x)
  stopifnot(n >= 2)
  mean = sum(x) / n
  ssq <- sum((x - mean)^2)
  stddev = sqrt(ssq/(n - 1))
  return(stddev)
}

#' Calculate the standard error of a vector
#' @param x a numeric vector
#' @return: The standard error
#'
#' @import assertthat
#' @examples
#' standard_error(c(1, 2, 3))
#' 0.5773503
#' @export
standard_error <- function(x) standard_deviation(x)/sqrt(length(x))
