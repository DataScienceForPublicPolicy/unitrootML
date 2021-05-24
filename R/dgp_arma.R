#' Generate data according to ARMA process
#'
#' Returns a time series vector
#' @param periods A time series object
#' @param sd Standard deviation
#' @param gamma Unit root
#' @param theta Theta
#' @return A data frame
#' @author Gary Cornwall and Jeffrey Chen
#' @examples
#' dgp_arma(periods = 100,
#'          sd = 1,
#'          gamma = 1,
#'          theta = -.25)
#' @export
#'
#'


dgp_arma <- function(periods = 100, sd = 1, gamma = 1, theta = -.25) {
  data <- rep(0,periods)
  p2   <- 2*periods
  data[1] <- runif(1,-100,100)
  e <- rnorm(2*periods)
  for (i in 2:p2) {
    data[i] <- gamma*data[i-1] + theta*e[i-1] + e[i]
  }
  data <- data[(periods+1):p2]
  return(data)
}
