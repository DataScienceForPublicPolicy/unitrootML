#' Generate data according to Equation 1 page 183 of Enders.
#'
#' Returns a vector of time series values
#' @param periods A time series object
#' @param sd Standard deviation
#' @param gamma Gamma
#' @param theta Theta
#' @return A data frame
#' @author Gary Cornwall and Jeffrey Chen
#' @examples dgp_ar1_mae(periods = 100, sd = 1, gamma = 1, theta = .5)
#' @export
#'
#'

dgp_ar1_mae <- function(periods = 100, sd = 1, gamma = 1, theta = .5) {
  #AR1 with moving average errors
  data <- rep(0,periods)
  p2 <- 2*periods
  nu <- rnorm(p2, mean = 0, sd = sd)
  for (i in 2:p2) {
    data[i] <- runif(1,-100,100) + gamma * data[i-1] + nu[i] + theta*nu[i-1]
  }
  data <- data[(periods + 1):p2]
}
