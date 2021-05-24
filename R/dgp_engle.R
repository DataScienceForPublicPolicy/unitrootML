#' Generate data according to Heteroskedasticitic error term from Engle (1982)
#'
#' Returns a vector of
#' @param periods A time series object
#' @param sd Standard deviation
#' @param gamma Gamma
#' @param theta Theta
#' @param alpha0 Alpha0
#' @param alpha1 Alpha1
#' @return A data frame
#' @author Gary Cornwall and Jeffrey Chen
#' @examples
#' dgp_engle(periods = 100,
#'           sd = 1,
#'           gamma = 1,
#'           theta = .5,
#'           alpha0 = 1,
#'           alpha1 = .5)
#' @export
#'
#'


dgp_engle <- function(periods = 100, sd = 1, gamma = 1, theta = .5, alpha0 = 1, alpha1 = .5) {

  data <- rep(0,periods)
  p2 <- 2*periods
  for (i in 2:p2) {
    data[i] <-  gamma*data[i-1] + rnorm(1)*sqrt(alpha0 + alpha1*abs(data[i-1]))
  }
  data <- data[(periods + 1):p2]
  return(data)
}
