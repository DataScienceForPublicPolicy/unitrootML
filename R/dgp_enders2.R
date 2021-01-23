#' Generate time series with properties according to Equation 2 page 183 of Enders (2008).
#'
#' Returns a vector of time series values
#' @param periods Number of periods
#' @param sd Standard deviation
#' @param alpha0 Alpha0
#' @param gamma Gamma
#' @return A data frame
#' @author Gary Cornwall and Jeffrey Chen
#' @references Enders, Walter. 2008 Applied econometric time series. New York: Wiley.
#' @examples dgp_enders2(periods = 200, sd = 1, alpha0 = 1,  gamma = 1)
#' @export
#'
#'


dgp_enders2 <- function(periods = 100, sd = 1, alpha0 = 1, gamma = 1) {

  data <- rep(0,periods)
  p2   <- 2*periods

  for (i in 2:p2) {
    data[i] <- alpha0 + gamma*data[i-1] + rnorm(1,mean = 0, sd = sd)
  }

  data <- data[(periods+1):p2]
  return(data)
}
