#' Generate time series with properties according to Equation 1 page 183 of Enders (2008).
#'
#' Returns a vector of time series values
#' @param periods Number of periods
#' @param sd Standard deviation
#' @param alpha0 Alpha0
#' @param alpha2 Alpha2
#' @param gamma Gamma
#' @return A data frame
#' @author Gary Cornwall and Jeffrey Chen
#' @references Enders, Walter. 2008 Applied econometric time series. New York: Wiley.
#' @examples dgp_enders1(periods = 200, sd = 1, alpha = 1, alpha2 = 0.005, gamma = 1)
#' @export
#'
#'


dgp_enders1 <- function(periods = 100, sd = 1, alpha0 = 1, alpha2 =  .005, gamma = 1) {
  #This function will generate data according to equation 1 page 183 of Enders.
  data <- rep(0,periods)
  p2   <- 2*periods
  for (i in 2:p2) {
    data[i] <- alpha0 + gamma*data[i-1] + alpha2*i + rnorm(1,mean = 0,sd = sd)
  }
  data <- data[(periods+1):p2]
  return(data)
}
