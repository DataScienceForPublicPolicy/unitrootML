#' Generate time series with properties according to Equation 3 page 183 of Enders (2008).
#'
#' Returns a vector of time series values
#' @param periods Number of periods
#' @param sd Standard deviation
#' @param gamma Gamma
#' @return A data frame
#' @author Gary Cornwall and Jeffrey Chen
#' @references Enders, Walter. 2008 Applied econometric time series. New York: Wiley.
#' @examples dgp_enders3(periods = 200, sd = 1, gamma = 1)
#' @export
#'
#'


dgp_enders3 <- function(periods = 100, sd = 1, gamma = 1) {

  data <- rep(0,periods)
  p2   <- 2*periods

  for (i in 2:p2) {
    data[i] <- gamma*data[i-1] + rnorm(1,mean = 0, sd = sd)
  }

  data <- data[1:periods]
  return(data)
}
