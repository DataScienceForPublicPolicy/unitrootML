#' Draw random values from an assortment of distributions
#'
#' @param n Number of observations Default = 1.
#' @param mu Mean. Default = 0.
#' @param sd Standard deviation. Default = 1.
#' @param hi_lo A value to represent maximum value and minimum value for binomial distribution. or uniform distribution. A value of 2 will set a maximum of +2 and a minimum of -2. Default = 1.
#' @param symmetric Boolean indicating whether simulate symmetry. Default = TRUE. When FALSE, an absolute value is applied to the noise parameter to force assymmetry
#' @param error_type Error distribution. Options include "gaussian", "binomial" and "uniform".
#' @return A numeric value or vector
#' @author Gary Cornwall and Jeffrey Chen
#' @examples
#' #Draw 100 from a binomial distribution of -3 and 3
#' make_noise(n = 100, error_type = "binomial", hi_lo = 3)
#'
#' #Draw 1 value from a Gaussian distribution N(2,10)
#' make_noise(n = 1, error_type = "gaussian", mu = 2, sd = 10)
#' @export
#'
#'


make_noise <- function(n = 1,
                       mu = 0,
                       sd = 1,
                       hi_lo = 1,
                       error_type = "gaussian",
                       symmetric = TRUE) {

  #Generate Noise
  if(error_type == "gaussian"){
      #Gaussian distribution
        noise <- rnorm(n,
                       mean = mu,
                       sd = sd)
  } else if(error_type == "uniform") {
    #Uniform distribution
    noise <- runif(n,
                   min = -hi_lo,
                   max = hi_lo)
  } else if(error_type == "binomial") {
    #Binomial distribution
      noise <- sample(c(-hi_lo, hi_lo),
                      n,
                      replace = TRUE)
    }

  #If not symmetric, take absolute value of noise object
  if(!symmetric){
    noise <- abs(noise)
  }

  #Return result
  return(noise)
}
