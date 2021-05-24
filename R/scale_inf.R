#' Scale a series from (0 to infinity) to (0 to 1)
#'
#' Scaling from (0 to infinity) to (0 to 1). This is a support function that is not intended for the end user to directly use.
#' @param x A time series object
#' @param a param a
#' @param b param b
#' @return Scaled values
#' @references https://robjhyndman.com/hyndsight/tscharacteristics/
#' @author Rob Hyndman
#' @examples scale_inf(ts(rnorm(120, 10,10), frequency=12))
#' @export
#'

scale_inf <- function(x, a = 1.510, b = 5.993){
  eax <- exp(a * x)
  if(is.infinite(eax)){
    f1eax <- 1
  } else{
    f1eax <- (eax - 1)/(eax + b)
  }
  return(f1eax)
}
