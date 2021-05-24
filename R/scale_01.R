#' Scale data from (0 to 1) to (0 to 1)
#'
#' Scaling to 0/1 from 0/1. This is a support function that is not intended for the end user to directly use.
#' @param x A time series object
#' @param a param a
#' @param b param b
#' @return Scaled values
#' @references https://robjhyndman.com/hyndsight/tscharacteristics/
#' @author Rob Hyndman
#' @examples
#' scale_01(ts(rnorm(120, 10,10),
#'            frequency=12))
#' @export
#'


scale_01 <- function(x,a,b){
  eax <- exp(a*x)
  ea <- exp(a)
  return((eax-1)/(eax+b) * (ea+b)/(ea-1))
}

