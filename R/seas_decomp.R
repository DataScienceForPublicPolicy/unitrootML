#' Extract seasonal decomposition from a time series
#'
#' Test for seasonality in a time series based on lags
#' @param x A time series object
#' @param transform Boolean for whether a Box-Cox transform is applied
#' @return A list object with QS-statistic and critical value
#' @references https://robjhyndman.com/hyndsight/tscharacteristics/
#' @author Rob Hyndman
#' @examples seas_decomp(ts(rnorm(120, 10,10), frequency=12))
#' @export
#'

seas_decomp <- function(x, transform = TRUE){

  # Transform series
  if(transform & min(x, na.rm = TRUE) >= 0){
    lambda <- forecast::BoxCox.lambda(na.contiguous(x))
    x <- forecast::BoxCox(x,lambda)
  } else  {
    lambda <- NULL
    transform <- FALSE
  }

  # Seasonal data
  if(frequency(x)>1)  {
    x.stl <- stl(x,
                 s.window = "periodic",
                 na.action = na.contiguous)
    trend <- x.stl[["time.series"]][,2]
    season <- x.stl[["time.series"]][,1]
    remainder <- x - trend - season
  } else #Nonseasonal data
    {
    tt <- 1:length(x)
    trend <- rep(NA,length(x))
    trend[!is.na(x)] <- fitted(mgcv::gam(x ~ s(tt)))
    season <- NULL
    remainder <- x - trend
  }


  return(list(x = x,
              trend = trend,
              season = season,
              remainder = remainder,
              transform = transform,
              lambda = lambda))
}
