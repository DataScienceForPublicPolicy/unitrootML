#' Calculate time series characteristics
#'
#' Test for seasonality in a time series based on lags
#' @param x A time series object
#' @return A vector of time series characteristics
#' @references https://robjhyndman.com/hyndsight/tscharacteristics/
#' @author Rob Hyndman
#' @examples ts_measures(ts(rnorm(120, 10,10), freq=12))
#' @export
#'

#CALCULATE MEASURES
  ts_measures <- function(x){

    N <- length(x)
    freq <- stats::frequency(x)
    fx <- c(frequency = (exp((freq - 1)/50) - 1)/(1 + exp((freq - 1)/50)))
    x <- ts(x, f = frequency(x))

    # Decomposition
    decomp.x <- seas_decomp(x)

    # Adjust data
    if(freq > 1){
      fits <- decomp.x[["trend"]] + decomp.x[["season"]]
    } else {
      # Nonseasonal data
      fits <- decomp.x[["trend"]]

    }
    adj.x <- decomp.x[["x"]] - fits + mean(decomp.x[["trend"]], na.rm=TRUE)

    # Backtransformation of adjusted data
    if(decomp.x[["transform"]])  {
      tadj.x <- forecast::InvBoxCox(adj.x,
                          decomp.x[["lambda"]])
      tadj.x[is.infinite(tadj.x)] <- max(tadj.x[!is.infinite(tadj.x)])
      } else{
      tadj.x <- adj.x
    }


    # Trend and seasonal measures
    v.adj <- var(adj.x, na.rm=TRUE)
    if(freq > 1) {
      detrend <- decomp.x[["x"]] - decomp.x[["trend"]]
      deseason <- decomp.x[["x"]] - decomp.x[["season"]]
      trend <- ifelse(var(deseason,na.rm=TRUE) < 1e-10, 0,
                      max(0,min(1,1-v.adj/var(deseason,na.rm=TRUE))))
      season <- ifelse(var(detrend,na.rm=TRUE) < 1e-10, 0,
                       max(0,min(1,1-v.adj/var(detrend,na.rm=TRUE))))
    }  else #Nonseasonal data
    {
      trend <- ifelse(var(decomp.x[["x"]],na.rm=TRUE) < 1e-10, 0,
                      max(0,min(1,1-v.adj/var(decomp.x[["x"]],na.rm=TRUE))))
      season <- 0
    }

    m <- c(fx, trend, season)

    # Measures on original data
      xbar <- base::mean(x, na.rm=TRUE)
      s <- sd(x, na.rm=TRUE)

      # Serial correlation
      Q <- Box.test(x,lag=10)[["statistic"]]/(N*10)
      fQ <- scale_01(Q, 7.53, 0.103)

      # Nonlinearity
      p <- tseries::terasvirta.test(na.contiguous(x))[["statistic"]]
      fp <- scale_inf(p, 0.069, 2.304)

      # Skewness
      sk <- abs(mean((x-xbar)^3,na.rm=TRUE)/s^3)
      fs <- scale_inf(sk, 1.510, 5.993)

      # Kurtosis
      k <- mean((x-xbar)^4,na.rm=TRUE)/s^4
      fk <- scale_inf(k, 2.273, 11567)

      # Hurst=d+0.5 where d is fractional difference.
      H <- fracdiff::fracdiff(na.contiguous(x),0,0)[["d"]] + 0.5

      # Lyapunov Exponent
      if(freq > N-10){
        stop("Insufficient data")
      }

      Ly <- numeric(N - freq)
      for(i in 1:(N - freq))
      {
        idx <- order(abs(x[i] - x))
        idx <- idx[idx < (N-freq)]
        j <- idx[2]
        Ly[i] <- log(abs((x[i+freq] - x[j+freq])/(x[i]-x[j])))/freq
        if(is.na(Ly[i]) | Ly[i]==Inf | Ly[i]==-Inf)
          Ly[i] <- NA
      }
      Lyap <- mean(Ly,na.rm=TRUE)
      fLyap <- exp(Lyap)/(1+exp(Lyap))

      m <- c(m,fQ,fp,fs,fk,H,fLyap)

    # Measures on adjusted data
      xbar <- mean(tadj.x, na.rm=TRUE)
      xbar<- ifelse(is.infinite(xbar), 10e6, xbar)

      s <- sd(tadj.x, na.rm=TRUE)
      s <- ifelse(is.infinite(s), 10e6, s)

      # Serial
      Q <- Box.test(adj.x,lag=10)[["statistic"]]/(N*10)
      fQ <- scale_01(Q,7.53,0.103)

      # Nonlinearity
      p <- tseries::terasvirta.test(na.contiguous(adj.x))[["statistic"]]
      fp <- scale_inf(p,0.069,2.304)

      # Skewness
      sk <- abs(mean((tadj.x/max(tadj.x/2, na.rm = T) - xbar/max(tadj.x/2, na.rm = T))^3,
                     na.rm=TRUE)/(s/max(tadj.x/2, na.rm = T))^3)
      fs <- scale_inf(sk, 1.510, 5.993)

      # Kurtosis
      k <- mean((tadj.x/max(tadj.x/2, na.rm = T) - xbar/max(tadj.x/2, na.rm = T))^4,
                na.rm=TRUE)/(s/max(tadj.x/2, na.rm = T))^4
      fk <- scale_inf(k, 2.273, 11567)

      #Return objects
      m <- c(m,fQ,fp,fs,fk)
      names(m) <- c("frequency", "trend","seasonal",
                    "autocorrelation","non-linear","skewness","kurtosis",
                    "Hurst","Lyapunov",
                    "dc autocorrelation","dc non-linear","dc skewness","dc kurtosis")

    return(m)
  }
