#' Estimate a battery of statistical tests for unit roots and time series features
#'
#' Returns a set of commonly used time series statistical tests and time series features.
#' @param series A time series object
#' @param cval A critical value as a p-value
#' @return A data frame
#' @author Gary Cornwall and Jeffrey Chen
#' @examples ts_features(ts(rnorm(120, 10,10), freq=12))
#' @export
#'
#'
ts_features <- function(series, cval = 0.05) {

  #require(tseries, fUnitRoots, egcm, urca)

  #frequency
  freq <- stats::frequency(series)

  #PART 1: Calculate a set of Unit Root Statistics

  # ADF Test:
  #   Dickey, D. A. and Fuller, W. A. (1979),
  #       Distributions of the Estimators For Autoregressive Time Series with a Unit Root,
  #       Journal of the American Statistical Association, 75, 427-431.
  #   Dickey, D. A. and Fuller, W. A. (1981),
  #       Likelihood Ratio Statistics for Autoregressive Time Series with a Unit Root,
  #       Econometrica, 49, 1057-1072.
  #   Hamilton (1994), Time Series Analysis, Princeton University Press.

    df_test <- urca::ur.df(series,
                           type = 'none',
                           selectlags = 'BIC')

    adf_statistic <- df_test@teststat
    phi_estimate <- as.matrix(rep(NA,15), nrow = 1, ncol = 15)
    phi_estimate <- c(df_test@testreg$coefficients, rep(NA, 15 - length(df_test@testreg$coefficients)))

  # KPSS Statistic:
  #    D. Kwiatkowski, P. C. B. Phillips, P. Schmidt, and Y. Shin (1992):
  #       Testing the Null Hypothesis of Stationarity against the Alternative of a Unit Root.
  #       Journal of Econometrics 54, 159-178.

    kpss_statistic  <- urca::ur.kpss(series,
                                       type = 'mu')@teststat


  #PGFF Statistic:
  #   Pantula, S. G., Gonzalez-Farias, G., and Fuller, W. A. (1994).
  #     A comparison of unit-root test criteria.
  #     Journal of Business & Economic Statistics, 12(4), 449-459

    pp_statistic    <- urca::ur.pp(series,
                                   type = 'Z-alpha',
                                   model = 'trend',
                                   lags = 'short')@teststat

    pgff_statistic  <- egcm::pgff.test(series,
                                 detrend = F)$statistic

  #Breitung Statistic:
  #   Breitung, J. (2002).
  #     Nonparametric tests for unit roots and cointegration.
  #     Journal of Econometrics, 108(2), 343-363.
  #   Breitung, J. and Taylor, A.M.R. (2003)
  #     Corrigendum to "Nonparametric tests for unit roots and cointegration"
  #     [J. Econom. 108 (2002) 343-363] Journal of Econometrics, 117(2), 401-404.

    breit_statistic <- egcm::bvr.test(series,
                                detrend = F)$statistic

  #ERS Statistic - D
  #   Elliott, G., Rothenberg, T. J., & Stock, J. H. (1992).
  #     Efficient tests for an autoregressive unit root.

    ers_statistic_d <- urca::ur.ers(series,
                                      type = "DF-GLS")@teststat

  #ERS Statistic - P
  #   Elliott, G., Rothenberg, T. J., & Stock, J. H. (1992).
  #     Efficient tests for an autoregressive unit root.

    ers_statistic_p <- urca::ur.ers(series,
                                      type = 'P-test')@teststat

  #URSP Statistic
  #   Schmidt, P., & Phillips, P. C. (1992).
  #     LM tests for a unit root in the presence of deterministic trends.
  #     Oxford Bulletin of Economics and Statistics, 54(3), 257-287.

    ursp_statistic <- urca::ur.sp(series,
                                    type = 'rho',
                                    signif = cval)@teststat

  #URZA Statistic
  #   Zivot, E., & Andrews, D. W. K. (2002).
  #   Further evidence on the great crash, the oil-price shock, and the unit-root hypothesis.
  #   Journal of business & economic statistics, 20(1), 25-44.

    urza_statistic <- urca::ur.za(series)@teststat


  # PART 2: Collect Time Series Features

  # Calculate time series attributes
    ts_diff <- data.frame(t(ts_measures(diff(series))))

    colnames(ts_diff) <- tolower(paste0("diff.",
                                        gsub("[[:space:][:punct:]]",
                                             "",
                                             colnames(ts_diff))))

    ts_levels <- data.frame(t(ts_measures(series)))

    colnames(ts_levels) <- tolower(paste0("lvl.",
                                          gsub("[[:space:][:punct:]]",
                                               "",
                                               colnames(ts_levels))))


  # Compile results
    # Test statistics as a data frame
    out <- data.frame(n = length(series),
                          freq = freq,
                          adf_stat   = adf_statistic,
                          kpss_stat  = kpss_statistic,
                          pp_stat    = pp_statistic,
                          pgff_stat  = pgff_statistic,
                          breit_stat = breit_statistic,
                          ers_stat_d = ers_statistic_d,
                          ers_stat_p = ers_statistic_p,
                          urza_stat  = urza_statistic,
                          ursp_stat  = ursp_statistic,
                          var_ratio  = sd(diff(series))/sd(series),
                          ts_diff,
                          ts_levels)
    colnames(out)[3] <- "adf_stat"

     phi_estimate <- t(phi_estimate)
     colnames(phi_estimate) <- paste0('phi',1:15)

   #Return results
     return(cbind(out, phi_estimate))


}
