#' Generate a bank of time series for use in a Near Unit Root/Unit Root training set
#'
#' @description Returns a list object of time series objects with their attributes. Whether a time series has a unit root is drawn at random based on a sampling probability (sample_prob). The length of the series is randomly drawn between t_min and t_max. The type of unit root process follows standard DGPs which can be calibrated and the split of each DGP can be controlled.
#' @param iter The number of series that will be generated.
#' @param sample_prob The probability of that a unit root exists in the sample. For example, 0.5 would indicate 50% of series have a unit root.
#' @param t A vector of length 2 that contains the minimum and maximum number of periods (default = c(5,50))
#' @param freq Time series frequency (default = 12)
#' @param nur_ur A vector of length 2 that contains the Phi domain (default = c(0.9, 0.99999))
#' @param run_par Boolean indicating whether to compute in parallel.
#' @param dgp_params A list object indicating which DGPs will be used and in what proportion within the sample_prob (see DGP functions). The basic DGP should include a tag "dgp" with a string value indicating which dgp function (e.g. "dgp_enders1", "dgp_engle") and the arguments required to execute the function. The parameters "period" will be handled by "t" and "gamma" is a function of the "sample_prob" parameter.  Note that this function that DGPs will be evenly split amongst the sample_prob. For example, if sample_prob = 0.5 and two DGPs are specified, 25% of the overall sample will be generated using each DGP.
#' @return A list object
#' @author Gary Cornwall and Jeffrey Chen
#' @examples
#' #Generate a new time series
#' gen_bank(iter = 300,
#'         dgp_params = list(dgp = "dgp_enders1",
#'                           periods = 100,
#'                           sd = 1,
#'                           alpha0 = 1,
#'                           alpha2 =  .005,
#'                           gamma = 1))
#' @export
#'
#'


gen_bank <- function(iter = 500000,
                     sample_prob = .50,
                     t = c(5,50),
                     freq = 12,
                     nur_ur = c(0.90000,.99999),
                     run_par = TRUE,
                     dgp_params = list(dgp = "dgp_enders1", periods = 100, sd = 1,
                                       alpha0 = 1, alpha2 =  .005, gamma = 1)) {

  #Set connectors
    `%dopar%` <- foreach::`%dopar%`
    `%>%` <- magrittr::`%>%`

  #Extract DGPs
    if("dgp" %in% names(dgp_params) ){
      dgp_frame <- dgp_params$dgp
      params <- do.call(data.frame, dgp_params)
    } else {
      dgp_frame <- unlist(lapply(dgp_params, function(x){return(x[["dgp"]])}))
      params <- plyr::ldply(dgp_params, data.frame)
    }

  #Check DGPs are present
    if(is.null(dgp_frame) | length(dgp_frame) < 1){
      stop("Call terminated: 'dgp' tag not supplied")
    }

  #Check if DGPs matches functions
    match_rate <- mean(dgp_frame %in% c("dgp_engle", "dgp_arma", "dgp_enders1",
                                        "dgp_enders2", "dgp_enders3", "dgp_ar1_mae"))

    if(match_rate < 1){
      stop("Call terminated: A 'dgp' in dgp_params does not match supported functions. \n  Review 'dgp_* functions in reference documentation")
    }


   #Set up cluster
    if(run_par){
      cl <- parallel::makeCluster(parallel::detectCores(logical = F)-1)
      doParallel::registerDoParallel(cl)
      parallel::clusterExport(cl, c("dgp_engle", "dgp_arma", "dgp_enders1",
                                    "dgp_enders2", "dgp_enders3", "dgp_ar1_mae"))
    }

  #Run iterations
  data <- foreach::foreach(i = 1:iter) %dopar% {

      #Load parameters
      len <- floor(runif(1, t[1], t[2]))
      ur_flag <- ifelse(runif(1) < sample_prob, 1, 0)
      gamma <- ifelse(ur_flag == 1, 1, runif(1, nur_ur[1], nur_ur[2]))


      #Gen the specific time series
      dgp_select <- sample(dgp_frame, 1)
      dgp_index <- grep(dgp_select, dgp_frame)


          if(dgp_select == "dgp_ar1_mae"){

            #AR1 with Moving Average
            temp_series <- dgp_ar1_mae(periods = (freq * len),
                                       sd = params[dgp_index,"sd"],
                                       gamma = gamma,
                                       theta = params[dgp_index,"theta"])

          } else if(dgp_select == "dgp_arma"){

            #ARMA
            temp_series <- dgp_arma(periods = (freq * len),
                                       sd = params[dgp_index,"sd"],
                                       gamma = gamma,
                                       theta = params[dgp_index,"theta"])

          } else if(dgp_select == "dgp_enders1"){

            #Enders 1
            temp_series <- dgp_enders1(periods = (freq * len),
                                      sd = params[dgp_index,"sd"],
                                      alpha0 = params[dgp_index,"alpha0"],
                                      alpha2 = params[dgp_index,"alpha2"],
                                      gamma = gamma)

          } else if(dgp_select == "dgp_enders2"){

            #Enders 2
            temp_series <- dgp_enders2(periods = (freq * len),
                                      sd = params[dgp_index,"sd"],
                                      alpha0 = params[dgp_index,"alpha0"],
                                      gamma = gamma)

          }else if(dgp_select == "dgp_enders3"){

            #Enders 3
            temp_series <- dgp_enders3(periods = (freq * len),
                                      sd = params[dgp_index,"sd"],
                                      gamma = gamma)

          }else if(dgp_select == "dgp_engle"){

            #Engle
            temp_series <- dgp_engle(periods = (freq * len),
                                      sd = params[dgp_index,"sd"],
                                      gamma = gamma,
                                      theta = params[dgp_index,"theta"],
                                      alpha0 = params[dgp_index,"alpha0"],
                                      alpha1 = params[dgp_index,"alpha1"])
          }

      #Construct payload
      list(id = i,
           ur_flag = ur_flag,
           n       = len * freq,
           gamma   = gamma,
           dgp = dgp_select,
           data = stats::ts(temp_series,
                     freq = freq))
  }

  #Stop cluster
    if(run_par){
      parallel::stopCluster(cl)
    }

  #Return result
  return(data)
}
