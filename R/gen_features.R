#' Estimate a battery of statistical tests for unit roots and time series features
#'
#' Returns a set of commonly used time series statistical tests and time series features.
#' @return Returns a list object of time series objects with their attributes.
#' @description Returns a data frame of unit root tests and time series features
#' @param bank A list of time series objects constructed by gen_bank function
#' @param cval A critical value as a p-value
#' @param run_par Boolean indicating whether to compute in parallel.
#' @return A data frame
#' @author Gary Cornwall and Jeffrey Chen
#' @examples
#' #Create simulated time series using Enders 1
#' new_series <- gen_bank(iter = 300,
#'                        dgp_params = list(dgp = "dgp_enders1",
#'                                          periods = 100,
#'                                          sd = 1,
#'                                          alpha0 = 1,
#'                                          alpha2 =  .005,
#'                                          gamma = 1))
#' #Generate features
#' feat <- gen_features(bank = new_series)
#' @export
#'
#'


gen_features <- function(bank, cval = 0.05, run_par = TRUE) {

  require(foreach)

  #Set up cluster
  if(run_par){
    cl <- parallel::makeCluster(parallel::detectCores(logical = F)-1)
    doParallel::registerDoParallel(cl)
    parallel::clusterExport(cl, c("scale_01",
                                  "scale_inf", "seas_decomp",
                                  "ts_measures", "ts_features"))
  }

  #Run iterations
  data <- foreach::foreach(i = 1:length(bank)) %dopar% {

      #Construct payload
      data.frame(id = i,
                 dgp = bank[[i]]$dgp,
                 gamma = bank[[i]]$gamma,
                 ur_flag = ifelse(bank[[i]]$ur_flag == 1,"ur", "nur"),
                 ts_features(bank[[i]]$data))
  }

  #Stop cluster
  if(run_par){
    parallel::stopCluster(cl)
    foreach::registerDoSEQ()
  }

  #Final touches
  data <- do.call(plyr::rbind.fill, data)
  data$ur_flag <- as.factor(data$ur_flag)
  for(j in ncol(data):50){
    if(mean(is.na(data[,j])) == 1){
      data[,j] <- NULL
    }

  }

  #Keep only non-duplicated columns
    #Check for duplicates
    master <- data.frame()
    for(i in 1:ncol(data)){
      for(j in ncol(data):1){

        check <- length(which(data[,i] == data[,j]))
        master <- rbind(master,
                        data.frame(i = i, j = j,
                                   check_out = check == nrow(data)))
      }
    }

    #Keep dupe indexes
    master <- master[master$i!= master$j & master$check_out == TRUE,]
    data <- data[,-master$j[1:(nrow(master)/2)]]

  #Return result

  return(data)
}
