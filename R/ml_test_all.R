#' Apply ML-based hypothesis test to a bank of time series
#'
#' @param bank List object with at least one time series
#' @param costratio A numeric cost ratio is e1/e2. Note that in the article, this is e2/e1. (Default = 1)
#' @param original Boolean indicating whether to use default model or a custom model. The default model is a gradient boost model with a sensitivity rated at 0.924 and specificity rated at 0.952. (Default = TRUE)
#' @param custom_model A hypML model object with a custom-trained model. Note that a custom model is used only when original is set to TRUE and a hypML model object is supplied. (Default = NULL)
#' @param run_par  Boolean indicating whether to compute in parallel (Default = TRUE).
#' @param num_cores  Number of logical cores to use for parallel processing. -9999 indicates maximum allowed minus one. Otherwise, provide an integer.  (Default = -9999).
#' @param fanfare Boolean indicating whether to print results (Default = FALSE).
#' @param verdicts Integer indicating whether to (1) report only ML model verdicts, (2) both ML and test statistic verdicts based on thresholds calibrated from selected cost ratio. (Default = 1)
#' @return A HypML object containing test results
#' @author Gary Cornwall and Jeffrey Chen
#' @examples ml_test_all(list(ts(rnorm(120, 10,10), freq=12)), costratio = (10/7))
#' @export
#'

#Suppress test stats verdicts

ml_test_all <- function(bank,
                        costratio = 1,
                        custom_model,
                        original = TRUE,
                        verdicts = 1,
                        run_par = TRUE,
                        num_cores = -9999,
                        fanfare = FALSE){

  #Opening statements
    if(fanfare){
      message("\n ML-BASED HYPOTHESIS TESTING FOR UNIT ROOTS \n \t \n------------------------ \n")
      message(paste0("Using ", ifelse(original, "base models", "custom model"),"."))
      message(paste0("Cost ratio to be tested is  ", costratio, "."))
      message("What is the verdict?")
    }

  #Determine which model to use
    if(original){
      test_model <- base_models
    } else {
      test_model <- custom_model
    }

  #Set up cluster
    if(run_par){
      #Set number of cores
      if(num_cores == -9999){
        num_cores <- parallel::detectCores(logical = FALSE)-1
      }

      #Set up machine
      cl <- parallel::makeCluster(num_cores)
      doParallel::registerDoParallel(cl)
      parallel::clusterExport(cl, c("scale_01",
                                    "scale_inf", "seas_decomp",
                                    "ts_measures", "ts_features"))
    }

  #Run iterations to extract time series features for scoring
    scores <- foreach::foreach(i = 1:length(bank)) %dopar% {

      #Construct payload
      data.frame(id = i,
                 ts_features(bank[[i]]))
    }

  #Create data frame
    scores <- do.call(rbind, scores)

  #Scoring
    #Create place holder
      pred_prob <- data.frame()

    #Scoring

      for(i in 1:length(test_model)){

        #Predict stat
        if(test_model[[i]]$type == "algorithm"){

          #Predict probabilities
          if(test_model[[i]]$method_name == "ranger"){
            preds <- predict(test_model[[i]]$model_obj$finalModel,
                             as.matrix(scores[,test_model[[i]]$rhs]))$predictions[,2]
          }

          #XG spits out probability of NUR, subtract it from one to get UR
          if(test_model[[i]]$method_name == "xgbTree"){
            preds <- 1 - predict(test_model[[i]]$model_obj$finalModel,
                             as.matrix(scores[, test_model[[i]]$rhs]))
          }

        } else {
          preds <- scores[,test_model[[i]]$method_name]
        }

        #Calculate threshold
          if(costratio <= max(test_model[[i]]$thresh$costratio) &
             costratio >= min(test_model[[i]]$thresh$costratio) ){

            temp <- test_model[[i]]$thresh

              if(0 %in% (temp$costratio - costratio)){
                reason <- "Using exact cost threshold"
                if(fanfare){message(reason)}
                res <- temp[temp$costratio == costratio,]
                thresh <- res$threshold[1]
                costratio.returned <- res$costratio[1]


              } else {
                reason <- "Using closest cost threshold"
                if(fanfare){message(reason)}
                res <- temp[which(abs(temp$costratio - costratio)==min(abs(temp$costratio - costratio))),]
                thresh <- res$threshold[1]
                costratio.returned <- res$costratio[1]
              }

          } else if(length(test_model[[i]]$roc_obj) > 0) {
            reason <- "Cost ratio is out of pre-calibrated range.
            Using available ROC object to calculate custom threshold."
            if(fanfare){message(reason)}

              res <- threshold_calc(test_model[[i]]$roc_obj, costratio)
              thresh <- res$threshold

              costratio.returned <- costratio


            } else {
              reason <- "Threshold is out of range and no ROC object available. Using next best threshold based on pre-calibrated thresholds."
              if(fanfare){ message(reason)}

              #Get a copy of the thresholds
              temp <- test_model[[i]]$thresh

              #Find closest match
              res <- temp[which(abs(temp$costratio - costratio) ==min(abs(temp$costratio - costratio))),]
              thresh <- res$threshold[1]
              costratio.returned <- res$costratio[1]

          }

        #Verdict
          verdict_temp <- data.frame(series_no = 1:length(bank),
                                     method = test_model[[i]]$method,
                                     costratio.requested = costratio,
                                     costratio.returned = costratio.returned,
                                     type.of.result = reason,
                                     score = preds,
                                     threshold = thresh,
                                     verdict = ifelse(preds >= thresh, "unit root", "stationary"))

        #Announce result
          if(fanfare & length(bank) == 1){
            message(paste0(test_model[[i]]$method, ": Is ", round(preds, 4),
                           " > threshold ",
                           round(thresh,4), "?  ",
                           " Verdict = ", ifelse(preds >= thresh, "Unit root", "Stationary")))
          }

        #Save result
           pred_prob <- rbind(pred_prob,
                              verdict_temp)
      }

    #Stop cluster
      if(run_par){
        parallel::stopCluster(cl)
        foreach::registerDoSEQ()
      }


  #Package the results
      #Suppress old test statistics that are scored based on cost ratio
      if(verdicts == 1){
        to_be_suppressed <- c("adf_stat", "kpss_stat", "pp_stat",
                              "pgff_stat", "breit_stat", "ers_stat_d",
                              "ers_stat_p", "urza_stat","ursp_stat")
        pred_prob_verd <- pred_prob[!pred_prob$method %in% to_be_suppressed,
                                    c("series_no", "costratio.requested",
                                      "costratio.returned", "type.of.result",
                                      "method", "verdict")]
      } else if(verdicts == 2){
        #Show all results
        pred_prob_verd <- pred_prob[, c("series_no", "costratio.requested",
                                        "costratio.returned", "type.of.result",
                                        "method", "verdict")]
      }

    #Reshape wide for final package

      #Verdicts
      verdicts <- reshape(pred_prob_verd,
                          idvar = c("series_no", "costratio.requested",
                                    "costratio.returned", "type.of.result"),
                          timevar = "method",
                          direction = "wide")

      #Underlying detail
      pred_prob <- reshape(pred_prob[, c("series_no", "costratio.requested",
                                         "costratio.returned", "method",
                                         "type.of.result","score", "threshold")],
                           idvar = c("series_no", "costratio.requested",
                                     "costratio.returned",  "type.of.result"),
                           timevar = "method",
                           direction = "wide")

      #Extract series names if they exist and overwrite
      if(!is.null(names(bank))){
        #Get names
        series_no <- names(bank)

        #Sort and relabel series_no
        verdicts <- verdicts[verdicts$series_no,]
        verdicts$series_no <- series_no

        #Sort and relabel series_no
        pred_prob <- pred_prob[pred_prob$series_no,]
        pred_prob$series_no <- series_no

      }

  #Print out message if fanfare is true
      if(fanfare){
        message(paste("Series ",
                      verdicts$series_no, ": ",
                      verdicts$verdict.xgbTree))
      }

  #Return results
  return(list(verdicts = verdicts,
              results = pred_prob,
              features = scores))
}
