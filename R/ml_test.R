#' Apply ML-based hypothesis test to a bank of time series
#'
#' @param bank List object with at least one time series
#' @param pvalue A numeric value indicating the desired p-value. (Default = 0.05) To retrieve the best accuracy, enter "-9999".
#' @param original Boolean indicating whether to use default model or a custom model. The default model is a gradient boost model with a sensitivity rated at 0.924 and specificity rated at 0.952. (Default = TRUE)
#' @param custom_model A unitrootML model object with a custom-trained model. Note that a custom model is used only when original is set to TRUE and a unitrootML model object is supplied. (Default = NULL)
#' @param run_par  Boolean indicating whether to compute in parallel (Default = TRUE).
#' @param num_cores  Number of logical cores to use for parallel processing. -9999 indicates maximum allowed minus one. Otherwise, provide an integer.  (Default = -9999).
#' @param fanfare Boolean indicating whether to print results (Default = FALSE).
#' @param verdicts Integer indicating whether to (1) report only ML model verdicts, (2) both ML and test statistic verdicts based on thresholds calibrated from selected cost ratio. (Default = 1)
#' @return A unitrootML object containing test results
#' @author Gary Cornwall and Jeffrey Chen
#' @examples ml_test(list(ts(rnorm(120, 10,10), freq=12)), pvalue = 0.05)
#' @export
#'

ml_test <- function(bank,
                        pvalue = 0.05,
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
      message(paste0("P-value to be tested is  ", pvalue, "."))
      message("What is the verdict?")
    }

  #Set connectors
    `%dopar%` <- foreach::`%dopar%`
    `%>%` <- magrittr::`%>%`

  #Determine which model to use
    if(original){
      #load("R/sysdata.Rda")
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
      temp <- data.frame(id = i,
                 ts_features(bank[[i]]))
      return(temp)
    }

  #Create data frame
    scores <- do.call(rbind, scores)

  #Scoring
    #Create place holder
      pred_prob <- data.frame()
      best_thesholds <- data.frame()

    #Scoring

      for(i in 1:length(test_model)){

        #Predict stat
        if(test_model[[i]]$type == "algorithm"){

          #Predict probabilities
          if(test_model[[i]]$method_name == "ranger"){

            #First, get predictions
            preds <- predict(test_model[[i]]$model_obj$finalModel,
                             as.matrix(scores[,test_model[[i]]$rhs]))$predictions[,2]


            #Second, get what would be best
            best_thesholds <- rbind(best_thesholds,
                                    data.frame(method = names(test_model)[i],
                                               test_model[[i]]$thresh %>%
                              dplyr::filter(pvalue == -9999)  %>%
                              dplyr::mutate(pvalue = "best")))

          }

          #XG spits out probability of NUR, subtract it from one to get UR
          if(test_model[[i]]$method_name == "xgbTree"){

            #First, get predictions
              preds <- 1 - predict(test_model[[i]]$model_obj$finalModel,
                               as.matrix(scores[, test_model[[i]]$rhs]))


            #Second, get what would be best
              best_thesholds <- rbind(best_thesholds,
                                      data.frame(method = names(test_model)[i],
                                                 test_model[[i]]$thresh %>%
                                dplyr::filter(pvalue == -9999)  %>%
                                dplyr::mutate(pvalue = "best")))

          }


        } else {
          preds <- scores[,test_model[[i]]$method_name]
        }

        #Calculate threshold
          if(pvalue <= max(test_model[[i]]$thresh$pvalue) &
             pvalue >= min(test_model[[i]]$thresh$pvalue) ){

            temp <- test_model[[i]]$thresh

              if(0 %in% (temp$pvalue - pvalue)){
                reason <- "Using exact p-value threshold"
                if(fanfare){message(reason)}

                #get exact match
                res <- temp[temp$pvalue == pvalue,]
                thresh <- res$threshold[1]
                pvalue_returned <- res$pvalue[1]

                #Save thresholds
                  #First, get what was requested
                    best_thesholds <- rbind(best_thesholds,
                                        data.frame(method = names(test_model)[i],
                                                   res[1,]  %>%
                                       dplyr::mutate(pvalue = as.character(pvalue_returned))))



              } else {
                reason <- "Using closest p-value threshold"
                if(fanfare){message(reason)}

                res <- temp[which(abs(temp$pvalue - pvalue) == min(abs(temp$pvalue - pvalue))),]
                thresh <- res$threshold[1]
                pvalue_returned <- res$pvalue[1]

                #Save thresholds
                  #First, get what was requested
                  best_thesholds <- rbind(best_thesholds,
                                          data.frame(method = names(test_model)[i],
                                                     res[1,]  %>%
                                     dplyr::mutate(pvalue = as.character(pvalue_returned))))


              }

          } else if(length(test_model[[i]]$roc_obj) > 0) {
            reason <- "P-value is out of pre-calibrated range.
            Using available ROC object to calculate custom threshold."
            if(fanfare){message(reason)}

            #Get metrics
              res <- threshold_calc(test_model[[i]]$roc_obj, pvalue)
              thresh <- res$threshold
              pvalue_returned <- pvalue

            #Store results
              best_thesholds <- rbind(best_thesholds,
                                      data.frame(method = names(test_model)[i],
                                                 res[1,] %>%
                                 dplyr::mutate(pvalue = as.character(pvalue_returned))))


            } else {
              reason <- "Threshold is out of range and no ROC object available. Using next best threshold based on pre-calibrated thresholds."
              if(fanfare){ message(reason)}

              #Get a copy of the thresholds
              temp <- test_model[[i]]$thresh

              #Find closest match
              res <- temp[which(abs(temp$pvalue - pvalue) == min(abs(temp$pvalue - pvalue))),]
              thresh <- res$threshold[1]
              pvalue_returned <- res$pvalue[1]

              #Save out closest threshold
              best_thesholds <- rbind(best_thesholds,
                                      data.frame(method = names(test_model)[i],
                                                 res[1,]  %>%
                                 dplyr::mutate(pvalue = as.character(pvalue_returned))))



          }

        #Verdict
          verdict_temp <- data.frame(series_no = 1:length(bank),
                                     method = test_model[[i]]$method,
                                     pvalue_requested = pvalue,
                                     pvalue_returned = pvalue_returned,
                                     type_of_result = reason,
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
                                    c("series_no", "pvalue_requested",
                                      "pvalue_returned", "type_of_result",
                                      "method", "verdict")]
      } else if(verdicts == 2){
        #Show all results
        pred_prob_verd <- pred_prob[, c("series_no", "pvalue_requested",
                                        "pvalue_returned", "type_of_result",
                                        "method", "verdict")]
      }

    #Reshape wide for final package

      #Verdicts
      verdicts <- reshape(pred_prob_verd,
                          idvar = c("series_no", "pvalue_requested",
                                    "pvalue_returned", "type_of_result"),
                          timevar = "method",
                          direction = "wide")
      colnames(verdicts) <- gsub("\\.", "_", colnames(verdicts))

      #Underlying detail
      pred_prob <- reshape(pred_prob[, c("series_no", "pvalue_requested",
                                         "pvalue_returned", "method",
                                         "type_of_result","score", "threshold")],
                           idvar = c("series_no", "pvalue_requested",
                                     "pvalue_returned",  "type_of_result"),
                           timevar = "method",
                           direction = "wide")
      colnames(pred_prob) <- gsub("\\.", "_", colnames(pred_prob))

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
                      verdicts$verdict_xgbTree))
      }



  #Return results
  return(list(verdicts = verdicts,
              threshold = best_thesholds,
              results = pred_prob,
              features = scores))
}
