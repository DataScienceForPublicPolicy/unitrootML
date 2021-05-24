#' Return summary of ML-based hypothesis test
#'
#' Print summary of ML-based hypothesis test
#' @param hypML_obj A hypML object from ml_test function
#' @return Retrieve test results
#' @author Gary Cornwall and Jeffrey Chen
#' @examples
#'
#' #Create a list of time series
#' x <- list(norm1 = ts(rnorm(120, 10,10), freq=12),
#'           with_trend = ts(rnorm(120, 10,10) + 1:120, freq=12),
#'           uroot = ts(cumsum(rnorm(120,10,10)), freq = 12))
#'
#' #Screen the time series for unit roots using the default model
#' output <- ml_test(bank = x,
#'                  pvalue = 0.05)
#'
#' #Retrieve test summary
#' testing_summary(hypML_obj = output)
#'
#'
#' @export
#'



testing_summary <- function(hypML_obj){

  #Overview
  message(rep("=", 85))
  message(paste0("ML-Based Unit Root Test results requested for ",
                 nrow(hypML_obj$verdicts),
                 " series"))

  #Assumptions
  pvalue <- hypML_obj$verdicts$pvalue_requested[1]
  message(paste("User-specified p-value = ",
                hypML_obj$verdicts$pvalue_requested[1]))

  message(rep("-", 85))


  #Compare thresholds
  message(paste("\n **How does the selected p-value compare with optimal?** \n"))


  thresh <- hypML_obj$threshold
  for(i in unique(hypML_obj$threshold$method)){
    temp_subset <- thresh[thresh$method == i,]
    if(nrow(temp_subset) > 1){

      #Split out
      temp_subset$accuracy <- temp_subset$accuracy * 100
      best <-temp_subset[grep("best", temp_subset$pvalue), ]
      selected  <-temp_subset[-grep("best", temp_subset$pvalue), ]
      diff_acc <- round(best$accuracy - selected$accuracy, 2)

      #Print out
      message(paste0(i, ":"))
      message(paste0("  When p-value is selected for highest predictive performance",
                     ", accuracy = ",
                     round(best$accuracy[1], 2),
                     "%"))
      message(paste0("  When p-value is set to ",
                     selected$pvalue[1],
                     ", accuracy = ",
                     round(selected$accuracy[1], 2),
                     "%"))
      message(paste0("  Using this p-value threshold equates to a ",
                     diff_acc,
                     " p.p. loss in testing accuracy."))
    }
  }

  message(rep("-", 85))

  #Print out verdicts
    #Clean up result header
    which_mods <- grep("(verdict_ranger|verdict_xgbTree)",
                       colnames(hypML_obj$verdicts),
                       value = TRUE)

    temp <- hypML_obj$verdicts[,c("series_no",
                                  "type_of_result",
                                  which_mods)]

    colnames(temp) <- c("Series Identifier",
                        "P-Value Identified?",
                        paste0("Verdict: ",
                               gsub("verdict_", "", which_mods)))

    #Print results
    message(paste("\n **What were the verdicts?** \n"))

    message(paste0(capture.output(temp), collapse = "\n"))

  message(rep("=", 85))
}



