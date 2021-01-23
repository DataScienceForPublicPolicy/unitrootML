#' Calculate threshold given cost ratios
#'
#' Returns a list of model objects with accuracies and ROC coordinates based on test set
#' @param roc_obj A training set outputted from the gen_features function
#' @param costratio A ratio indicating the importance of Type I errors (False Positive) relative to Type II errors (False Negative). A value of 2 would indicate Type I errors are twice as costly as Type II errors.
#' @return A list object
#' @author Gary Cornwall and Jeffrey Chen
#' @examples threshold_calc(roc_obj, costratio)
#' @export
#'

threshold_calc <- function(roc_obj, costratio){

  out <- pROC::coords(roc_obj,
               'best',
               best.weights = c(costratio, 0.5),
               ret = c('threshold','tp', 'tn', 'fp','fn', "accuracy",
                       "tpr", "fpr", "ppv", "npv"),
               transpose = TRUE)
  #Return results
  return(cbind(costratio = costratio, t(out)))
}
