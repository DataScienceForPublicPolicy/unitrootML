#' Calculate threshold given cost ratios
#'
#' Returns a list of model objects with accuracies and ROC coordinates based on test set. This is a support function that is not intended for the end user to directly use.
#' @param roc_obj A training set outputted from the gen_features function
#' @param pvalue A desired level of type I errors, default is 0.05. To get the best accuracy, enter "-9999".
#' @return A list object
#' @author Gary Cornwall and Jeffrey Chen
#' @examples
#' \dontrun{
#' threshold_calc(roc_obj, pvalue)
#' }
#' @export
#'



threshold_calc <- function(roc_obj, pvalue = 0.05){

  #Split logic
  if(pvalue > 0){
    #P-values
    #Get coordinates of the ROC curve
    out <- pROC::coords(roc_obj,
                        ret = c('threshold','sensitivity','specificity', 'tp', 'tn', 'fp','fn', "accuracy",
                                "tpr", "fpr", "ppv", "npv"),
                        transpose = F)

    #Find thresholds that are closest to 1-pvalue
    dist_spec <- (out$specificity - (1-pvalue))^2
    idx <- which(dist_spec == min(dist_spec))
    out <- out[idx[1],]
  } else {

    #Best accuracy
    out <- pROC::coords(roc_obj,
                 'best',
                 ret = c('threshold','sensitivity','specificity', 'tp', 'tn', 'fp','fn', "accuracy",
                         "tpr", "fpr", "ppv", "npv"),
                 transpose = F)

  }


  #Return results
  return(cbind(pvalue = pvalue, out))
}


#OLD BELOW
# threshold_calc <- function(roc_obj, costratio){
#
#   out <- pROC::coords(roc_obj,
#                'best',
#                best.weights = c(costratio, 0.5),
#                ret = c('threshold','tp', 'tn', 'fp','fn', "accuracy",
#                        "tpr", "fpr", "ppv", "npv"),
#                transpose = TRUE)
#   #Return results
#   return(cbind(costratio = costratio, t(out)))
# }
