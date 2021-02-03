#' Construct ML models for use as a ML test
#'
#' Returns a list of model objects with accuracies and ROC coordinates based on test set
#' @param train_set A training set outputted from gen_features function
#' @param use_case A string indicating if cross validation should be applied ('cv') or full sample should be used to train ('full'). If 'cv' is specified, then cv_folds should be specified in the model_params argument.
#' @param val_set A validation set outputted from gen_features function to be used to calibrate threshold
#' @param costratios A vector of cost ratios. This will be used to identify optimal decision threshold that satisfies the preference Type I error relative to Type II error.  (Default = seq(0.1, 10, 0.1))
#' @param model_params A list of parameters for specifying models. Multiple methods allowed. Requires a 'method' tag to specify algorithm, 'cv_folds' to indicate number of folds for cross validation, then data frame of hyperparameters for inclusion in tune_grid.
#' @return A list object
#' @author Gary Cornwall and Jeffrey Chen
#' @examples gen_test(train_set, val_set, model_params)
#' @export
#'

gen_test <- function(train_set,
                     val_set,
                     use_case = "cv",
                     costratios = seq(0.1, 10, 0.1),
                     model_params = list(list(method = "ranger",
                                              cv_folds = 5,
                                              tune_grid = expand.grid(mtry = c(3, 9, 27),
                                                                      splitrule = "gini",
                                                                      min.node.size = c(2, 4, 8))),
                                         list(method = "xgbTree",
                                              cv_folds = 5,
                                              tune_grid = expand.grid(nrounds = 300,
                                                                      eta = c(0.01, 0.03, 0.1, 0.3, 0.5),
                                                                      gamma = 0,
                                                                      colsample_bytree = c(0.8, 1),
                                                                      min_child_weight = 1,
                                                                      subsample = c(0.8, 1),
                                                                      max_depth = c(4, 6))))
                     ){

  #Set object type
    hypmod <- list()
    class(hypmod) <- "hypML-models"

  #Check number of models
    if(sum(names(model_params) %in% "method") == 1){
      mod_index <- 1
      model_params <- list(model_params)
    } else  {
      mod_index <- length(model_params)
    }

   #Check number of models
    if(use_case == "full"){
      use_case <- "none"
    }


  #Set base levels
    train_feat$ur_flag <- relevel(train_feat$ur_flag, ref = "nur")
    val_feat$ur_flag <- relevel(val_feat$ur_flag, ref = "nur")


  #Train classifiers
    for(iter in 1:mod_index){
      #Message
      message(paste("Training", model_params[[iter]]$method))

      #Train model
      mod <- caret::train(ur_flag ~ .,
                   data = train_set[,-c(1:3)],
                   method = model_params[[iter]]$method,
                   trControl = trainControl(method = use_case,
                                            number = model_params[[iter]]$cv_folds,
                                            classProbs = TRUE),
                   tuneGrid = model_params[[iter]]$tune_grid)

      #Obtain test performance
        yhat <- caret::predict.train(mod, val_set, type = "prob")[,2]
        roc_obj <- pROC::roc(1 * (val_set$ur_flag == "ur") ~ yhat)

        #Calculate decision threshold for different cost ratios
        precomp_thresh <- data.frame()
        for(i in costratios){
          precomp_thresh <- rbind(precomp_thresh, threshold_calc(roc_obj, i))
        }

        conf_mat <- table(val_set$ur_flag, caret::predict.train(mod, val_set))
        acc <- (conf_mat[1,1] + conf_mat[2,2]) / nrow(val_set)

      #Store object
      message(paste("\t Storing model object", model_params[[iter]]$method))
      message(paste("\t Top CV performance: ", round(acc, 3)))
      hypmod[[model_params[[iter]]$method]] <- list(method_name = model_params[[iter]]$method,
                                                    type = "algorithm",
                                                    model_obj = mod,
                                                    rhs = names(attr(mod$terms,"dataClasses"))[-1],
                                                    roc_obj = roc_obj,
                                                    thresh = precomp_thresh,
                                                    acc = acc)

    }

  #Add coordinates for standard test statistics
    ur_tests <- c("adf_stat", "kpss_stat", "pp_stat",
                  "pgff_stat", "breit_stat", "ers_stat_d",
                  "ers_stat_p", "urza_stat", "ursp_stat")

    for(iter in ur_tests){
      #Create ROC
      roc_obj <- pROC::roc(1 * (val_set$ur_flag == "ur") ~ val_set[,iter])

      #Set up precomputed thresholds
      precomp_thresh <- data.frame()
      for(i in costratios){
        precomp_thresh <- rbind(precomp_thresh, threshold_calc(roc_obj, i))
      }

      hypmod[[iter]] <- list(method_name = iter,
                             type = "test statistic",
                             roc_obj = roc_obj,
                             thresh = precomp_thresh)

    }

  #Return results
  return(hypmod)
}
