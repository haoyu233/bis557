#' ridge regression
#' @description This is a function called ridge_regression, which does ridge regression that takes into account colinear regreesion variables.
#'
#' @param form A regression formula you want to use.
#' @param data A dataset you want to do ridge regression with.
#' @param lambda A number you want to specify for lambda in ridge regression, default is lambda = 0.
#'
#' @return A list containing the coefficients fo ridge regression, regression type and so on.
#'
#' @export
#'
#' @examples
#' ridge_regression(Sepal.Length~.,iris,lambda=0.01)

ridge_regression <- function(form, data, lambda = 0) {
  rownames(data) <- NULL
  X <- model.matrix(form, data)
  Y <- data[[as.character(form)[2]]][as.numeric(rownames(X))]
  ret <- solve( crossprod(X) + diag(rep(lambda, ncol(X))) ) %*% t(X) %*% Y
  attributes(ret)$formula <- form
  class(ret) <- c(class(ret), "ridge_regression")
  ret
}

###########predict.ridge_regression###################
#' predict.ridge_regression
#'
#' @description This is a function called predict.ridge_regression,
#' which predict values based on ridge regression model object.
#'
#' @param object Object of class inheriting from 'ridge_regression'.
#' @param ... Further arguments passed to or from other methods.
#'
#' @return A vector of predictions or a matrix of predictions.
#'
#' @export
#'

predict.ridge_regression <- function(object, ...) {
  dots <- list(...)
  x_frame <- dots[[1]]
  if (!is.data.frame(x_frame)) {
    stop(red("The first argument should be a data.frame of values",
             "to predict"))
  }
  X <- model.matrix(attributes(object)$formula, x_frame)
  X %*% object
}



##################3.opt_lambda##################
#' opt_lambda
#'
#' @description This is a function called opt_lambda,
#' which optimize the ridge regression parameter lambda by spliting data into test and trian subset
#' and estamate the rmse for test data based on the ridge regression results on train data with selected lambda.
#' The lambda with least rmse is the optimal.
#'
#' @param lambdas A numeric vector containing all the candidates for lambda.
#' @param data The data set to do ridge regression with.
#' @param form A regression formula you want to use.
#'
#' @return A number indicating the optimal lambda.
#'
#' @export
#'
#' @examples
#' opt_lambda(lambdas=seq(0, 0.9, 0.005),iris,Sepal.Length~.)

opt_lambda<-function(lambdas,data,form){
  library(foreach)
  #split data into half test and half train
  set.seed(1110)
  test <- sample.int(nrow(data),nrow(data)/2)
  train <- dplyr::setdiff(seq_len(nrow(data)), test)

  #get X and Y from the original dataset
  rownames(data) <- NULL
  X <- model.matrix(form, data)
  Y <- data[[as.character(form)[2]]][as.numeric(rownames(X))]

  #calculate rmse of test data based on each given lambda

  rmse <- foreach::foreach(lambda = lambdas, .combine = c) %dopar% {
    casl::casl_util_rmse(Y[test],
                   predict(ridge_regression(form, data[train,], lambda = lambda),
                           data[test,]))
  }
 lambdas[which.min(rmse)]
}
