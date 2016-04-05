#' Compute Negative Log Likelihood
#'
#' Bernoulli likelihood: x = 0, 1; f(x,theta) = (theta^x)*(1-theta)^(1-x)
#' Bernoulli log-likelihood: $$ ln(L) = sum_{i=1}^I sum_{t=1}^T  D_i^C(t)  *  ln(P_{i}^{C} (t)) + (1 - D_i^C(t)) * ln(1 - P_{i}^{C} (t)) $$
#'
#' @param prediction Numeric vector same length as actual
#' @param actual Numeric vector same length as prediction
#'
#' @return Numeric vector length one
#'
#' @examples
#' compute_log_lik(runif(10), sample(c(1,0), 10, replace=TRUE))
#'
#' @export
compute_log_lik <- function(prediction, actual){
  stopifnot(length(prediction)==length(actual))

  prediction[prediction == 0] <- 1e-05 # bc log(0) == -Inf

  # Treating predictions outside of probability range as NA:
  prediction[prediction < 0 | prediction > 1] <- NA
  #missings <- sum(is.na(prediction))

  if(anyNA(prediction)){
    # Must throw away the whole thing if anyNA b/c cannot compare population of predictions of different length
    # based on log likelihood b/c it is not scaled to size of prediction vector like accuracy is.
    log.likelihood <- -Inf
  } else {

    #     if(missings > 0) {
    #       actual <- actual[complete.cases(prediction)]
    #       prediction <- prediction[complete.cases(prediction)]
    #       stopifnot(length(prediction)==length(actual))
    #       warning(paste0("Dropped ", missings,
    #                      " elements of the prediction and actual vecs bc there were that many missing vals in the predictions."))
    #     }

    log.likelihood <- 0
    for (i in seq(length(prediction))) {
      p <- prediction[i]

      #       if(p <= 0 || p >= 1) {
      #         # Bad:
      #         log.likelihood <- -Inf #p <- 1e-05; p <- 0.99999
      #       } else {
      log.likelihood <- log.likelihood +
        base::log(ifelse(actual[i] == 1, p, 1 - p))
      # }

    }

  }

  - log.likelihood
}

#' Compute Identity
#'
#' Best is -1, worst is 0. Objective is to minimize.
#'
#' @param prediction Numeric vector same length as actual
#' @param actual Numeric vector same length as prediction
#'
#' @return Numeric vector length one
#'
#' @examples
#' compute_identity(1:10, 1:10)
#' compute_identity(1:10, c(1:9, 1))
#' compute_identity(c("hey", "hello"), c("hey", "he"))
#' compute_identity(runif(10), sample(c(1,0), 10, replace=TRUE))
#'
#' @export
compute_identity <- function(prediction, actual){
  # objective is to minimize
  stopifnot(length(prediction)==length(actual))

  len <- length(prediction)

  missings <- sum(is.na(prediction))
  if(missings > 0) {
    actual <- actual[complete.cases(prediction)]
    prediction <- prediction[complete.cases(prediction)]
    stopifnot(length(prediction)==length(actual))
    warning(paste0("Dropped ", missings,
                   " elements of the prediction and actual vecs bc there were that many missing vals in the predictions."))
    # Dividing by original length of longer vector implicitly treats all the missings as wrongly labeled
    return(sum(ifelse(prediction == actual, 1, 0))/len)
  } else{
    stopifnot(!anyNA(prediction))
    return(- mean(ifelse(prediction == actual, 1, 0))) # best is -1, worst is 0
  }
}

#' Compute Identity Multi Class
#'
#' Best is -1, worst is 0. Objective is to minimize.
#'
#' @param prediction Numeric matrix same number of rows as length of actual
#' @param actual Numeric vector same length as prediction
#'
#' @return Numeric vector length one
#'
#' @examples
#' compute_identity_multi_class(matrix(1:100, nrow=10), 1:10)
#' compute_identity_multi_class(matrix(rep(1:2, 10), nrow=10), rep(2,10))
#'
#' @export
compute_identity_multi_class <- function(prediction, actual){
  stopifnot(nrow(prediction)==length(actual))

  out <- 0
  for (i in seq(nrow(prediction))){
    out <- out + ifelse(which(prediction[i, ] == max(prediction[i, ])) == actual[i], 1, 0)
  }

  - mean(out)/nrow(prediction) # best is -1, worst is 0
}

#' Compute Root Mean Squared Error
#'
#' @param prediction Numeric vector same length as actual
#' @param actual Numeric vector same length as prediction
#'
#' @return Numeric vector length one
#'
#' @examples
#' compute_rmse(1:10, 1:10)
#' compute_rmse(1:10, c(1:9, 1))
#' compute_rmse(rnorm(10), rnorm(10))
#'
#' @export
compute_rmse <- function(prediction, actual) {
  stopifnot(length(prediction)==length(actual))

  missings <- sum(is.na(prediction))

  if(missings >= length(prediction)){
    res <- Inf
  } else {
    if(missings > 0) {
      actual <- actual[complete.cases(prediction)]
      prediction <- prediction[complete.cases(prediction)]
      stopifnot(length(prediction)==length(actual))
      warning(paste0("Dropped ", missings,
                     " elements of the prediction and actual vecs bc there were that many missing vals in the predictions."))
    }
    res <- sqrt(mean((prediction - actual)^2, na.rm = TRUE))
  }
  res
}

#' Pick one of the loss functions from the package.
#'
#' @param loss Character vector length one specifying which loss function you
#'   want.
#'
#' @return Returns a function that has two arguments \code{prediction} (Numeric
#'   vector same length as actual), and \code{actual} (Numeric vector same
#'   length as prediction), and returns a Numeric vector length one.
#'
#' @examples
#' create_loss_func("log_lik")
#'
#' @export
create_loss_func <- function(loss){
  switch(loss,
         identity = compute_identity,
         identity_multi_class = compute_identity_multi_class,
         log_lik = compute_log_lik,
         rmse = compute_rmse
  )
}
