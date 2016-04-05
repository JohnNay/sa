model_stats <- function(x, y, on.ranks) {
  # The calculation of R^2 component of this function is adapted from a helper function associated with:
  # J. C. Thiele, W. Kurth, V. Grimm, Facilitating Parameter Estimation and Sensitivity Analysis of Agent-Based Models: 
  # A Cookbook Using NetLogo and R. Journal of Artificial Societies and Social Simulation. 17, 11 (2014).
  data <- data.frame(Y = y, x)
  if (on.ranks) {
    for (i in seq(ncol(data))) {
      data[ ,i] <- rank(data[ ,i])
    }
  }
  lm.Y <- stats::lm(formula(paste(colnames(data)[1], "~", paste(colnames(data)[-1], collapse = "+"))), 
                    data = data)
  r.squared <- summary(lm.Y)$r.squared
  rmse <- compute_rmse(y, stats::predict.lm(object = lm.Y, newdata = x))
  list(r.squared=r.squared, rmse=rmse)
}

#'Partial Correlation Analysis of a Simulation Model
#'
#'\code{pc_sa} conducts a partial correlation analysis.
#'
#'This is function of the \strong{eat} package. It takes a simulation model in function form
#'and a list of input values. Helper function for extracting R-sqaured is
#'adapted from Thiele et al. (2014).
#'
#'@param abm A function that takes each of the \code{input_values} as arguments.
#'@param input_values List
#'@param out Optional Character vector length one to be passed an argument to 
#'  the \code{abm} function to specify what outcome measure to use.
#'@param sample_count  Optional Numeric vector length one. Default is 100.
#'@param constraints Optional Character vector that is either "none" or is using
#'  only variable names that are specified in the input_values List argument. 
#'  This character vector is evaluated in an environment created for the sampled
#'  data on the variables, and its evaluation results in a Logical vector that 
#'  that subsets sampled.
#'@param nboot Optional Numeric vector length one. Default is 1000.
#'@param previous_pc_sa Optional list where each element is an S4 object created
#'  by using this function to previously run this analysis. The main use case is
#'  when the previous analysis shows that we need to run more simulations to
#'  reduce the uncertainty in the estimates. If this is provided, the simulation
#'  outputs and the design matrix of parameter inputs will be addde to the
#'  outputs and inputs used by the current call of the function and then the
#'  sensitivity analysis will be computed with the previous and current results.
#'@param model_data Optional data.frame with the data that was used to build the
#'  model. This is used if one wants to ensure that all parameters tested are in
#'  the convex hull of the data used to build the model that is now being 
#'  analyzed. This uses the WhatIf package in the Suggest field of the eat 
#'  description file.
#'@param iterations Optional numeric vector length one.
#'@param parallel Optional logical vector length one. Default is FALSE.
#'@param cores Optional Numeric vector length one. Default is 
#'  parallel::detectCores().
#'@param verbose Optional logical vector.
#'@param extra_verbose Optional logical vector, default is FALSE.
#'@param rank Optional logical vector.
#'@param method Optional character vector that is either "src" (Standardized 
#'  Regression Coefficient) or "pcc" (Partial Correlation Coefficient)
#'  
#'@return Returns a sensitivity object that can be plotted by functions.
#'  
#' @examples
#' # Unconstrained Analysis
#' fake_abm <- function(params, out) {
#'   x1 <- params[1]
#'   x2 <- params[2]
#'   if (out=="sq") return(x1^2 + x2 + rnorm(1, 0))
#'   if (out=="ident") return(x1 + x2 + rnorm(1, 0))
#' }
#' inputs <- lapply(list(param1 = NA, param2 = NA), 
#'                  function(x) list(random_function = "qunif",
#'                                   ARGS = list(min = 0, max = 1)))
#' pc_sa(fake_abm, inputs, "sq")
#' pc_sa(fake_abm, inputs, "sq", method = "pcc", rank = FALSE)
#' res <- pc_sa(fake_abm, inputs, "sq", method = "src")
#' plot(res); res@@r_squared
#' 
#' # Constrained Analysis
#' fake_abm <- function(params, out) {
#'   x1 <- params[1]
#'   x2 <- params[2]
#'   if (out=="sq") return(x1^2 + x2 + rnorm(1, 0))
#'   if (out=="ident") return(x1 + x2 + rnorm(1, 0))
#' }
#' inputs <- lapply(list(param1 = NA, param2 = NA), 
#'                  function(x) list(random_function = "qunif",
#'                                   ARGS = list(min = 0, max = 1)))
#' res <- pc_sa(fake_abm, inputs, "sq", constraints = "param1 > 0.1 & param2 < 0.9")
#' 
#'@references G. Pujol et al., Sensitivity: Sensitivity Analysis (2014), 
#'  (available at
#'  http://cran.r-project.org/web/packages/sensitivity/index.html).
#'  
#'  A. Saltelli, K. Chan, E. M. Scott, Sensitivity Analysis (Wiley, Chichester, 
#'  2009).
#'  
#'  J. C. Thiele, W. Kurth, V. Grimm, Facilitating Parameter Estimation and 
#'  Sensitivity Analysis of Agent-Based Models: A Cookbook Using NetLogo and R. 
#'  Journal of Artificial Societies and Social Simulation. 17, 11 (2014).
#'  
#'@export

pc_sa <- function(abm, 
                  input_values,
                  out = "action_avg", 
                  sample_count = 100,
                  constraints = "none",
                  nboot = 1000,
                  previous_pc_sa = NULL,
                  model_data = NULL,
                  iterations = NULL,
                  parallel = FALSE,
                  cores = NULL,
                  verbose = TRUE, extra_verbose = FALSE,
                  rank = TRUE,
                  method = c("src", "pcc")){
  
  # Get names of input factors:
  input_names <- names(input_values)
  
  method <- match.arg(method)
  start_time <- as.numeric(proc.time()[[1]])
  call <- match.call()
  
  if(!is.null(previous_pc_sa)){
    input.set1 <- lapply(previous_pc_sa, function(x) x@input_set)
    pc_sim1 <- lapply(previous_pc_sa, function(x) x@sims)
    stopifnot(all(sapply(input.set1, function(x) input_names==colnames(x))))
  }
  
  if (parallel) {
    if (is.null(cores)) cores <- parallel::detectCores() - 1
    doParallel::registerDoParallel(cores = cores)
    forloop <- foreach::`%dopar%`
  } else{
    forloop <- foreach::`%do%`
  }
  
  # Create samples, removing samples violating constraints, until you have enough:
  input.set <- create_set(input_values, input_names, sample_count, constraints, model_data)
  if(verbose) cat("Done with input set creation.\n")
  if(extra_verbose) print(input.set)
  
  ##################################################
  # Simulation runs with generated input factor sets:
  if(verbose) cat("Starting simulations.\n")
  # simulation results for input factor sets (as matrix)
  i <- NULL # http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
  if (is.null(iterations)){
    pc_sim <- forloop(foreach::foreach(i=seq(nrow(input.set)), .combine='c'), {
      tryCatch(abm(as.numeric(input.set[i, ]), out = out), error = function(e) NA)
    })
  } else {
    pc_sim <- forloop(foreach::foreach(i=seq(nrow(input.set)), .combine='c'), {
      tryCatch(abm(as.numeric(input.set[i, ]), out = out, iterations = iterations), error = function(e) NA)
    })
  }
  if(verbose) cat("Done with simulations.\n")
  if(extra_verbose) print(pc_sim)
  
#   # testing code for using a previous object:
#   previous_pc_sa <- list(list(input_set = data.frame(a=c(1,2), b=c(1,2)), sims=  c(1,2)), 
#                          list(input_set = data.frame(a=c(3,4), b=c(3,4)), sims=  c(3,4)),
#                          list(input_set = data.frame(a=c(5,6), b=c(5,6)), sims=  c(5,6)))
#   #previous_pc_sa <- list(list(input_set = data.frame(a=c(1,2), b=c(1,2)), sims=  c(1,2)))
#   input.set1 <- lapply(previous_pc_sa, function(x) x$input_set)
#   pc_sim1 <- lapply(previous_pc_sa, function(x) x$sims)
#   stopifnot(all(sapply(input.set1, function(x) c("a","b")==colnames(x))))
  
  if(!is.null(previous_pc_sa)){
    # combine all elements in the list if list has more than one element
    input.set1 <- as.data.frame(do.call(rbind, input.set1))
    pc_sim1 <- as.numeric(do.call(c, pc_sim1))
    # add to the current results
    input.set <- rbind(input.set1, input.set)
    pc_sim <- c(pc_sim1, pc_sim)
  }
  
  if(anyNA(pc_sim)){
    warning(paste("You had", sum(is.na(pc_sim)), "NA's in your output of the simulation model."))
  } 
  to_keep <- complete.cases(pc_sim)
  
  if(length(pc_sim) != nrow(input.set)){
    warning("Could not return a sensitivity analysis result because length(simulated outcomes) != nrow(input set)")
    # If we hadnt added this conditional, we would have errored out.
    result <- NA
    r_squared <- "No results."
  } else {
    if (method == "src"){
      result <- sensitivity::src(X = input.set[to_keep, ], y = pc_sim[to_keep], nboot = nboot, rank = rank)
      model_stat <- model_stats(x = input.set[to_keep, ], y = pc_sim[to_keep], 
                               on.ranks = rank)
      r_squared <- model_stat$r.squared
      rmse <- model_stat$rmse
    }
    
    if (method == "pcc"){
      result <- sensitivity::pcc(X = input.set[to_keep, ], y = pc_sim[to_keep], nboot = nboot, rank = rank)
      r_squared <- "Not relevant to this method. Only relevant to the 'src' method."
      rmse <- "Not relevant to this method. Only relevant to the 'src' method."
    }
  }
  
  if(extra_verbose) print(result)
  
  new("pcSA",
      call = call,
      input_set = input.set,
      sims = pc_sim,
      result = result, 
      r_squared = r_squared,
      rmse = rmse,
      timing = as.numeric(proc.time()[[1]]) - start_time,
      session = sessionInfo())
}
