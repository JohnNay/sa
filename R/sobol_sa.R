#'Conduct a Sobol Sensitivity Analysis of a Simulation Model
#'
#'\code{sobol_sa} conducts a global variance decomposition.
#'
#'This is function of the \strong{eat} package. It takes an abm in function form
#'and a list of input values.
#'
#'
#'@param abm A function that takes as input values for each of the 
#'  \code{input_values}
#'@param input_values List
#'@param out Optional Character vector length one to be passed an argument to the 
#'  \code{abm} function to specify what outcome measure to use.
#'@param sample_count  Optional Numeric vector length one. Default is 100.
#'@param constraints Optional Character vector that is either "none" or is using
#'  only variable names that are specified in the input_values List argument. 
#'  This character vector is evaluated in an environment created for the sampled
#'  data on the variables, and its evaluation results in a Logical vector that 
#'  that subsets sampled.
#'@param sobol_nboot Optional Numeric vector length one. Default is 1000.
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
#'  
#'@return Returns an S4 object that can be plotted by its plot method.
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
#' sobol_sa(fake_abm, inputs, "sq")
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
#' sobol_sa(fake_abm, inputs, "sq", constraints = "param1 > 0.1 & param2 < 0.9")
#' 
#'@references J. C. Thiele, W. Kurth, V. Grimm, Facilitating Parameter
#'Estimation and Sensitivity Analysis of Agent-Based Models: A Cookbook Using
#'NetLogo and R. Journal of Artificial Societies and Social Simulation. 17, 11
#'(2014).
#'
#'G. Pujol et al., Sensitivity: Sensitivity Analysis (2014), (available at
#'http://cran.r-project.org/web/packages/sensitivity/index.html).
#'
#'I.M. Sobol, S. Tarantola, D. Gatelli, S.S. Kucherenko and W. Mauntz, 2007,
#'Estimating the approximation errors when fixing unessential factors in global
#'sensitivity analysis, Reliability Engineering and System Safety, 92, 957-960.
#'
#'A. Saltelli, P. Annoni, I. Azzini, F. Campolongo, M. Ratto and S. Tarantola,
#'2010, Variance based sensitivity analysis of model output. Design and
#'estimator for the total sensitivity index, Computer Physics Communications
#'181, 259-270.
#'
#'@export

sobol_sa <- function(abm, 
                     input_values,
                     out = "action_avg", 
                     sample_count = 100,
                     constraints = "none",
                     sobol_nboot = 1000, 
                     model_data = NULL,
                     iterations = NULL,
                     parallel = FALSE,
                     cores = NULL,
                     verbose = TRUE, extra_verbose = FALSE){
  
  # Get names of input factors:
  input_names <- names(input_values)
  
  start_time <- as.numeric(proc.time()[[1]])
  call <- match.call()
  
  if(parallel==TRUE & is.null(cores)) cores <- parallel::detectCores()
  
  # Create two samples, removing samples violating constraints, until you have enough:
  input.sets.1 <- create_set(input_values, input_names, sample_count, constraints, model_data)
  if(verbose) cat("Done with input set", 1,"\n")
  input.sets.2 <- create_set(input_values, input_names, sample_count, constraints, model_data)
  if(verbose) cat("Done with input set", 2,"\n")
  
  # Make sets the same size:
  rows <- min(nrow(input.sets.1), nrow(input.sets.2))
  input.sets.1  <- input.sets.1[seq(rows), ]
  input.sets.2  <- input.sets.2[seq(rows), ]
  stopifnot(nrow(input.sets.1) == nrow(input.sets.2) & nrow(input.sets.2) > 0)
  if(verbose) cat("Done making them same size \n")
  if(extra_verbose) print(input.sets.1); print(input.sets.2)
  
  ##################################################
  # Simulation runs with generated input factor sets:
  # Create instance of sobol class:
  sobol_aggregate <- sensitivity::sobol2007(model = NULL, 
                                            X1 = input.sets.1, X2 = input.sets.2, 
                                            nboot = sobol_nboot)
  if(verbose) cat("Starting simulations \n")
  # simulation results for input factor sets (as matrix)
  if (parallel) {
    doParallel::registerDoParallel(cores = cores)
  } # without registering the backend the %dopar% should just run as %do%
  i <- NULL # http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
  if (is.null(iterations)){
    sobol_sim <- foreach::`%dopar%`(foreach::foreach(i=seq(nrow(sobol_aggregate$X)), .combine='c'), {
      abm(as.numeric(sobol_aggregate$X[i, ]), out = out)
    })
  } else {
    sobol_sim <- foreach::`%dopar%`(foreach::foreach(i=seq(nrow(sobol_aggregate$X)), .combine='c'), {
      abm(as.numeric(sobol_aggregate$X[i, ]), out = out, iterations = iterations)
    })
  }
  if(verbose) cat("Done with simulations \n")
  if(extra_verbose) print(sobol_sim)
  
  # add simulation results (as vector) to sobol object
  sensitivity::tell(sobol_aggregate, sobol_sim)
  
  new("pcSobol",
      call = call,
      result = sobol_aggregate, 
      timing = as.numeric(proc.time()[[1]]) - start_time,
      session = sessionInfo())
}

