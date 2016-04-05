# Coefficient of variation
# @param na.rm Logical vector length one indicating whether 
# NA values should be stripped before the computation proceeds
coef_var <- function(x, na.rm = FALSE){
  sd(x, na.rm = na.rm)/
    mean(x, na.rm = na.rm)
}

#'Determine Number of Iterations to Simulate a Stochastic Model
#'
#'\code{compute_iters} estimates a sufficient number of iterations for 
#'subsequent analysis of a simulation model.
#'
#'This is a function of the \strong{eat} package. It takes an abm in function 
#'form and a list of input values. It returns a list with the result and 
#'diagnostic information.
#'
#'We often deal with stochastic simulations, which requires a 
#'model run (a model run may comprise an arbitrary number of discrete time steps
#'specific to the model) to be repeated >1 times with the same global parameter
#'settings in order to reduce the variance of outcomes within a specified global
#'ABM parameter setting low enough to allow for comparison of outcomes across
#'parameter settings, i.e. for learning anything useful about the input-output
#'relationships that define the ABM. The size of this experimental noise should
#'be analyzed prior to executing simulation experiments and optimization
#'routines. Lorscheid, I., Heine, B.O., & Meyer, M. (2012) suggest using the 
#'coefficient of variation, c_v = s/mu, where s is the standard deviation and mu
#'is the mean. Because the coefficient of variation is a dimensionless and
#'normalized measure of variance it can be used to investigate the variance of 
#'multiple simulation outcome variables.
#'
#'@param abm A function that takes each of the \code{input_values} as arguments.
#'  Specifically, a function that has at least three arguments: 
#'  \code{parameters, out, iterations}.
#'@param input_values List
#'@param out Character vector length one to be passed an argument to the 
#'  \code{abm} function to specify what outcome measure to use.
#'@param sample_count Optional numeric vector length one specifying the number 
#'  of samples for a given \code{iters} value that is being tested. If 
#'  \code{repeats} is very high then this does not need to be as high.
#'@param model_data Optional data.frame with the data that was used to build the
#'  model. This is used if one wants to ensure that all parameters tested are in
#'  the convex hull of the data used to build the model that is now being
#'  analyzed. This uses the WhatIf package in the Suggest field of the eat
#'  description file.
#'@param repeats Optional numeric vector length one specifying the number of
#'  times to repeat the main loop of this algo. If \code{sample_count} is very 
#'  high then this does not need to be as high.
#'@param thresh Optional numeric vector length one. This is a hyper-parameter of
#'  this algorithm and may need to be experimented with by the user.
#'@param initial_iters Optional numeric vector length one. Although optional, 
#'  this is very problem dependent and so should usually be set with knowledge 
#'  of the simulation model.
#'@param max_iters Optional numeric vector length one. Although optional, this 
#'  is very problem dependent and so should usually be set with knowledge of the
#'  simulation model.
#'@param constraints Optional Character vector that is either "none" or is using
#'  only variable names that are specified in the input_values List argument. 
#'  This character vector is evaluated in an environment created for the sampled
#'  data on the variables, and its evaluation results in a Logical vector that 
#'  that subsets sampled.
#'@param parallel Optional logical vector length one. Default is FALSE.
#'@param cores Optional Numeric vector length one. Default is 
#'  parallel::detectCores().
#'@param verbose Optional logical vector. Default for this algorithm is FALSE 
#'  because messages are created during an inner loop, i.e. very often.
#'@param measure Optional character vector. One of \code{c("coef_var", "var", 
#'  "sd")}. Don't use \code{coef_var} if the outcome values of the simulation 
#'  model can take on negative values or are on an arbitrary scale; otherwise, 
#'  use \code{coef_var}.
#'  
#'@return List with the result and diagnostic information. List has elements: 
#'  \code{call; result; timing; and session}. Timing is in seconds.
#'  
#' @examples
#' 
#' fake <- function(inputs, out, iterations) 
#' mean(rnorm(iterations, inputs[1], inputs[2]))
#' inputs <- lapply(list(.mean = NA, .sd = NA),
#' function(x) list(random_function = "qunif",
#' ARGS = list(min = 0.1, max = 0.1)))
#' hist(rnorm(15, 0.1, 0.1))
#' res <- compute_iters(fake, inputs, "hello", repeats = 20,
#' thresh = 1e-100, max_iters = 1e1000,
#' initial_iters = 100)
#' plot(res, ylab = "Coefficient of Variation")
#' 
#'@references Lorscheid, I., Heine, B.O., & Meyer, M. (2012). Opening the "black
#'  box" of simulations: increased transparency and effective communication 
#'  through the systematic design of experiments. Computational and Mathematical
#'  Organization Theory, 18 (1), 22-62.
#'  
#'  Hendricks W, Robey K (1936) The sampling distribution of the coefficient of 
#'  variation. Ann Math Stat 7:129-132
#'  
#'@export

compute_iters <- function(abm, 
                          input_values,
                          out, 
                          sample_count = 20,
                          model_data = NULL,
                          repeats = 30,
                          thresh = 0.05,
                          initial_iters = 5,
                          max_iters = 1e3,
                          constraints = "none",
                          parallel = FALSE,
                          cores = NULL,
                          verbose = FALSE,
                          measure = c("coef_var", "var", "sd")){
  
  # Preparing: ###
  measure <- match.arg(measure)
  start_time <- as.numeric(proc.time()[[3]])
  call <- match.call()
  
  # Get names of input factors:
  input_names <- names(input_values)
  
  if(parallel & missing(cores)) cores <- parallel::detectCores() - 1
  
  # Main outer loop: ###
  res <- foreach::`%do%`(foreach::foreach(i=seq(repeats), .combine='rbind', .verbose=FALSE), {
    iters <- initial_iters
    measured <- 0
    
    repeat{
      # Create sample, removing samples violating constraints, until you have one:
      input.set <- create_set(input_values, input_names, 1, constraints, model_data)
      if(verbose) cat("Done with input set creation.\n")
      
      if (parallel) {
        doParallel::registerDoParallel(cores = cores)
      } # without registering the backend the %dopar% should just run sequentially as %do%
      output <- foreach::`%dopar%`(foreach::foreach(i = seq(sample_count), .combine='c'), {
        abm(as.numeric(input.set), out = out, iterations = iters)
      })
      if(verbose) cat("Done with simulations.\n")
      
      if (measure == "coef_var"){
        measured2 <- coef_var(output)
      }
      if (measure == "var"){
        measured2 <- var(output)
      }
      if (measure == "sd"){
        measured2 <- sd(output)
      }
      
      convergence <- (measured2 - measured) / abs(measured)
      
      if (iters >= max_iters || convergence <= thresh){
        break
      } else {
        measured <- measured2
        iters <- iters + 1
      }
    }
    
    c(iters, measured)
  })
  
  if (dim(res)[1] > 1){
    res_int <- round(mean(res[ ,1]))
    plot_data <- data.frame(iters = res[ ,1], measured = res[ ,2])
  } else {
    res_int <- round(mean(res[1]))
    plot_data <- data.frame(iters = res[1], measured = res[2])
  }
  
  new("computeIters",
      call = call,
      measure = measure,
      result = as.integer(res_int),
      plot_data = plot_data,
      timing = as.numeric(proc.time()[[3]]) - start_time,
      session = sessionInfo())
}
