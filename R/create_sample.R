#'Create set of samples by sampling with LHS and then checking constraints.
#'
#'\code{create_set} creates sample that stay within constraints.
#'
#'@param input_values List
#'@param input_names Character vector
#'@param sample_count Numeric vector length one.
#'@param constraints Character vector that is either "none" of is using only 
#'  variable names that are specified in the input_values List argument. This 
#'  character vector is evaluated in an environment created for the sampled data
#'  on the variables, and its evaluation results in a Logical vector that that 
#'  subsets sampled.
#'@param model_data Optional data.frame with the data that was used to build the
#'  model. This is used if one wants to ensure that all parameters tested are in
#'  the convex hull of the data used to build the model that is now being
#'  analyzed. This uses the WhatIf package in the Suggest field of the eat
#'  description file.
#'@param create_input_values Instead of the input_values and input_names args which lead the function to use LHS and create the samples that way
#' you can provide a function to this arg that takes an arg for how many samples to create and returns a data.frame with that many rows of samples.
#'  
#'@return Returns a \code{data.frame} of samples.
#'  
#'@export
create_set <- function(input_values = NULL, 
                       input_names = NULL,
                       sample_count, constraints,
                       model_data, model_data_formula = NULL,
                       create_input_values = NULL){
  
  if(!is.null(model_data)){
    if (!requireNamespace("WhatIf", quietly = TRUE)) {
      stop("The WhatIf package is needed to ensure that all parameters tested are in the convex hull of the data you have provided. Please install it.",
           call. = FALSE)
    }
    # A lot will probably not work so generate more each time than you would otherwise.
    sample_count2 <- sample_count*2
  } else {
    sample_count2 <- sample_count
  }
  
  if(!is.null(input_values) & !is.null(input_names)){
    input.sets <- create_sample(input_values, input_names, sample_count2)
  } else{
    if(!is.null(create_input_values)){
      input.sets <- create_input_values(sample_count2)
    } else{
      stop("You either need create_input_values or BOTH input_values and input_names.")
    }
  }
  
  # Discard input factor sets that violate constraints:
  if(constraints != "none") {
    constrained <- with(input.sets, eval(parse(text=constraints)))
    input.sets <- keep_satisfied(input.sets, constrained)
  }
  if(!is.null(model_data)){
    if(!is.null(model_data_formula)){
      constrained <- WhatIf::whatif(formula = model_data_formula,
                                    data = model_data[sort(colnames(model_data))], 
                                    cfact = input.sets[sort(colnames(input.sets))],
                                    choice = "hull")$in.hull
    } else {
      if(!identical(sort(colnames(input.sets)), sort(colnames(model_data))))
        stop("Names of the input_values are not identical to the names of the columns in the model_data.")
      
      constrained <- WhatIf::whatif(data = model_data[sort(colnames(model_data))], 
                                    cfact = input.sets[sort(colnames(input.sets))],
                                    choice = "hull")$in.hull
    }
    input.sets <- keep_satisfied(input.sets, constrained)
  }
  
  needed <- sample_count - nrow(input.sets)
  message("We need ", needed, " more observations in the sample.")
  
  while(needed > 0) { 
    # Create input factor sets by latin hypercube sampling:
    if(!is.null(input_values) & !is.null(input_names)){
      to_add <- create_sample(input_values, input_names, as.integer(needed+(needed/2)))
    } else{
      if(!is.null(create_input_values)){
        to_add <- create_input_values(as.integer(needed+(needed/2)))
      } else{
        stop("You either need create_input_values or BOTH input_values and input_names.")
      }
    }
    
    # Discard input sets that violate constraints:
    if(constraints != "none") {
      constrained <- with(to_add, eval(parse(text=constraints)))
      to_add <- keep_satisfied(to_add, constrained)
    }
      if(!is.null(model_data)){
    if(!is.null(model_data_formula)){
      constrained <- WhatIf::whatif(formula = model_data_formula,
                                    data = model_data[sort(colnames(model_data))], 
                                    cfact = to_add[sort(colnames(to_add))],
                                    choice = "hull")$in.hull
    } else {
      constrained <- WhatIf::whatif(data = model_data[sort(colnames(model_data))], 
                                    cfact = to_add[sort(colnames(to_add))],
                                    choice = "hull")$in.hull
    }
      to_add <- keep_satisfied(to_add, constrained)
  }
    if(!is.null(model_data)){
      constrained <- WhatIf::whatif(data = model_data[sort(colnames(model_data))], 
                                    cfact = to_add[sort(colnames(to_add))],
                                    choice = "hull")$in.hull
      to_add <- keep_satisfied(to_add, constrained)
    }
    
    input.sets <- rbind(input.sets, to_add)
    needed <- sample_count - nrow(input.sets)
    message("We need ", needed, " more observations in the sample.")
  }
  
  input.sets
}

################################################################################
#'@describeIn create_set Create a sample.
#'  
#'@return Returns a data.frame of samples.
#'  
#'@references B. Beachkofski, R. Grandhi, in 43rd AIAA/ASME/ASCE/AHS/ASC
#'Structures, Structural Dynamics, and Materials Conference (American Institute
#'of Aeronautics; Astronautics, 2002;
#'http://arc.aiaa.org/doi/abs/10.2514/6.2002-1274).
#'
#'R. Carnell, Lhs Latin Hypercube Samples (2012), (available at
#'http://cran.r-project.org/web/packages/lhs/index.html).
#'
#'@export

create_sample <- function(input_values, input_names, sample_count) {
  
  # create a random sample of input factor sets with Latin Hypercube Sampling
  lhs_design <- lhs::improvedLHS(sample_count, length(input_values))
  
  # apply random distribution
  lhs_design <- lapply(seq(length(input_values)), function(i) {
    input_values[[i]]$ARGS$p <- as.vector(lhs_design[ ,i])
    do.call(input_values[[i]]$random_function, input_values[[i]]$ARGS)
  })
  
  names(lhs_design) <- input_names
  data.frame(lhs_design)
}

################################################################################
#'@describeIn create_set Stay within constraints.
#'  
#'@param sampled Output of create sample_sample
#'@param constrained Logical vector
#'  
#'@return Returns a data.frame of samples thats the same or less rows as input.
#'  
#'@examples
#'fake_constraints <- "param1 < 0.5 & param2 > 0.5"
#'fake_data <- data.frame(param1 = runif(100), param2 = runif(100))
#'fake_constraints <- with(fake_data, eval(parse(text=fake_constraints)))
#'keep_satisfied(fake_data, fake_constraints)
#'
#'@export

keep_satisfied <- function(sampled, constrained){
  message("Droppping ", sum(!constrained), " observations.")
  stopifnot(identical(nrow(sampled), length(constrained)))
  result <- data.frame(sampled[constrained, , drop=FALSE])
  stopifnot(nrow(result) <= nrow(sampled))
  result
}
