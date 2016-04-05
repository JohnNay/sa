################################################################################
#' An S4 class to return the results of computing iterations needed
#' 
#' @slot call Language from the call of the function.
#' @slot measure Character vector length one with the measure used.
#' @slot result c("src", "pcc") s3 classes from \code{sensitivity} package.
#' @slot plot_data data.frame with two columns.
#' @slot timing Numeric vector length one with the total elapsed time it took 
#' to execute.
#' @slot session the results from calling \code{sessionInfo()} at end of
#'   \code{\link{pc_sa}} function.
#'   
#' @export

setClass(Class = "computeIters",
         slots = c(call = "language",
                   measure = "character",
                   result = "integer",
                   plot_data = "data.frame",
                   timing = "numeric",
                   session = "ANY")
)

################################################################################
#' Prints (cat()) summary of computeIters S4 object
#' @describeIn computeIters
#'
#' @param digits Optional numeric vector length one for how many significant digits to
#' print, default is 3.
#'
#'  @export

setMethod("summary", "computeIters",
          function(object, digits = 3) {
            cat(paste0("\n This process took ", object@timing, " seconds (", 
                round(object@timing/60/60, 2), " hours)."))
            cat("\n Average number of iterations found to be sufficient is", object@result)
            invisible(object)
          }
)

################################################################################
#' @describeIn computeIters An S4 method for printing a computeIters S4 object
#' @param ... ignored
#'  @export

setMethod("print", "computeIters",
          function(x, ...) str(x)
)

################################################################################
#' @describeIn computeIters An S4 method for showing a computeIters S4 object
#' @param object S4 computeIters object
#' 
#'  @export

setMethod("show", "computeIters",
          function(object) {
            cat("An object of class \"computeIters\"\n")
            cat("\nCall:\n", deparse(object@call), "\n\n",sep="")
            cat("Available slots:\n")
            print(slotNames(object))
          }
)


################################################################################
#'Plots computeIters S4 object
#'@describeIn computeIters
#'  
#'@param x The result slot of an object created by \code{compute_iters}.
#'@param outcome_var Optional character vector.
#'@param xlab Optional character vector.
#'@param ylab Optional character vector.
#'  
#'@return Returns a ggplot2 object.
#'  
#'@export

setMethod("plot", "computeIters",
          function(x, outcome_var = "Outcome", 
                   xlab = "Iterations", ylab = NULL){
            
            measure <- x@measure
            
            if (missing(ylab)) {
              ylab <- measure
            } else {
              measure <- ylab
            }
            
            x <- x@plot_data
            
            ggplot2::ggplot(x, 
                            ggplot2::aes_string(x = "iters", y = "measured")) + 
              ggplot2::geom_point() +
              ggplot2::geom_smooth(method = "loess") +
              ggplot2::xlab(xlab) + ggplot2::ylab(ylab) +
              ggplot2::ggtitle(paste("Iterations and", measure, "for", outcome_var)) +
              ggplot2::theme_bw()
          }
)
