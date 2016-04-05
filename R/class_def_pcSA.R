setClassUnion("numericOrchar", members = c("numeric", "character"))

################################################################################
#' An S4 class to return the results of sensitivity analyses
#' 
#' @slot call Language from the call of the function.
#' @slot input_set data.frame of input set used
#' @slot sims numeric vector with outcome of simulating model with input_set
#' @slot result c("src", "pcc") s3 classes from \code{sensitivity} package.
#' @slot r_squared Numeric vector length one.
#' @slot rmse Numeric vector length one.
#' @slot timing Numeric vector length one with the total elapsed time it took to execute.
#' @slot session the results from calling \code{sessionInfo()} at end of
#'   \code{\link{pc_sa}} function.
#'   
#' @export

setClass(Class = "pcSA",
         slots = c(call = "language",
                   input_set = "data.frame",
                   sims = "numeric",
                   result = "ANY", # "pcsens"
                   r_squared = "numericOrchar",
                   rmse = "numericOrchar",
                   timing = "numeric",
                   session = "ANY")
)

################################################################################
#' @describeIn pcSA An S4 method for printing a pcSA S4 object
#' @param ... ignored
#'  @export

setMethod("print", "pcSA",
          function(x, ...) str(x)
)

################################################################################
#' Show
#' 
#' @describeIn pcSA An S4 method for showing a pcSA S4 object
#' 
#' @param object S4 pcSA object
#' 
#'  @export

setMethod("show", "pcSA",
          function(object) {
            cat("An object of class \"pcSA\"\n")
            cat("\nCall:\n", deparse(object@call), "\n\n",sep="")
            cat("Available slots:\n")
            print(slotNames(object))
          }
)


correct_bias <- function(x) {
  x_corr <- x[["original"]] - x[["bias"]]
  min_ci <- x[["min. c.i."]]
  max_ci <- x[["max. c.i."]]
  x <- cbind(x_corr = x_corr, min_ci = min_ci, max_ci = max_ci, x)
  cbind(var = row.names(x), x)
}
################################################################################
#'Plots pcSA S4 object, a Partial Correlation Analysis of a Simulation Model
#'
#'@describeIn pcSA
#'  
#'  This is function of the \strong{eat} package. \code{pc_sa} conducts a a 
#'  partial correlation analysis.
#'  
#'@param x The result slot of an object created by \code{pc_sa}.
#'@param outcome_var Optional character vector for labeling the outcome variable
#'  in the plot. Default is "Outcome".
#'@param xlab Optional character vector for labeling the variables.
#'@param ylab Optional character vector. Default is "Partial Rank Correlation
#'  Coefficient". Could also be "Partial Correlation Coefficient", "Standardized
#'  Rank Regression Coefficient", and "Standardized Regression Coefficient".
#'  
#'@return Returns a ggplot2 plot.
#'  
#' @examples
#' fake_abm <- function(params, out) {
#'   x1 <- params[1]
#'   x2 <- params[2]
#'   if (out=="sq") return(x1^2 + x2 + rnorm(1, 0))
#'   if (out=="ident") return(x1 + x2 + rnorm(1, 0))
#' }
#' inputs <- lapply(list(param1 = NA, param2 = NA), 
#'                  function(x) list(random_function = "qunif",
#'                                   ARGS = list(min = 0, max = 1)))
#' s <- pc_sa(fake_abm, inputs, "sq")
#' plot(s)
#' 
#'@references J. C. Thiele, W. Kurth, V. Grimm, Facilitating Parameter 
#'  Estimation and Sensitivity Analysis of Agent-Based Models: A Cookbook Using 
#'  NetLogo and R. Journal of Artificial Societies and Social Simulation. 17, 11
#'  (2014).
#'  
#'@export

setMethod("plot", "pcSA",
          function(x, outcome_var = "Outcome", 
                   xlab = "Variable", ylab = "Standardized Rank Regression Coefficient"){
            x <- x@result
            ss <- correct_bias(x[[7]]) # $SRRC, $SRC, $PRCC, $PCC
            
            ggplot2::ggplot(ss, ggplot2::aes_string(x = "var", y = "x_corr")) + 
              ggplot2::geom_point() +
              ggplot2::xlab(xlab) + ggplot2::ylab(ylab) +
              ggplot2::ggtitle(paste("Estimated Effects on", outcome_var)) +
              ggplot2::geom_errorbar(ggplot2::aes_string(ymax = "max_ci", ymin = "min_ci"), width=0.25) +
              ggplot2::geom_hline(yintercept = 0) +
              ggplot2::ylim(c(-1,1)) +
              ggplot2::theme_bw()
          }
)
