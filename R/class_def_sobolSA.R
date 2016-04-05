################################################################################
#' An S4 class to return the results of sensitivity analyses
#' 
#' @slot call Language from the call of the function.
#' @slot result sobol2007 s3 class from \code{sensitivity} package.
#' @slot timing Numeric vector length one with the total elapsed time it took 
#' to execute.
#' @slot session the results from calling \code{sessionInfo()} at end of
#'   \code{\link{pc_sa}} function.
#'   
#' @export

setClass(Class = "pcSobol",
         slots = c(call = "language",
                   result = "ANY", # "sobol2007"
                   timing = "numeric",
                   session = "ANY")
)

################################################################################
#' @describeIn pcSobol An S4 method for printing a pcSobol S4 object
#' @param ... ignored
#' @export

setMethod("print", "pcSobol",
          function(x, ...) str(x)
)

################################################################################
#' @describeIn pcSobol An S4 method for showing a pcSobol S4 object
#' @param object S4 pcSA object
#' @export

setMethod("show", "pcSobol",
          function(object) {
            cat("An object of class \"pcSA\"\n")
            cat("\nCall:\n", deparse(object@call), "\n\n",sep="")
            cat("Available slots:\n")
            print(slotNames(object))
          }
)

correct_sobol_bias <- function(x) {
  x_corr <- x[["original"]] - x[["bias"]]
  min_ci <- x[["min. c.i."]]
  max_ci <- x[["max. c.i."]]
  x <- cbind(x_corr = x_corr, min_ci = min_ci, max_ci = max_ci, x)
  x[x < 0] <- 0
  x[x > 1] <- 1
  cbind(var = row.names(x), x)
}

################################################################################
#'Plots pcSobol S4 object
#'@describeIn pcSobol
#'  
#'  Plot First-Order (Main) Effects and Total Effects from a Sobol Sensitivity 
#'  Analysis of a Simulation Model in the Same Plot
#'  
#'  
#'  This is function of the \strong{eat} package. \code{sobol_sa} conducts a 
#'  global variance decomposition, and then this can be used to plot it.
#'  
#'@param x The result slot of an object created by \code{sobol_sa}.
#'@param outcome_var Optional character vector for labeling the outcome variable
#'  in the plot. Default is "Outcome".
#'@param legend_pos Character vector that sets the position of the legend to one
#'  of: "topright", "bottomright", "bottomleft", or "topleft".
#'@param ptype Optional Character vector for first order, total, or all effects
#'  c("all", "fo", "total").
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
#' s <- sobol_sa(fake_abm, inputs, "sq")
#' plot(s)
#' 
#'@references J. C. Thiele, W. Kurth, V. Grimm, Facilitating Parameter
#'Estimation and Sensitivity Analysis of Agent-Based Models: A Cookbook Using
#'NetLogo and R. Journal of Artificial Societies and Social Simulation. 17, 11
#'(2014).
#'
#'@export

setMethod("plot", "pcSobol",
          function(x, outcome_var = "Outcome", 
                   legend_pos = c("topright", "bottomright", "bottomleft", "topleft"),
                   ptype = c("all", "fo", "total")){
            
            x <- x@result
            
            ptype <- match.arg(ptype)
            
            if(ptype=="all"){
              legend_pos <- match.arg(legend_pos)
              if(legend_pos == "topright") pos <- c(1,1)
              if(legend_pos == "bottomright") pos <- c(1,0)
              if(legend_pos == "bottomleft") pos <- c(0,0)
              if(legend_pos == "topleft") pos <- c(0,1)
              
              ss <- cbind(correct_sobol_bias(x$S), Effect = "First Order")
              tt <- cbind(correct_sobol_bias(x$T), Effect = "Total")
              p_dat <- rbind(ss, tt)
              
              p <- ggplot2::ggplot(p_dat, ggplot2::aes_string(x = "var", y = "x_corr", 
                                                       color = "Effect",
                                                       ymax = 1.001)) + 
                ggplot2::geom_point(position = ggplot2::position_dodge(width = 0.5)) +
                ggplot2::xlab("Variable") + ggplot2::ylab(
                  paste("Contribution to Variance of", outcome_var)) +
                ggplot2::ggtitle(paste("First Order and Total Effects on", outcome_var)) +
                ggplot2::geom_errorbar(ggplot2::aes_string(ymax = "max_ci", ymin = "min_ci"), 
                                       width=0.5, position = ggplot2::position_dodge(width = 0.5)) + 
                ggplot2::ylim(c(0,1)) + 
                ggplot2::theme_bw() +
                ggplot2::theme(legend.justification = pos, legend.position = pos, 
                               legend.background = ggplot2::element_rect(fill = "white"),
                               legend.key = ggplot2::element_rect(fill = "white"))
            }
            if(ptype=="fo"){
              ss <- correct_sobol_bias(x$S)
              p <- ggplot2::ggplot(ss, ggplot2::aes_string(x = "var", y = "x_corr")) + 
                ggplot2::geom_point() +
                ggplot2::xlab("Variable") + ggplot2::ylab(
                  paste("Estimated First-Order Contribution to Variance of", outcome_var)) +
                ggplot2::ggtitle(paste("First Order Effects of Variables on", outcome_var)) +
                ggplot2::geom_errorbar(ggplot2::aes_string(ymax = "max_ci", ymin = "min_ci"), 
                                       width=0.25) +
                ggplot2::ylim(c(0,1))
            }
            if(ptype=="total"){
              tt <- correct_sobol_bias(x$T)
              p <- ggplot2::ggplot(tt, ggplot2::aes_string(x = "var", y = "x_corr")) + 
                ggplot2::geom_point() + 
                ggplot2::xlab("Variable") + ggplot2::ylab(
                  paste("Estimated Total Contribution to Variance of", outcome_var)) +
                ggplot2::ggtitle(paste("Total Sensitivity Effects of Variables on", outcome_var)) +
                ggplot2::geom_errorbar(ggplot2::aes_string(ymax = "max_ci", ymin = "min_ci"), 
                                       width=0.25) + 
                ggplot2::ylim(c(0,1))
            }
            
            p
          })
