# Package dependencies ####

#' @importFrom R6 R6Class
NULL

# Class LogLikelihood (R6) ####

#' @export
#' 
#' @title 
#'   A log likelihood objective function class (R6 class generator)
#' 
#' @description
#'   The R6 class generator for a log likelihood objective function
#'   object used by optimization algorithms. 
#'   For example, an instance of this class would be the objective function 
#'   used for in Maximum Likelihood Estimate (MLE) analysis.
#'     
LogLikelihood <- R6Class(
   classname = "LogLikelihood",
   inherit = ObjectiveFunction,
   public = list(
      
      #' @field sd
      #'   Standard deviation used for calculating likelihood
      sd = NULL,
      
      #' @field negate
      #'   Flag determining if likelihood should be negated
      #'   (usually set to TRUE for minimizing optimizers)
      negate = NULL,
      
      #' @field ignore.na
      #'   Flag determining if NA values are ignored
      ignore.na = NULL,
      
      # Method LogLikelihood$new ####
      #
      #' @description 
      #'   Constructs a new instance of the class
      #'   
      #' @param ... 
      #'    Additional required arguments for the constructor 
      #'    of the superclass \code{ObjectiveFunction} must be provided. Optional
      #'    arguments are also available for various features. 
      #'    See documentation for the class \code{\link{ObjectiveFunction}} for a description
      #'    of the additional required and optional arguments.
      #' @param sd 
      #'    A vector of standard deviations to be used for calculating the 
      #'    likelihood. This vector shoud have the an element with a standard deviation 
      #'    corresponding to each of the columns in the dataframes containing the 
      #'    predictons and observations that are compared by the objective function.
      #' @param negate 
      #'    A boolean switch indicating if the objective function value
      #'    should be negated for switching between maximization or minimization
      #'    algorithms. Note that this is a log likelihood, so negation is 
      #'    equivalent to inverting the probability representing the raw likelihood.
      #'    Default value if FALSE (log likelihood is not negated).
      #' @param ignore.na
      #'    Determines if NA elements will be ignored in objective function value calculation.
      #'    Default is TRUE (ignore NAs in calculation).
      #'    
      initialize = function(..., sd, negate = FALSE, ignore.na = TRUE)
      {
         super$initialize(...);
         self$sd <- sd;
         self$negate <- negate;
         self$ignore.na <- ignore.na;
      },
      
      # Method LogLikelihood$compare ####
      #
      #' @description 
      #'   Compares a list of prediction vectors to an associated list of
      #'   observation vectors based on a formal log likelihood function
      #'   and assumption of a normal distribution of residual error.
      #' 
      #'   Note that handling of NA values depend on the "ignore.na" attribute
      #'   that is configurable in the constructor. By default NAs are ignored.
      #' 
      #' @param params
      #'    Any "NaN" values in the sd attribute will be replaced by parameters values
      #'    from the end of the parameter vector. This feature is critical
      #'    if these standard deviations are being inferred as part of an
      #'    inferential modeling algorithm, rather than being provided as an
      #'    attribute of the object.
      #'    
      #' @return The log likelihood value from the comparison
      #'    (will be negated if negate attribute of the object is TRUE)
      #'    
      compare = function(params)
      {
         sd <- self$sd;
         estimateSD <- is.nan(sd);
         if(any(estimateSD)) {
            replaceIndices <- which(estimateSD);
            paramIndices <- 
               (length(params) - length(replaceIndices) + 1):length(params);
            sd[replaceIndices] <- params[paramIndices];
         }
         logLike <- mapply(
            FUN = function(p, o, sd) 
            {
               sum(
                  dnorm(
                     x = o,
                     mean = p,
                     sd = sd,
                     log = TRUE
                  ),
                  na.rm = self$ignore.na
               )
            },
            p = self$prediction,
            o = self$observation,
            sd = as.list(sd)
         );
         if (self$negate) {
            return(-logLike);
         } else {
            return(logLike);
         }
      }   

   )
)
