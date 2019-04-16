# Package dependencies ####

#' @importFrom R6 R6Class
NULL

# Class ObservationGenerator ####

#' @export
#' 
#' @title 
#'   Generates an observation from a known model
#' 
#' @description 
#'   Generates a synthetic observation data set base on a model.
#' 
#'   This class is abstract and is not intended to be instantiated
#'   directly. 
#' 
#' @usage 
#'   Abstract
#'   
#' @section Methods:
#'   \code{$generate} - 
#'     See \code{\link{ObservationGenerator_generate}}\cr
#'
ObservationGenerator <- R6Class(
   classname = "ObservationGenerator",
   public = list(
      objFunc = NULL
   )
);

#' @name ObservationGenerator_generate
#'   
#' @title 
#'   Generate a synthetic observation (abstract) 
#' 
#' @description 
#'   A class that inherits from ObservationGenerator must implement a 
#'   "generate" method that will create the synthetic observations. 
#' 
#'   This method declaration will cause a program to fail if 
#'   the method is called and the implementing class does not override it.
#' 
#' @return 
#'   An object representing the synthetic observation 
#'   
#' @section Method of class:
#'   \code{\link{ObservationGenerator}}
#'   
ObservationGenerator$set(
   which = "public",
   name = "generate",
   value = function()
      {
         stop("Abstract function 'ObservationGenerator$generate' 
              has not been implemented");
      }
);

# Class ObservationGeneratorNormalErr ####

#' @export
#' 
#' @title 
#'   Generates observations with normal error
#' 
#' @description 
#'   A observation generator for independent and normally
#'   distributed error.
#' 
#' @section Methods:
#'   \code{$generate} - 
#'     See \code{\link{ObservationGeneratorNormalErr_generate}}\cr
#'
ObservationGeneratorNormalErr <- R6Class(
   classname = "ObservationGeneratorNormalErr",
   inherit = ObservationGenerator,
   public = list(
      mean = NULL,
      sd = NULL,
      initialize = function(mean, sd)
      {
         self$mean <- mean;
         self$sd <- sd;
      }
   )
);

#' @name ObservationGeneratorNormalErr_generate
#' 
#' @title 
#'   Generate an observation with normally distribute synthetic error
#' 
#' @description  
#'   Generates synthetic observations for an objective function
#'   based on the current prediction and independent, normally-distributed
#'   error.
#' 
#' @return 
#'   An object representing the synthetic observations 
#'   
#' @section Method of class:
#'   \code{\link{ObservationGeneratorNormalErr}}
#'   
#' @section Implements:
#'   \code{\link{ObservationGenerator_generate}}
#'   
ObservationGeneratorNormalErr$set(
   which = "public",
   name = "generate",
   value = function()
      {
         return(data.frame(mapply(
            FUN = function(pred, lengthPred, mean, sd) 
            {
               return(
                  pred + rnorm(
                     n = lengthPred, 
                     mean = mean, 
                     sd = sd
                  )
               );
            }, 
            pred = self$objFunc$synthPrediction,
            lengthPred = lapply(
               X = self$objFunc$synthPrediction,
               FUN = length
               ),
            mean = self$mean,
            sd = self$sd,
            SIMPLIFY = FALSE
         )));
      }
);

#' @export
#' 
#' @title 
#'   Observation error generator for autocorrelated error
#'   
ObservationGeneratorNormalAR1Err <- R6Class(
   classname = "ObservationGeneratorNormalAR1Err",
   inherit = ObservationGeneratorNormalErr,
   public = list(
      arCoeff = NULL,
      initialize = function(..., arCoeff)
      {
         super$initialize(...);
         self$arCoeff <- arCoeff;
      }
   )
);

ObservationGeneratorNormalAR1Err$set(
   which = "public",
   name = "generate",
   value = function()
      {
         observation <- self$objFunc$synthPrediction;
         for (column in 1:length(observation)) {
            error <- rnorm(
               n = 1, 
               mean = self$mean[[column]], 
               sd = self$sd[[column]]
            );
            observation[[column]][1] <- 
               observation[[column]][1] +
               error;
            
            for (row in 2:length(self$objFunc$synthPrediction[[column]])) {
               error <-
                  (self$arCoeff[[column]] * error) +
                  rnorm(
                     n = 1, 
                     mean = self$mean[[column]], 
                     sd = self$sd[[column]]
                  );
               observation[[column]][row] <- 
                  observation[[column]][row] +
                  error;
            }
         }
         
         return(observation);
      }
);
