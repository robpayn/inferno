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
   classname = "ObservationGenerator"
);

# Method ObservationGenerator$generate ####

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
#' @param base
#'   Basis for the synthetic observations
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
   value = function(base)
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

# Method ObservationGeneratorNormalErr$generate ####

#' @name ObservationGeneratorNormalErr_generate
#' 
#' @title 
#'   Generate an observation with normally distribute synthetic error
#' 
#' @description  
#'   Generates synthetic observations based on normally-distributed
#'   and independent error.
#'   
#' @param base
#'   Basis for the synthetic observations. Normally distributed error
#'   is added to the basis to create the observations.
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
   value = function(base)
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
            pred = base,
            lengthPred = lapply(
               X = base,
               FUN = length
               ),
            mean = self$mean,
            sd = self$sd,
            SIMPLIFY = FALSE
         )));
      }
);

# Class ObservationGeneratorNormalAR1Err ####

#' @export
#' 
#' @title 
#'   Observation error generator for autocorrelated error
#' 
#' @description 
#'   A observation generator for residual error with 
#'   random (normally distributed) and autocorrelated
#'   components.
#' 
#' @section Methods:
#'   \code{$generate} - 
#'     See \code{\link{ObservationGeneratorNormalAR1Err_generate}}\cr
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

# Method ObservationGeneratorNormalAR1Err$generate ####

#' @name ObservationGeneratorNormalAR1Err_generate
#' 
#' @title 
#'   Generate an observation with autocorrelated synthetic error
#' 
#' @description  
#'   Generates synthetic observations based on components of
#'   random (normally distributed) and autocorrelated error.
#'   
#' @param base
#'   Basis for the synthetic observations. Synthetic error
#'   is added to the basis to create the observations.
#' 
#' @return 
#'   An object representing the synthetic observations 
#'   
#' @section Method of class:
#'   \code{\link{ObservationGeneratorNormalAR1Err}}
#'   
#' @section Implements:
#'   \code{\link{ObservationGenerator_generate}}
#'   
ObservationGeneratorNormalAR1Err$set(
   which = "public",
   name = "generate",
   value = function(base)
      {
         observation <- base;
         for (column in 1:length(observation)) {
            error <- rnorm(
               n = 1, 
               mean = self$mean[[column]], 
               sd = self$sd[[column]]
            );
            observation[[column]][1] <- 
               observation[[column]][1] +
               error;
            
            for (row in 2:length(observation[[column]])) {
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
