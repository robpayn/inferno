# Package dependencies ####

#' @importFrom R6 R6Class
NULL

# Class ObservationGenerator ####

#' @export
#' 
#' @title 
#'   R6 class defining an artificial observation generator
#' 
#' @description 
#'   Generates an artificial observation data set base on a model.
#' 
#'   This class is abstract and is not intended to be instantiated
#'   directly. 
#' 
ObservationGenerator <- R6Class(
   classname = "ObservationGenerator",
   public = list(
      
      # Method ObservationGenerator$generate ####
      #
      #' @description 
      #'   A class that inherits from ObservationGenerator must implement a 
      #'   "generate" method that will create the synthetic observations. 
      #' 
      #'   This method declaration will cause a program to fail if 
      #'   the method is called and the implementing class does not override it.
      #'   
      #' @param base
      #'   Basis for the artifical observations
      #' 
      #' @return 
      #'   An object representing the synthetic observation 
      #'   
      generate = function(base)
      {
         stop("Method ObservationGenerator$generate has not been implemented");
      }

   )
);


# Class ObservationGeneratorNormalErr ####

#' @export
#' 
#' @title 
#'   R6 class defining an observation generator assuming normal error
#' 
#' @description 
#'   A observation generator for independent and normally
#'   distributed error.
#' 
ObservationGeneratorNormalErr <- R6Class(
   classname = "ObservationGeneratorNormalErr",
   inherit = ObservationGenerator,
   public = list(
      
      #' @field mean
      #'   Mean of the normal distribution of artificial error
      mean = NULL,
      
      #' @field sd
      #'   Standard deviation of the distribution of artificial error
      sd = NULL,
      
      # Method ObservationGeneratorNormalErr$new ####
      #
      #' @description 
      #'   Construct a new instance of the class
      #' 
      #' @param mean
      #'   Mean of the normal distribution to use for artificial error
      #' @param sd  
      #'   Standard deviation of the normal distribution to use for
      #'   artifical error
      #'   
      initialize = function(mean, sd)
      {
         self$mean <- mean;
         self$sd <- sd;
      },
      
      # Method ObservationGeneratorNormalErr$generate ####
      #
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
      generate = function(base)
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

   )
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
ObservationGeneratorNormalAR1Err <- R6Class(
   classname = "ObservationGeneratorNormalAR1Err",
   inherit = ObservationGeneratorNormalErr,
   public = list(
      
      #' @field arCoeff
      #'   The autoregression coefficient
      arCoeff = NULL,
      
      # Method ObservationGeneratorNormalAR1Err$new ####
      #
      #' @description 
      #'   Construct a new instance of the clas
      #' 
      #' @param ...
      #'   Arguments passed to the constructor of the super class
      #' @param arCoeff
      #'   The autoregression coefficient value
      #'   
      initialize = function(..., arCoeff)
      {
         super$initialize(...);
         self$arCoeff <- arCoeff;
      },
      
      # Method ObservationGeneratorNormalAR1Err$generate ####
      #
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
      generate = function(base)
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

   )
)
