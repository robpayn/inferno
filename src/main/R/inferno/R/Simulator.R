# Package dependencies ####

#' @importFrom R6 R6Class
NULL

# Class Simulator (R6) ####

#' @export
#' 
#' @title 
#'   Simulates a scenario based on a model
#'   
#' @description 
#'   A simulator is designed to run a model based on a limited amount of
#'   model input that defines a modeling scenario. This limited input is
#'   characterized by a set of parameters for the model. The simulator also
#'   only returns the model output that is of interest in the scenario, and
#'   the output of interest is defined by the prediction extractor.
#'   
#' @usage
#'   Simulator$new()
#' @param parameterTranslator 
#'   A parameter translator object that is capable
#'   of translating a simple vector of parameter values and inserting them
#'   into the appropriate attributes of the model object, such that
#'   the following execution of the model will generation a prediction
#'   corresponding to those parameter values.
#' @param model
#'   The model used to simulate the scenario
#' @param predictionExtractor 
#'   A prediction extractor object that is capable
#'   of extracting a set of simple vectors from the model output, which can
#'   then be compared to the observations in calculation of the objective
#'   function value.
#'  
#' @section Methods:
#'   $new \cr
#'   $simulate - See \code{\link{Simulator_simulate}}
#'    
Simulator <- R6Class(
   classname = "Simulator",
   public = list(
      parameterTranslator = NULL,
      model = NULL,
      predictionExtractor = NULL,
      initialize = function
         (
            parameterTranslator,
            model,
            predictionExtractor
         )
         {
            self$parameterTranslator <- parameterTranslator;
            self$model <- model;
            self$predictionExtractor <- predictionExtractor;
         }
      )
);

# Method Simulator$simulate ####

#' @name Simulator_simulate
#' 
#' @title 
#'   Simultes a model scenario
#' 
#' @param params
#'   Optional set of parameters defining the scenario.
#'   Defaults to NULL, which will run the model with its current input
#' 
#' @return 
#'   The prediction representing the output of interest in the scenario
#'
#' @section Method of class:
#'   \code{\link{Simulator}}
#'   
Simulator$set(
   which = "public",
   name = "simulate",
   value = function(params = NULL)
      {
         if (!is.null(params)) {
            self$parameterTranslator$translate(params = params);
         }
         self$model$run();
         return(self$predictionExtractor$extract());
      }
);
