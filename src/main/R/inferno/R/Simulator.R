# Package dependencies ####

#' @importFrom R6 R6Class
NULL

# Class Simulator (R6) ####

#' @export
#' 
#' @title 
#'   R6 class defining a simulator
#'   
#' @description 
#'   A simulator is designed to run a model based on a limited amount of
#'   model input that defines a modeling scenario. This limited input is
#'   characterized by a set of parameters for the model. The simulator also
#'   only returns the model output that is of interest in the scenario, and
#'   the output of interest is defined by the prediction extractor.
#'   
Simulator <- R6Class(
   classname = "Simulator",
   public = list(
      
      #' @field parameterTranslator
      #'   The object used to translate parameters before running the model
      parameterTranslator = NULL,
      
      #' @field model
      #'   The model used to generate output
      model = NULL,
      
      #' @field predictionExtractor
      #'   The object used to extract a prediction from the model output
      predictionExtractor = NULL,
      
      # Method Simulator$new ####
      #
      #' @description
      #'   Construct a new instance of the class
      #'   
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
      },
      
      # Method Simulator$simulate ####
      #
      #' @description 
      #'   Run a simulation based on the provided parameter values
      #' 
      #' @param params
      #'   Optional set of parameters defining the scenario.
      #'   Defaults to NULL, which will run the model with its current input
      #' 
      #' @return 
      #'   The prediction representing the output of interest in the scenario
      #'
      simulate = function(params = NULL)
      {
         if (!is.null(params)) {
            self$parameterTranslator$translate(params = params);
         }
         self$model$run();
         return(self$predictionExtractor$extract());
      }

   )
)
