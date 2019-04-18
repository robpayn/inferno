# Package dependencies ####

#' @importFrom R6 R6Class
NULL

# Class Model (R6) ####

#' @export
#' 
#' @title 
#'   Abstract model class
#' 
#' @description
#'   An abstract class representing a model used to make a prediction
#'   that is compared to an observation for calculation of an
#'   objective function.
#' 
#' @return
#'   This class is abstract and is not intended to be instantiated
#'   directly.
#'    
#' @section Abstract methods: 
#'   $run - see \code{\link{Model_run}}
Model <- R6Class(classname = "Model");

# Method Model$run ####

#' @name Model_run
#' 
#' @title 
#'   Run the model (abstract) 
#' 
#' @description 
#'   A class that inherits from Model must implement a "run" method that will
#'   execute the model with its given configuration.
#' 
#'   This method declaration will cause a program to fail if 
#'   the method is called and the implementing class does not override it.
#' 
#' @return 
#'   No required return value 
#'    
#' @section Method of class: 
#'   \code{\link{Model}}
Model$set(
   which = "public",
   name = "run",
   value = function() 
      {
         stop("Abstract function 'run' has not been implemented.");
      }
);


# Class ParameterTranslator (R6) ####

#' @export
#' 
#' @title 
#'   Abstract parameter translator class
#' 
#' @description 
#'   Configures a model for execution based translating model parameters from
#'   an object.  For example, as simple parameter translator may
#'   be used to configure a model from a simple vector of parameter values
#'   provided by an inferential modeling algorithm.
#' 
#'   This class is abstract and is not intended to be instantiated
#'   directly. Inheriting parameter translator classes are specific to a given 
#'   type of model input and the model that takes that input before execution.
#' 
#' @section Abstract methods: 
#'   $translate - see \code{\link{ParameterTranslator_translate}}
#'   
ParameterTranslator <- R6Class(
   classname = "ParameterTranslator",
   public = list(
      model = NULL,
      initialize = function(model)
      {
         self$model <- model;  
      }
   )
);

# Method ParameterTranslator$translate ####

#' @name ParameterTranslator_translate
#' 
#' @title 
#'   Translate the parameters (abstract) 
#' 
#' @description 
#'   A class that inherits from ParameterTranslator must implement a 
#'   "translate" method that will reconfigure the model input according
#'   to the provided object representing model parameters. 
#' 
#'   This method declaration will cause a program to fail if 
#'   the method is called and the implementing class does not override it.
#' 
#' @param params
#'   Parameter values
#' 
#' @return 
#'   No defined return value 
#'   
#' @section Method of class: 
#'   \code{\link{ParameterTranslator}}
#'   
ParameterTranslator$set(
   which = "public",
   name = "translate",
   value = function(params)
      {
         stop("Abstract function 'ParameterTranslator$translate' 
              has not been implemented");
      }
);

# Class PredictionExtractor (R6) ####

#' @export
#' 
#' @title 
#'   Extracts predictions from model output
#' 
#' @description 
#'   Extracts a relatively simple subset of model ouptut used to compare 
#'   the model prediction to other data. For example, a prediciton extractor may
#'   be used to extract a simple vector of predicted values to compare
#'   to observed values in an inferential modeling algorithm.
#' 
#'   This class is abstract and is not intended to be instantiated
#'   directly. Inheriting parameter extractors are specific to a given 
#'   type of model input and the model that takes that input before execution.
#' 
#' @section Abstract methods: 
#'   $extract - see \code{\link{PredictionExtractor_extract}}
#'   
PredictionExtractor <- R6Class(
   classname = "PredictionExtractor",
   public = list(
      model = NULL,
      initialize = function(model)
         {
            self$model <- model;  
         }
   )
);

# Method PredictionExtractor$extract ####

#' @name PredictionExtractor_extract
#' 
#' @title 
#'   Extract prediction from output (abstract) 
#' 
#' @description 
#'   A class that inherits from PredictionExtractor must implement an 
#'   "extract" method that will extract the desired subset of model
#'   output. 
#' 
#'   This method declaration will cause a program to fail if 
#'   the method is called and the implementing class does not override it.
#' 
#' @return 
#'   An object representing the subset of model output
#'    
#' @section Method of class: 
#'   \code{\link{PredictionExtractor}}
#'   
PredictionExtractor$set(
   which = "public",
   name = "extract",
   value = function()
      {
         stop(paste(
            "Abstract function 'PredictionExtractor$extract'", 
            "has not been implemented"
         ));
      }
);
