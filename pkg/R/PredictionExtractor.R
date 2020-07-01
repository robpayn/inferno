# Package dependencies ####

#' @importFrom R6 R6Class
NULL

# Class PredictionExtractor (R6) ####

#' @export
#' 
#' @title 
#'   R6 class defining a simulation prediction extractor
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
PredictionExtractor <- R6Class(
   classname = "PredictionExtractor",
   public = list(
      
      #' @field model
      #'   Model object from which the output is extracted
      model = NULL,
      
      # Method PredictionExtractor$new ####
      #
      #' @description 
      #'   Constructs a new instance of the class
      #'   
      #' @param model
      #'   The model from which the output is extracted
      #' 
      initialize = function(model)
      {
         self$model <- model;  
      },
      
      # Method PredictionExtractor$extract ####
      #
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
      extract = function()
      {
         stop("Method PredictionExtractor$extract has not been implemented.");
      }
      
   )
)
