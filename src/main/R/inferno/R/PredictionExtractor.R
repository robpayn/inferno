# Package dependencies ####

#' @importFrom R6 R6Class
NULL

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
#' @param model
#'   The model from which the prediction extractor extracts output
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
#' @section Abstract method of class: 
#'   \code{\link{PredictionExtractor}}
#'   
NULL