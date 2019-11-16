# Package dependencies ####

#' @importFrom R6 R6Class
NULL

# Class ParameterTranslator (R6) ####

#' @export
#' 
#' @title 
#'   Abstract parameter translator class (R6)
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
#'   Parameters listed are for the constructor function ($new).
#' 
#' @param model
#'   The model for which the parameter translator generates input
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
#' @section Abstract method of class: 
#'   \code{\link{ParameterTranslator}}
#'   
NULL