# Package dependencies ####

#' @importFrom R6 R6Class
NULL

# Class ParameterTranslator (R6) ####

#' @export
#' 
#' @title 
#'   R6 class defining a parmater translator abstraction
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
ParameterTranslator <- R6Class(
   classname = "ParameterTranslator",
   public = list(
      #' @field model
      #'   The model object for which parameter values are changed by the
      #'   translator
      model = NULL,
      
      # Method ParameterTranslator$new ####
      #
      #' @description 
      #'   Construct a new instance of the class
      #' 
      #' @param model
      #'   The model for which the parameter translator generates input
      #' 
      initialize = function(model = NULL)
      {
         if (!is.null(model)) {
            self$setModel(model = model);  
         }
      },
      
      #' @description 
      #'   Set the model for which parameters are translated
      #' 
      #' @param model
      #'   The model with parameters that are altered by calling 
      #'   the translate method.
      #'
      setModel = function(model)
      {
         self$model <- model;
         
         invisible(NULL);
      },
      
      # Method ParameterTranslator$translate ####
      #
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
      translate = function(params)
      {
         stop("Method ParameterTranslator$translate has not been implemented.")
      }
      
   )
)
