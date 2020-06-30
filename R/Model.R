# Package dependencies ####

#' @importFrom R6 R6Class
NULL

# Interface Model (R6) ####

#' @name Model
#' 
#' @title 
#'   R6 Abstract class defining a model interface
#' 
#' @description
#'   Inferno classes use a Model interface to manipulate and run models.
#' 
Model <- R6Class(
   classname = "Model",
   public = list(
      
      # Method Model$run ####
      #
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
      run = function()
      {
         stop("Method Model$run is not implemented.");
      }
      
   )
)
