# Package dependencies ####

#' @importFrom R6 R6Class
NULL

# Interface Model (R6) ####

#' @name Model
#' 
#' @title 
#'   Interface to a Model (R6)
#' 
#' @description
#'   Inferno classes use a Model interface to manipulate and run models.
#' 
#' @return
#'   This is a definition of an abstract interface and cannot be instantiated
#'    
#' @section Abstract methods: 
#'   $run - see \code{\link{Model_run}}
NULL

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
#' @section Abstract method of interface: 
#'   \code{\link{Model}}
NULL
