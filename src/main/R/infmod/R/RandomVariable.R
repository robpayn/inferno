# Package dependencies ####

# All RandomVariable classes are R6 classes.
library(R6);

# Class RandomVariable (R6) ####

#' @title Abstract random variable class (R6)
#' 
#' @description
#' A class representing a random variable with a given distribution.
#' This class is abstract and is not intended to be instantiated
#' directly.
#' 
#' @usage Abstract
#' @export
RandomVariable <- R6::R6Class(
   classname = "RandomVariable",
   public = list(
      density = function(val, log)
      {
         stop("Abstract function 'density' has not been implemented");   
      }
   )
);

#' @title Provides the probability density 
#' 
#' @description 
#' Provides the probability density for a given value in the 
#' distribution of the random variable.
#' 
#' @name RandomVariable_density
#' @param val The value for which the probability density is requested
#' @param log A boolean switch for requesting the log probability density
#' @return The probability density of the value requested 
NULL


# Class RVUniform (R6) ####

#' @title Unifrom random variable class (R6)
#' 
#' @description 
#' Provides tools for working with a random variable with
#' a uniform distribution defined by minimum and maximum
#' values
#' 
#' @export
#' @usage \code{RVUnifrom$new(min, max)} (constructor)
#' @param min Mininum value for the uniform distribution
#' @param max Maximum value for the uniform distribution
#' @return The object of class \code{RVUniform} created
#'    by the constructor
RVUniform <- R6::R6Class(
   classname = "RVUniform",
   inherit = RandomVariable,
   public = list(
      min = NULL,
      max = NULL,
      initialize = function(min, max)
      {
         self$min <- min;
         self$max <- max;
      },
      density = function(val, log = FALSE)
      {
         return(dunif(
            x = val, 
            min = self$min,
            max = self$max,
            log = log
         ));
      }
   )
);

#' @title Provides the probability density
#' 
#' @description 
#' Method to provide the probability density for a given value in the 
#' uniform distribution represented by the object
#' 
#' @name RVUniform_density
#' @param val The value for which the probability density is requested (numeric vector)
#' @param log A boolean switch for requesting the log probability density
#' @return The probability density of the value requested (numeric vector)
NULL

# Class RVNormal (R6) ####

#' @title Normally distributed random variable class (R6)
#' 
#' @description 
#' Provides tools for working with a random variable with
#' a normal distribution defined by a mean and standard deviation
#' 
#' @export
#' @usage \code{RVNormal$new(mean, sd)} (constructor)
#' @param mean Mean of the distribution
#' @param sd Standard deviation of the uniform distribution
#' @return The object of class \code{RVNormal} created
#'    by the constructor
RVNormal <- R6::R6Class(
   classname = "RVNormal",
   inherit = RandomVariable,
   public = list(
      mean = NULL,
      sd = NULL,
      initialize = function(mean, sd)
      {
         self$mean <- mean;
         self$sd <- sd;
      },
      density = function(val, log = FALSE)
      {
         return(dnorm(
            x = val, 
            mean = self$mean,
            sd = self$sd,
            log = log
         ));
      }
   )
);

#' @title Provides the probability density
#' 
#' @description 
#' Method to provide the probability density for a given value in the 
#' normal distribution represented by the object
#' 
#' @name RVNormal_density
#' @param val The value for which the probability density is requested
#' @param log A boolean switch for requesting the log probability density
#' @return The probability density of the value requested 
NULL
