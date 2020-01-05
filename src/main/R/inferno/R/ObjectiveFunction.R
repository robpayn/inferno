# Package dependencies ####

#' @importFrom R6 R6Class
NULL

# Class ObjectiveFunction (R6) ####

#' @export
#' 
#' @title 
#'   R6 class defining an Objective Function abstraction
#' 
#' @description 
#'   The R6 class generator for objects representing an objective function 
#'   used to calculate a value that represents agreement between model 
#'   predictions and observations. This abstract class defines the interface
#'   that is generally used by optimizations algorithms designed find the
#'   parameter values that predict the best fit of a model to measured data.
#'   See documentation of the propose method 
#'   as the primary method of interface from optimization algorithms.
#'   This class is abstract and is not intended to be instantiated
#'   directly. The constructor is only intended to be called by
#'   an extending subclass.
#'   
ObjectiveFunction <- R6Class(
   classname = "ObjectiveFunction",
   public = list(
      
      #' @field params
      #'   Parameter values that generated the current objective function value
      params = NULL,
      
      #' @field model
      #'   Model (from simulator) used to generate the prediction that is 
      #'   compared to the observation
      model = NULL,
      
      #' @field simulator
      #'   Simulator used to generate the prediction that is 
      #'   compared to the observation
      simulator = NULL,
      
      #' @field prediction
      #'   The prediction that generated the current objective function value
      prediction = NULL,
      
      #' @field synthPrediction
      #'   A synthetic prediction (used for Monte Carlo realizations)
      synthPrediction = NULL,
      
      #' @field observation
      #'   The observations to which predictions are compared
      observation = NULL,
      
      #' @field observationGenerator
      #'   An optional observation generator that can be used to generate
      #'   artificial data for Monte Carlo realizations
      observationGenerator = NULL,
      
      #' @field multivariateValues
      #'   The vector of values representing objective function value for 
      #'   each column of the observation
      #'   and prediction data frames. These values are summed to provide the
      #'   overall value of the objective function.
      multivariateValues = NULL,
      
      #' @field value
      #'   The calculated value of the objective function for the last proposal
      value = NULL,
      
      # Method ObjectiveFunction$new ####
      #
      #' @description 
      #'   Construct a new instance of the class
      #' 
      #' @param simulator 
      #'   The simulator (see \code{\link{Simulator}}) used to generate the predictions to be
      #'   compared to the observations by the objective function
      #' @param observationGenerator 
      #'   An optional artificial observation generator object 
      #'   that can generate a synthetic observation based on some known structure
      #'   in error. By default this is null.  By setting to a valid object, this
      #'   will create a synthetic prediction from the current model configuration,
      #'   and then create a synthetic observation upon construction of the
      #'   objective function object. This feature and the "realize" method for
      #'   generating a new realization of synthetic error is designed to 
      #'   facilitatin Monte Carlo error propagation algorithms.
      #' @param observation 
      #'   The observations to compare to the predictions
      #'   by the objective function. Observations should be a data frame with
      #'   the same number of rows and columns as the prediction.
      #'    
      #'   Note that any observations provided as an
      #'   argument will be overwritten if a valid synthetic error processor is
      #'   provided. This argument defaults to a null value, so it is optional
      #'   if a synthetic error processor is provided. The object cannot be constructed
      #'   if the synthErrorProcessor and observation arguments are both NULL.
      #'   
      initialize = function
      (
         simulator,
         observationGenerator = NULL,
         observation = NULL
      ) 
      {
         self$model <- simulator$model;
         self$simulator <- simulator;
         self$observationGenerator <- observationGenerator;
         if (!is.null(self$observationGenerator)) {
            if (!is.null(observation)) {
               stop(paste(
                  "Observation generator and ",
                  "observation arguments cannot both be ", 
                  "non-NULL vaulues."
               ));
            }
            #FIXME Need to switch to cleaner strategy pattern
            self$prediction <- simulator$simulate();
            self$synthPrediction <- self$prediction;
            self$realize();
         } else {
            if (is.null(observation)) {
               stop(paste(
                  "Observation generator and ",
                  "observation arguments cannot both be ", 
                  "NULL vaulues."
               ));
            }
            self$observation <- observation;
         }
      },
      
      # Method ObjectiveFunction$propose ####
      #
      #' @description 
      #'    Proposes a model with a given permutation represented by a subset of
      #'    input
      #' 
      #' @param params 
      #'    The subset of input being proposed as a permutation
      #'    
      #' @return 
      #'    The value of the objective function 
      #'    
      propose = function(params)
      {
         self$params <- params;
         self$prediction <- self$simulator$simulate(params);
         if(is.null(self$prediction)) {
            self$multivariateValues <- rep(
               x = NA, 
               times = length(self$observation)
            );
            self$value <- NA;   
         } else {
            self$multivariateValues <- self$compare(params);
            self$value <- sum(self$multivariateValues);
         }
         return(self$value);
      },
      
      # Method ObjectiveFunction$realize ####
      #
      #' @description 
      #'    Generates a new realization of the observation based on the observation generator
      #'    provided. Will cause an error if called and an observation generator attribute
      #'    is NULL.
      #' 
      #' @return 
      #'    The artificial observation created
      #'    
      realize = function()
      {
         self$observation <- 
            self$observationGenerator$generate(self$synthPrediction);
         return(self$observation);
      },
      
      # Method ObjectiveFunction$compare ####
      #
      #' @description 
      #'    A class that inherits from ObjectiveFunction must implement a 
      #'    "compare" method that will compare a prediciton to an observation. 
      #' 
      #'    This method declaration will cause a program to fail if 
      #'    the method is called and the implementing class does not override it.
      #' 
      #' @param params
      #'    The object representing model parameters in the event that 
      #'    parameters are necessary for the comparison logic
      #'    
      #' @return 
      #'    The value calculated from the comparison 
      #'    
      compare = function(params) 
      {
         stop("Method ObjectiveFunction$compare has not been implemented.");
      },
      
      # Method ObjectiveFunction$plotFit ####
      #
      #' @description 
      #'   Plots the prediction based on the provided parameters 
      #'   on the same axes as the observations
      #'   
      #' @param params
      #'   Parameters for generating the prediction
      #' @param x
      #'   Optional vector of x-axis values for the plot.
      #'   Defaults to plotting x-axis as vector index.
      #' @param ylabs
      #'   Optional list of labels for y-axes on plots.
      #'   Defaults to names of list elements in the observations.
      #' @param lineArgs
      #'   List of arguments passed to the function creating
      #'   the lines for the predictions.
      #'   Defaults to no additional arguments 
      #'   (defaults for \code{lines()} function)
      #' @param ...
      #'   Additional arguments are passed to the \code{plot()} function
      #'   that generates the axes and plots the observations
      #'   
      #' @return 
      #'   No defined return value
      #'   
      plotFit = function
      (
         params, 
         x = numeric(), 
         ylabs = as.list(names(self$observation)),
         lineArgs = list(), 
         ...
      )
      {
         self$propose(params);
         par(mfrow = c(length(self$observation) ,1));
         for(count in 1:length(self$observation)) {
            if (length(x) == 0) {
               xvar = self$observation[[count]];
               yvar = NULL;
               lineArgs$x <- self$prediction[[count]];
            } else {
               xvar = x;
               yvar = self$observation[[count]];
               lineArgs$x <- x;
               lineArgs$y <- self$prediction[[count]];
            }
            plot(
               x = xvar,
               y = yvar,
               ylab = ylabs[[count]],
               ...
            );
            do.call(
               what = lines,
               args = lineArgs
            );
         }
      }

   )
)
