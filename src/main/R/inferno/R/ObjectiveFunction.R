# Package dependencies ####

#' @importFrom R6 R6Class
NULL

# Class ObjectiveFunction (R6) ####

#' @export
#' 
#' @title 
#'   Abstract objective function class (R6 class generator)
#' 
#' @description 
#'   The R6 class generator for objects representing an objective function 
#'   used to calculate a value that represents agreement between model 
#'   predictions and observations. This abstract class defines the interface
#'   that is generally used by optimizations algorithms designed find the
#'   parameter values that predict the best fit of a model to measured data.
#'   See documentation of the method \code{\link{ObjectiveFunction_propose}}
#'   as the primary method of interface from optimization algorithms.
#'   This class is abstract and is not intended to be instantiated
#'   directly. The constructor is only intended to be called by
#'   an extending subclass.
#'   Usage below is for the class constructor method.
#' 
#' @usage 
#'   ObjectiveFunction$new(<arguments>)
#' @param model 
#'   The model used to generate the predictions to be
#'   compared to the observations by the objective function
#' @param parameterTranslator 
#'   A parameter translator object that is capable
#'   of translating a simple vector of parameter values and inserting them
#'   into the appropriate attributes of the model object, such that
#'   the following execution of the model will generation a prediction
#'   corresponding to those parameter values.
#' @param predictionExtractor 
#'   A prediction extractor object that is capable
#'   of extracting a set of simple vectors from the model output, which can
#'   then be compared to the observations in calculation of the objective
#'   function value.
#' @param synthErrorProcessor 
#'   An optional synthetic error processor object 
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
#' @return 
#'   The object of class \code{ObjectiveFunction} 
#'   instantiated by the constructor
#'    
#' @seealso 
#'   Methods: 
#'   
#' @section Methods:
#'   \code{$new}\cr
#'   \code{$compare} - 
#'     See \code{\link{ObjectiveFunction_compare}}\cr
#'   \code{$propose} - 
#'     See \code{\link{ObjectiveFunction_propose}}\cr
#'   \code{$realize} - 
#'     See \code{\link{ObjectiveFunction_realize}}\cr
#'   \code{$plotFit} - 
#'     See \code{\link{ObjectiveFunction_plotFit}}\cr
#'
ObjectiveFunction <- R6Class(
   classname = "ObjectiveFunction",
   public = list(
      params = NULL,
      parameterTranslator = NULL,
      model = NULL,
      prediction = NULL,
      predictionExtractor = NULL,
      synthPrediction = NULL,
      observation = NULL,
      observationGenerator = NULL,
      multivariateValues = NULL,
      value = NULL,
      initialize = function
         (
            model,
            parameterTranslator,
            predictionExtractor,
            observationGenerator = NULL,
            observation = NULL
         ) 
         {
            self$model <- model;
            self$parameterTranslator <- parameterTranslator;
            self$predictionExtractor <- predictionExtractor;
            self$observationGenerator <- observationGenerator;
            if (!is.null(self$observationGenerator)) {
               if (!is.null(observation)) {
                  stop(paste("Observation generator and ",
                             "observation arguments cannot both be ", 
                             "non-NULL vaulues."));
               }
               self$observationGenerator$objFunc <- self;
               self$model$run();
               self$prediction <- self$predictionExtractor$extract();
               self$synthPrediction <- self$prediction;
               self$realize();
            } else {
               if (is.null(observation)) {
                  stop(paste("Observation generator and ",
                             "observation arguments cannot both be ", 
                             "NULL vaulues."));
               }
               self$observation <- observation;
            }
         }
   )
);

# Method ObjectiveFunction$propose ####

#' @name ObjectiveFunction_propose
#' 
#' @title 
#'    Propose a model 
#' 
#' @description 
#'    Proposes a model with a given permutation represented by a subset of
#'    input
#' 
#' @usage 
#'    [Object]$propose(params)
#' @param params 
#'    The subset of input being proposed as a permutation
#'    
#' @return 
#'    The value of the objective function 
#'    
#' @section Method of class:
#'   \code{\link{ObjectiveFunction}}
#'   
ObjectiveFunction$set(
   which = "public",
   name = "propose",
   value = function(params)
      {
         self$params <- params;
         self$parameterTranslator$translate(params = params);
         self$model$run();
         self$prediction <- self$predictionExtractor$extract();
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
      }
);

# Method ObjectiveFunction$realize ####

#' @name ObjectiveFunction_realize
#' 
#' @title 
#'    Realize a synthetic observation
#' 
#' @description 
#'    Generates a new realization of the observation based on the observation generator
#'    provided. Will cause an error if called and an observation generator attribute
#'    is NULL.
#' 
#' @usage 
#'    [Object]$realize()
#'    
#' @return 
#'    The synthetic observation created
#'    
#' @section Method of class:
#'   \code{\link{ObjectiveFunction}}
#'   
ObjectiveFunction$set(
   which = "public",
   name = "realize",
   value = function()
      {
         self$observation <- self$observationGenerator$generate();
      }
);

# Method ObjectiveFunction$compare ####

#' @name ObjectiveFunction_compare
#' 
#' @title 
#'    Compare the prediction and observation (abstract) 
#' 
#' @description 
#'    A class that inherits from ObjectiveFunction must implement a 
#'    "compare" method that will compare a prediciton to an observation. 
#' 
#'    This method declaration will cause a program to fail if 
#'    the method is called and the implementing class does not override it.
#' 
#' @usage 
#'    [Object]$compare(params)
#' @param params
#'    The object representing model parameters in the event that 
#'    parameters are necessary for the comparison logic
#'    
#' @return 
#'    The value calculated from the comparison 
#'    
#' @section Method of class:
#'   \code{\link{ObjectiveFunction}}
#'   
ObjectiveFunction$set(
   which = "public",
   name = "compare",
   value = function(params) 
      {
         stop("Abstract function 'compare' has not been implemented.");
      }
);

# Method ObjectiveFunction$plotFit ####

#' @name ObjectiveFunction_plotFit
#' 
#' @title 
#'   Plot the fit of a prediction
#' 
#' @description 
#'   Plots the prediction based on the provided parameters 
#'   on the same axes as the observations
#'   
#' @usage 
#'   [Object]$plotFit(<arguments>)
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
#'   No meaningful return value
#'   
#' @section Method of class:
#'   \code{\link{ObjectiveFunction}}
#'   
ObjectiveFunction$set(
   which = "public",
   name = "plotFit",
   value = function
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
);
