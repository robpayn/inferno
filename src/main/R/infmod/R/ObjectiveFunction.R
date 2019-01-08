# Package dependencies ####

# All ObjectiveFunction classes and composite classes
# are R6 classes.
library(R6);

# Class Model (R6) ####

#' @title Abstract model class
#' 
#' @description
#' A class representing a model used to make a prediction
#' that is compared to an observation for calculation of an
#' objective function.
#' 
#' This class is abstract and is not intended to be instantiated
#' directly. 
#' 
#' @usage Abstract
#' @export
Model <- R6::R6Class(
   classname = "Model",
   public = list(
      run = function() 
      {
         stop("Abstract function 'run' has not been implemented.");
      }
   )
);

#' @title Run the model (abstract) 
#' 
#' @description 
#' A class that inherits from Model must implement a "run" method that will
#' execute the model with its given configuration.
#' 
#' This method declaration will cause a program to fail if 
#' the method is called and the implementing class does not override it.
#' 
#' @name Model_run
#' @return No required return value 
NULL

# Class ParameterProcessor ####

#' @title Abstract parameter processor class
#' 
#' @description 
#' A parameter processor object knows how to configure a model for
#' execution based on an object representing some portion of that
#' model's inputs.  For example, as simple parameter processor may
#' be used to configure a model from a simple vector of parameter values
#' in an inferential modeling algorithm.
#' 
#' This class is abstract and is not intended to be instantiated
#' directly. Inheriting parameter processors are specific to a given 
#' type of model input and the model that takes that input before execution.
#' 
#' @usage Abstract
#' @export
ParameterProcessor <- R6::R6Class(
   classname = "ParameterProcessor",
   public = list(
      model = NULL,
      initialize = function(model = NULL)
      {
         self$model <- model;  
      },
      process = function(params)
      {
         stop("Abstract function 'ParameterProcessor.process' 
              has not been implemented");
      }
   )
);

#' @title Process the input (abstract) 
#' 
#' @description 
#' A class that inherits from ParameterProcessor must implement a 
#' "process" method that will reconfigure the model input according
#' to the provided object representing model parameters. 
#' 
#' This method declaration will cause a program to fail if 
#' the method is called and the implementing class does not override it.
#' 
#' @name ParameterProcessor_process
#' @return No required return value 
NULL

# Class PredictionProcessor ####

#' @title Abstract prediction processor class
#' 
#' @description 
#' A prediction processor object knows how to extract a relatively
#' simple subset of model ouptut used to compare the model results
#' to other data. For example, a prediciton processor may
#' be used to extract a simple vector of predicted values to compare
#' to observed values in an inferential modeling algorithm.
#' 
#' This class is abstract and is not intended to be instantiated
#' directly. Inheriting parameter processors are specific to a given 
#' type of model input and the model that takes that input before execution.
#' 
#' @usage Abstract
#' @export
PredictionProcessor <- R6::R6Class(
   classname = "PredictionProcessor",
   public = list(
      model = NULL,
      initialize = function(model = NULL)
      {
         self$model <- model;  
      },
      process = function()
      {
         stop("Abstract function 'PredictionProcessor.process' 
              has not been implemented");
      }
   )
);

#' @title Process the output (abstract) 
#' 
#' @description 
#' A class that inherits from PredictionProcessor must implement a 
#' "process" method that will extract the desired subset of model
#' output. 
#' 
#' This method declaration will cause a program to fail if 
#' the method is called and the implementing class does not override it.
#' 
#' @name PredictionProcessor_process
#' @return An object representing the subset of model output 
NULL

# Class SynthErrorProcessor ####

#' @title Abstract synthetic error processor class
#' 
#' @description 
#' A synthetic error processor can generate a synthetic set of errors
#' based on a known structure of error.
#' 
#' This class is abstract and is not intended to be instantiated
#' directly. Inheriting parameter processors are specific to a given 
#' type of model input and the model that takes that input before execution.
#' 
#' @usage Abstract
#' @export
SynthErrorProcessor <- R6::R6Class(
   classname = "SynthErrorProcessor",
   public = list(
      objFunc = NULL,
      process = function()
      {
         stop("Abstract function 'SynthErrorProcessor.process' 
              has not been implemented");
      }
   )
);

#' @title Process the synthetic error (abstract) 
#' 
#' @description 
#' A class that inherits from SynthErrorProcessor must implement a 
#' "process" method that will create the synthetic observations. 
#' 
#' This method declaration will cause a program to fail if 
#' the method is called and the implementing class does not override it.
#' 
#' @name SynthErrorProcessor_process
#' @return An object representing the synthetic error 
NULL

# Class SynthErrorNormal ####

#' @title Processor for synthetic normally distributed error
#' 
#' @description 
#' A synthetic error processor for normally independent normally
#' distributed error.
#' 
#' @export
SynthErrorNormal <- R6::R6Class(
   classname = "SynthErrorNormal",
   inherit = SynthErrorProcessor,
   public = list(
      mean = NULL,
      sd = NULL,
      initialize = function(mean, sd)
      {
         self$mean <- mean;
         self$sd <- sd;
      },
      process = function()
      {
         return(data.frame(mapply(
            FUN = function(pred, mean, sd) 
            {
               return(
                  pred + rnorm(
                     n = nrow(self$objFunc$synthPrediction), 
                     mean = mean, 
                     sd = sd
                  )
               );
            }, 
            pred = self$objFunc$synthPrediction,
            mean = self$mean,
            sd = self$sd,
            SIMPLIFY = FALSE
         )));
      }
   )
);

#' @title Process normally distribute synthetic error
#' 
#' @description  
#' Generates synthetic observations for an objective function
#' based on the current prediction and independent, normally-distributed
#' error.
#' 
#' @name SynthErrorProcessor_process
#' @return An object representing the synthetic observations 
NULL

# Class ObjectiveFunction (R6) ####

#' Abstract objective function class
#' 
#' A class representing an objective function for calculating a 
#' value that represents to what degree a model prediction matches
#' an observation.
#' This class is abstract and is not intended to be instantiated
#' directly. The constructor is only intended to be called by
#' an extending subclass.
#' 
#' @export
#' @usage \code{ObjectiveFunction$new(...)}
#' @param model The model used to generate the predictions to be
#'    compared to the observations by the objective function
#' @param parameterProcessor A parameter processor object that is capable
#'    of translating a simple vector of parameter values and inserting them
#'    into the appropriate attributes of the model object, such that
#'    the following execution of the model will generation a prediction
#'    corresponding to those parameter values.
#' @param predictionProcessor A prediction processor object that is capable
#'    of extracting a set of simple vectors from the model output, which can
#'    then be compared to the observations in calculation of the objective
#'    function value.
#' @param synthErrorProcessor An optional synthetic error processor object 
#'    that can generate a synthetic observation based on some known structure
#'    in error. By default this is null.  By setting to a valid object, this
#'    will create a synthetic prediction from the current model configuration,
#'    and then create a synthetic observation upon construction of the
#'    objective function object. This feature and the "realize" method for
#'    generating a new realization of synthetic error is designed to 
#'    facilitatin Monte Carlo error propagation algorithms.
#' @param observation The observations to compare to the predictions
#'    by the objective function. Not that any observations provided as an
#'    argument will be overwritten if a valid synthetic error processor is
#'    provided. This argument defaults to a null value, so it is optional
#'    if a synthetic error processor is provided. The object cannot be constructed
#'    if the synthErrorProcessor and observation arguments are both NULL.
ObjectiveFunction <- R6::R6Class(
   classname = "ObjectiveFunction",
   public = list(
      params = NULL,
      parameterProcessor = NULL,
      model = NULL,
      prediction = NULL,
      predictionProcessor = NULL,
      synthPrediction = NULL,
      observation = NULL,
      synthErrorProcessor = NULL,
      multivariateValues = NULL,
      value = NULL,
      initialize = function(
         model,
         parameterProcessor,
         predictionProcessor,
         synthErrorProcessor = NULL,
         observation = NULL
      ) 
      {
         self$model <- model;
         self$parameterProcessor <- parameterProcessor;
         if(is.null(self$parameterProcessor$model)) {
            self$parameterProcessor$model <- self$model;
         }
         self$predictionProcessor <- predictionProcessor;
         if(is.null(self$predictionProcessor$model)) {
            self$predictionProcessor$model <- self$model;
         }
         
         self$synthErrorProcessor <- synthErrorProcessor;
         if (!is.null(self$synthErrorProcessor)) {
            self$synthErrorProcessor$objFunc <- self;
            self$model$run();
            self$prediction <- self$predictionProcessor$process();
            self$synthPrediction <- self$prediction;
            self$realize();
         } else {
            if (is.null(observation)) {
               stop(paste("Synthetic error processor and",
                          "observation arguments cannot both be NULL."));
            }
            self$observation <- observation;
         }
      },
      propose = function(params)
      {
         self$params <- params;
         self$parameterProcessor$process(params = params);
         self$model$run();
         self$prediction <- self$predictionProcessor$process();
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
      realize = function()
      {
         self$observation <- self$synthErrorProcessor$process();
      },
      compare = function(params) 
      {
         stop("Abstract function 'compare' has not been implemented.");
      }
   )
);

#' @title Propose a model 
#' 
#' @description 
#' Proposes a model with a given permutation represented by a subset of
#' input
#' 
#' @name ObjectiveFunction_propose
#' @param params 
#'    The subset of input being proposed as a permutation
#' @return The value of the objective function 
NULL

#' @title Realize a synthetic observation
#' 
#' @description 
#' Generates a new realization of the observation based on the synthetic
#' error processor provided. Will cause an error if a synthetic error processor
#' was not provided in construction of the object.
#' 
#' @name ObjectiveFunction_realize
#' @return The synthetic observation created
NULL

#' @title Compare the prediction and observation (abstract) 
#' 
#' @description 
#' A class that inherits from ObjectiveFunction must implement a 
#' "compare" method that will compare a prediciton to an observation. 
#' 
#' This method declaration will cause a program to fail if 
#' the method is called and the implementing class does not override it.
#' 
#' @name ObjectiveFunction_compare
#' @param params
#'    The object representing model parameters in the event that 
#'    parameters are necessary for the comparison logic
#' @return The value calculated from the comparison 
NULL
