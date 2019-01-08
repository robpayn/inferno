# Package dependencies ####

# All Likelihood objective function classes are R6 classes.
library(R6);

# Class LogLikelihood (R6) ####

#' @title Log likelihood objective function class
#' 
#' @description
#' Provides the tools for calculating a log likelihood
#' value for the comparison of a model prediction and
#' observations.
#' 
#' @export
#' @usage \code{LogLikelihood$new(...)}
#' @param ... 
#'    Arguments passed to constructor \code{LogLikelihood$new(...)} will be 
#'    passed generically to the constructor for the superclass \code{ObjectiveFunction}. 
#'    See documentation for the class \code{\link{ObjectiveFunction}} for a description
#'    of these arguments.
#' @param sd 
#'    A vector of standard deviations to be used for calculating the 
#'    likelihood. There should be the same number of standard deviations as
#'    the number of columns in the dataframes of predictons and observations
#'    to be compared by the objective function.
#' @param negate 
#'    A boolean switch indicating if objective function value
#'    should be negated for switching between maximization or minimization
#'    algorithms
#' @param ignore.na
#'    Determines if NA elements will be ignored in objective function value calculation.
#'    Default is TRUE (ignore NAs in calculation)
#' @return The object of class \code{LogLikelihood} created
#'    by the constructor
LogLikelihood <- R6::R6Class(
   classname = "LogLikelihood",
   inherit = ObjectiveFunction,
   public = list(
      sd = NULL,
      negate = NULL,
      ignore.na = NULL,
      initialize = function(..., sd, negate = FALSE, ignore.na = TRUE)
      {
         super$initialize(...);
         self$sd <- sd;
         self$negate <- negate;
         self$ignore.na <- ignore.na;
      },
      compare = function(params)
      {
         sd <- self$sd;
         estimateSD <- is.nan(sd);
         if(any(estimateSD)) {
            replaceIndices <- which(estimateSD);
            paramIndices <- 
               (length(params) - length(replaceIndices) + 1):length(params);
            sd[replaceIndices] <- params[paramIndices];
         }
         logLike <- mapply(
            FUN = function(p, o, sd) 
            {
               sum(
                  dnorm(
                     x = o,
                     mean = p,
                     sd = sd,
                     log = TRUE
                  ),
                  na.rm = self$ignore.na
               )
            },
            p = self$prediction,
            o = self$observation,
            sd = as.list(sd)
         );
         if (self$negate) {
            return(-logLike);
         } else {
            return(logLike);
         }
      }         
   )
);

#' @title Compare the prediction and observation
#' 
#' @description 
#' Compares a list of prediction vectors to an associated list of
#' observation vectors based on a formal log likelihood function
#' based on a normal distribution of error.
#' 
#' Note that handling of NA values depend on the "ignore.na" attribute
#' that is configurable in the constructor. By default NAs are ignored.
#' 
#' @name LogLikelihood_compare
#' @param params
#'    Any "NaN" values in the sd attribute will be replaced by parameters
#'    from the end of the parameter vector. (This feature is critical
#'    if these standard deviations are being inferred as part of an
#'    inferential modeling algorithm).
#' @return The log likelihood (will be negated if negate attribute is TRUE)
NULL

# Class BayesLogLikelihood (R6) ####

#' @title Bayes posterior likelihood objective function class
#' 
#' @description 
#' Provides the tools for calculating a Bayes posterior 
#' likelihood value for the comparison of a model prediction and
#' observations. The Bayes likelihood is calculated by adding
#' the summed log likelihood from prior distributions of
#' parameters to the log likelihood of a provided objective 
#' function.
#' 
#' @export
#' @usage \code{BayesLogLikelihood$new(...)}
#' @param paramDists A list of random variables representing the prior
#'    prior probabilites for the parameters being estimated
#' @param baseObjFunc The objective function that will calculate the 
#'    fit metric (most like a log likelihood) that will be added to the
#'    sum of the prior log likelihoods to generate the overall objective
#'    function value
#' @param negate Optional switch to negate the objective function value
#'    to adjust for algorthims that minimize or maximize objective funciton
#'    values
#' @return The object of class \code{BayesLogLikelihood} created
#'    by the constructor
BayesLogLikelihood <- R6::R6Class(
   classname = "BayesLogLikelihood",
   inherit = ObjectiveFunction,
   public = list(
      paramDists = NULL,
      baseObjFunc = NULL,
      logPriors = NULL,
      negate = NULL,
      initialize = function(
         paramDists, 
         baseObjFunc, 
         negate = FALSE
      ) 
      {
         # Parameter and prediction processors are not 
         # necessary
         self$parameterProcessor <- NULL;
         self$predictionProcessor <- NULL;
         
         # Set the values of attributes based on
         # arguments to the constructor
         self$model <- baseObjFunc$model;
         self$negate <- negate;
         self$paramDists <- paramDists;
         self$baseObjFunc <- baseObjFunc;
         self$synthPrediction <- baseObjFunc$synthPrediction;
         self$observation <- baseObjFunc$observation;
      },
      propose = function(params)
      {
         # Override the implementation of propose to allow for
         # operation of the base objective function calculations
         self$params <- params;
         self$baseObjFunc$propose(params);
         if(is.na(self$baseObjFunc$value)) {
            self$value <- self$baseObjFunc$value;
         } else {
            self$value <- self$compare(params);
         }
         return(self$value);
      },
      realize = function()
      {
         # Override the implementation of realize to allow for
         # operation of the base objective function calculations
         self$baseObjFunc$realize();
         self$observation <- self$baseObjFunc$observation;
      },
      compare = function(params)
      {
         # Calculate the log of the prior likelihoods
         self$logPriors <- mapply(
            FUN = function(paramDist, param)
            {
               return(paramDist$density(param, log = TRUE));   
            },
            paramDist = self$paramDists,
            param = params
         );
         
         # Sum the priors with the result from the base objective
         # function (depending on value of negate switch)
         if (self$negate) {
            return(self$baseObjFunc$value - sum(self$logPriors));
         } else {
            return(self$baseObjFunc$value + sum(self$logPriors));
         }
      }
   )
);

#' @title Propose a model 
#' 
#' @description 
#' Overrides the superclass propose method to call the base objective 
#' function propose method, before adding the log of the prior probabilities
#' to produce the Bayesian posterior likelihood.
#' 
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
#' Overrides the superclass realize method to be sure realize is called
#' for the base objective function and the observation attribute is set
#' appropriately.
#' 
#' Generates a new realization of the observation based on the synthetic
#' error processor provided. Will cause an error if a synthetic error processor
#' was not provided in construction of the object.
#' 
#' @name BayesLogLikelihood_realize
#' @return The synthetic observation created
NULL

#' @title Compare the prediction and observation
#' 
#' @description 
#' Compares a list of prediction vectors to an associated list of
#' observation vectors based on a Bayesian posterior likelihood.
#' 
#' This objective function uses a base objective function that to which 
#' the logarithms of prior probabilities are added to for a Bayesian 
#' objective context. For example, a formal Bayes likelihood based on
#' independent and normally distributed error would use a formal log
#' likelihood as the base objective function.
#' 
#' Note that handling of NA values depend on the "ignore.na" attribute
#' that is configurable in the constructor. By default NAs are ignored.
#' 
#' @name BayesLogLikelihood_compare
#' @param params
#'    Any "NaN" values in the sd attribute will be replaced by parameters
#'    from the end of the parameter vector. (This feature is critical
#'    if these standard deviations are being inferred as part of an
#'    inferential modeling algorithm).
#' @return The log likelihood (will be negated if negate attribute is TRUE)
NULL

# Class StatsLoggerBayes (R6) ####

#' Markov Chain Bayesian stats logging tool
#' 
#' Extends \code{StatsLogger} to include stats specific to a Bayesian likelihood
#' 
#' @export
#' @usage \code{StatsLoggerBayes$new(...)} (constructor)
#' @param filePath
#'    Optional argument to set the path to which output files are written
#' @param statsFile
#'    Name of the file with logged stats data
#' @return The object of class \code{StatsLogger} created
#'    by the constructor
StatsLoggerBayes <- R6::R6Class(
   classname = "StatsLoggerBayes",
   inherit = StatsLogger,
   public = list(
      buildLog = function(numRows, ...)
      {
         super$buildLog(numRows, ...);
         names(self$stats)[1] <- "posterior";
         names(self$stats)[2] <- "propPosterior";
         self$stats <- cbind(
            self$stats,
            data.frame(
               likelihood = numeric(length = self$numRows),
               propLikelihood = numeric(length = self$numRows)
            )
         );
      },
      logProposed = function(index)
      {
         super$logProposed(index);
         self$stats[index, 5] <- self$objFunc$baseObjFunc$value;
      },
      logAccepted = function(index)
      {
         super$logAccepted(index);
         self$stats[index, 4] <- self$objFunc$baseObjFunc$value;
      },
      logRejected = function(index)
      {
         super$logRejected(index);
         self$stats[index, 4] <- self$stats[index - 1, 4];
      }
   )
);
