# Package dependencies ####

#' @importFrom R6 R6Class
NULL

# Class BayesLogLikelihood (R6) ####

#' @export
#' 
#' @title 
#'    Bayes posterior likelihood objective function class
#' 
#' @description 
#'   Provides the tools for calculating a Bayes posterior 
#'   likelihood value for the comparison of a model prediction and
#'   observations. The Bayes likelihood is calculated by adding
#'   the summed log likelihood from prior distributions of
#'   parameters to the log likelihood of a provided objective 
#'   function.
#'   Usage below is for the class constructor method.
#' 
#' @usage 
#'    BayesLogLikelihood$new(..., <arguments>)
#' @param ... 
#'    Additional required arguments for the constructor 
#'    of the superclass \code{ObjectiveFunction} must be provided. Optional
#'    arguments are also available for various features. 
#'    See documentation for the class \code{\link{ObjectiveFunction}} for a description
#'    of the additional required and optional arguments.
#' @param paramDists A list of random variables representing the prior
#'    prior probabilites for the parameters being estimated
#' @param logLikelihood The objective function that will calculate the 
#'    fit metric (most like a log likelihood) that will be added to the
#'    sum of the prior log likelihoods to generate the overall objective
#'    function value
#' @param negate Optional switch to negate the objective function value
#'    to adjust for algorthims that minimize or maximize objective funciton
#'    values
#'    
#' @return The object of class \code{BayesLogLikelihood} created
#'    by the constructor
#'    
#' @seealso 
#'    Extends the R6 class \code{\link{ObjectiveFunction}}
#'    
#' @section Extends:
#'   \code{\link{ObjectiveFunction}}
#'    
#' @section Methods: 
#'   $new
#'   $compare - \code{\link{BayesLogLikelihood_compare}} \cr
#'   $propose - \code{\link{BayesLogLikelihood_propose}} \cr
#'   $realize - \code{\link{BayesLogLikelihood_realize}} \cr
#'   
BayesLogLikelihood <- R6Class(
   classname = "BayesLogLikelihood",
   inherit = ObjectiveFunction,
   public = list(
      paramDists = NULL,
      logLikelihood = NULL,
      logPriors = NULL,
      negate = NULL,
      initialize = function
         (
            paramDists, 
            logLikelihood, 
            negate = FALSE
         ) 
         {
            # Set the values of attributes based on
            # arguments to the constructor
            self$model <- logLikelihood$model;
            self$simulator <- logLikelihood$simulator;
            self$negate <- negate;
            self$paramDists <- paramDists;
            self$logLikelihood <- logLikelihood;
            self$synthPrediction <- logLikelihood$synthPrediction;
            self$observation <- logLikelihood$observation;
         }
   )
);

# Method BayesLogLikelihood$propose ####

#' @name BayesLogLikelihood_propose
#' 
#' @title 
#'    Propose a model 
#' 
#' @description 
#'   Overrides the superclass propose method to call the base objective 
#'   function propose method, before adding the log of the prior probabilities
#'   to produce the Bayesian posterior likelihood.
#' 
#'   Proposes a model with a given permutation represented by a subset of
#'   input
#' 
#' @usage 
#'    [Object]$propose(params)
#' @param params 
#'    The subset of input being proposed as a permutation
#'    
#' @return The value of the objective function 
#' 
#' @section Method of class:
#'   \code{\link{BayesLogLikelihood}}
#'   
#' @section Overrides:  
#'    \code{\link{ObjectiveFunction_propose}}
#'    
BayesLogLikelihood$set(
   which = "public",
   name = "propose",
   value = function(params)
      {
         # Override the implementation of propose to allow for
         # operation of the base objective function calculations
         self$params <- params;
         self$logLikelihood$propose(params);
         self$prediction <- self$logLikelihood$prediction;
         if(is.na(self$logLikelihood$value)) {
            self$value <- self$logLikelihood$value;
         } else {
            self$value <- self$compare(params);
         }
         return(self$value);
      }
);

# Method BayesLogLikelihood$realize ####

#' @name 
#'    BayesLogLikelihood_realize
#' @title 
#'    Realize a synthetic observation
#' 
#' @description 
#'   Overrides the superclass realize method to be sure realize is called
#'   for the base objective function and the observation attribute is set
#'   appropriately.
#'   
#' @usage 
#'    [Object]$realize()
#'    
#' @return 
#'    The synthetic observation created
#'    
#' @section Method of class: 
#'   \code{\link{BayesLogLikelihood}}
#'   
#' @section Overrides:
#'   \code{\link{ObjectiveFunction_realize}}
#'    
BayesLogLikelihood$set(
   which = "public",
   name = "realize",
   value = function()
      {
         # Override the implementation of realize to allow for
         # operation of the base objective function calculations
         self$logLikelihood$realize();
         self$observation <- self$logLikelihood$observation;
      }
);

# Method BayesLogLikelihood$compare ####

#' @name BayesLogLikelihood_compare
#' 
#' @title 
#'    Compare the prediction and observation
#' 
#' @description 
#'   Compares a list of prediction vectors to an associated list of
#'   observation vectors based on a Bayesian posterior likelihood.
#' 
#'   This objective function uses a base objective function that to which 
#'   the logarithms of prior probabilities are added to for a Bayesian 
#'   objective context. For example, a formal Bayes likelihood based on
#'   independent and normally distributed error would use a formal log
#'   likelihood as the base objective function.
#' 
#'   Note that handling of NA values depend on the "ignore.na" attribute
#'   that is configurable in the constructor. By default NAs are ignored.
#' 
#' @usage 
#'    [Object]$compare(params)
#' @param params
#'    Any "NaN" values in the sd attribute will be replaced by parameters
#'    from the end of the parameter vector. (This feature is critical
#'    if these standard deviations are being inferred as part of an
#'    inferential modeling algorithm).
#'    
#' @return 
#'    The log likelihood (will be negated if negate attribute is TRUE)
#'    
#' @section Method of class:
#'   \code{\link{BayesLogLikelihood}}
#'    
#' @section Implements: 
#'   \code{\link{ObjectiveFunction_compare}}
#'    
BayesLogLikelihood$set(
   which = "public",
   name = "compare",
   value = function(params)
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
            return(self$logLikelihood$value - sum(self$logPriors));
         } else {
            return(self$logLikelihood$value + sum(self$logPriors));
         }
      }
);

# Class StatsLoggerBayes (R6) ####

#' @export
#' 
#' @title 
#'    Markov Chain Bayesian stats logging tool
#' 
#' @description 
#'   Extends \code{AdaptiveMCMCStatsLogger} to include stats specific to a Bayesian likelihood.
#'   Usage below is for the class constructor method.
#' 
#' @usage 
#'    StatsLoggerBayes$new(filePath = NULL, statsFile = "stats.csv")
#' @param filePath
#'    Optional argument to set the path to which output files are written
#' @param statsFile
#'    Name of the file with logged stats data
#'    
#' @return 
#'    The object of class \code{StatsLoggerBayes} created
#'    by the constructor
#'    
#' @section Extends: 
#'    \code{\link{AdaptiveMCMCStatsLogger}}
#'    
#' @section Methods: 
#'   $new
#'   $buildLog - see \code{\link{StatsLoggerBayes_buildLog}} \cr
#'   $logAccepted - see \code{\link{StatsLoggerBayes_logAccepted}} \cr
#'   $logRejected - see \code{\link{StatsLoggerBayes_logRejected}} \cr
#'   
StatsLoggerBayes <- R6Class(
   classname = "StatsLoggerBayes",
   inherit = AdaptiveMCMCStatsLogger,
   public = list(
      likelihoodColumn = NULL,
      propLikelihoodColumn = NULL
      )
);

# Method StatsLoggerBayes$buildLog ####

#' @name 
#'    StatsLoggerBayes_buildLog
#' @title 
#'    Build the logger data structures and files
#' 
#' @description 
#'   Overrides the superclass buildLog method to add the structures needed
#'   for tracking Bayesian specific statistics
#' 
#' @usage 
#'    [Object]$buildLog(numRows, objFunc, filePath = "./output")
#' @param numRows
#'    Number of rows of data expected in the log
#' @param objFunc
#'    The objective function with the statistics to be logged
#' @param filePath
#'    The file path where log files should be created
#'    
#' @return 
#'    No meaningful return value
#'    
#' @section Method of class:
#'   \code{\link{StatsLoggerBayes}}
#'   
#' @section Overrides:
#'   \code{\link{AdaptiveMCMCStatsLogger_buildLog}}
#'   
StatsLoggerBayes$set(
   which = "public",
   name = "buildLog",
   value = function(numRows, ...)
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
         self$likelihoodColumn <- length(self$stats) - 1;
         self$propLikelihoodColumn <- length(self$stats) ;
      }
);

# Method StatsLoggerBayes$logAccepted ####

#' @name StatsLoggerBayes_logAccepted
#' 
#' @title 
#'   Create accepted log entries
#' 
#' @section Method of class:
#'   \code{\link{StatsLoggerBayes}}
#'   
#' @section Overrides:
#'   \code{\link{AdaptiveMCMCStatsLogger_logAccepted}}
#'   
StatsLoggerBayes$set(
   which = "public",
   name = "logAccepted",
   value = function(index)
      {
         super$logAccepted(index);
         self$stats[index, self$propLikelihoodColumn] <- 
            self$objFunc$logLikelihood$value;
         self$stats[index, self$likelihoodColumn] <- 
            self$objFunc$logLikelihood$value;
      }
);

# Method StatsLoggerBayes$logRejected ####

#' @name StatsLoggerBayes_logRejected
#' 
#' @title 
#'   Create rejected log entries
#' 
#' @section Method of class:
#'   \code{\link{StatsLoggerBayes}}
#'   
#' @section Overrides:
#'   \code{\link{AdaptiveMCMCStatsLogger_logRejected}}
#'   
StatsLoggerBayes$set(
   which = "public",
   name = "logRejected",
   value = function(index)
      {
         super$logRejected(index);
         self$stats[index, self$propLikelihoodColumn] <- 
            self$objFunc$logLikelihood$value;
         self$stats[index, self$likelihoodColumn] <- 
            self$stats[index - 1, self$likelihoodColumn];
      }
);
