# Package dependencies ####

#' @importFrom R6 R6Class
NULL

# Class LogLikelihood (R6) ####

#' @export
#' 
#' @title 
#'    A log likelihood objective function class (R6 class generator)
#' 
#' @description
#' The R6 class generator for a log likelihood objective function
#' object used by optimization algorithms. 
#' For example, an instance of this class would be the objective function 
#' used for in Maximum Likelihood Estimate (MLE) analysis.
#' Usage below is for the class constructor method.
#' 
#' @usage 
#'    LogLikelihood$new(..., sd, negate = FALSE, ignore.na = TRUE)
#' @param ... 
#'    Additional required arguments for the constructor 
#'    of the superclass \code{ObjectiveFunction} must be provided. Optional
#'    arguments are also available for various features. 
#'    See documentation for the class \code{\link{ObjectiveFunction}} for a description
#'    of the additional required and optional arguments.
#' @param sd 
#'    A vector of standard deviations to be used for calculating the 
#'    likelihood. This vector shoud have the an element with a standard deviation 
#'    corresponding to each of the columns in the dataframes containing the 
#'    predictons and observations that are compared by the objective function.
#' @param negate 
#'    A boolean switch indicating if the objective function value
#'    should be negated for switching between maximization or minimization
#'    algorithms. Note that this is a log likelihood, so negation is 
#'    equivalent to inverting the probability representing the raw likelihood.
#'    Default value if FALSE (log likelihood is not negated).
#' @param ignore.na
#'    Determines if NA elements will be ignored in objective function value calculation.
#'    Default is TRUE (ignore NAs in calculation).
#'    
#' @return The object of class \code{LogLikelihood} created
#'    by the constructor
#'    
#' @seealso 
#'    Extends the R6 class \code{\link{ObjectiveFunction}}
#'    
#'    Methods: \code{\link{LogLikelihood_compare}}
LogLikelihood <- R6Class(
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

# Roxygen Method LogLikelihood_compare ####

#' @name 
#'    LogLikelihood_compare
#' @title 
#'    Compare the prediction and observation with a log likelihood
#' 
#' @description 
#' Compares a list of prediction vectors to an associated list of
#' observation vectors based on a formal log likelihood function
#' and assumption of a normal distribution of residual error.
#' 
#' Note that handling of NA values depend on the "ignore.na" attribute
#' that is configurable in the constructor. By default NAs are ignored.
#' 
#' @usage 
#'    [Object]$compare(params)
#' @param params
#'    Any "NaN" values in the sd attribute will be replaced by parameters values
#'    from the end of the parameter vector. This feature is critical
#'    if these standard deviations are being inferred as part of an
#'    inferential modeling algorithm, rather than being provided as an
#'    attribute of the object.
#'    
#' @return The log likelihood value from the comparison
#'    (will be negated if negate attribute of the object is TRUE)
#'    
#' @seealso 
#'    Method of the R6 class \code{\link{LogLikelihood}};
#'    Implements the abstract method \code{\link{ObjectiveFunction_compare}}
NULL

# Class BayesLogLikelihood (R6) ####

#' @export
#' 
#' @title 
#'    Bayes posterior likelihood objective function class
#' 
#' @description 
#' Provides the tools for calculating a Bayes posterior 
#' likelihood value for the comparison of a model prediction and
#' observations. The Bayes likelihood is calculated by adding
#' the summed log likelihood from prior distributions of
#' parameters to the log likelihood of a provided objective 
#' function.
#' Usage below is for the class constructor method.
#' 
#' @usage 
#'    BayesLogLikelihood$new(..., paramDists, baseObjFunc, negate = FALSE)
#' @param ... 
#'    Additional required arguments for the constructor 
#'    of the superclass \code{ObjectiveFunction} must be provided. Optional
#'    arguments are also available for various features. 
#'    See documentation for the class \code{\link{ObjectiveFunction}} for a description
#'    of the additional required and optional arguments.
#' @param paramDists A list of random variables representing the prior
#'    prior probabilites for the parameters being estimated
#' @param baseObjFunc The objective function that will calculate the 
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
#'    Methods: \code{\link{BayesLogLikelihood_compare}}; 
#'       \code{\link{BayesLogLikelihood_propose}};
#'       \code{\link{BayesLogLikelihood_realize}}
BayesLogLikelihood <- R6Class(
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
         self$prediction <- self$baseObjFunc$prediction;
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

# Roxygen Method BayesLogLikelihood_propose ####

#' @name 
#'    BayesLogLikelihood_propose
#' @title 
#'    Propose a model 
#' 
#' @description 
#' Overrides the superclass propose method to call the base objective 
#' function propose method, before adding the log of the prior probabilities
#' to produce the Bayesian posterior likelihood.
#' 
#' Proposes a model with a given permutation represented by a subset of
#' input
#' 
#' @usage 
#'    [Object]$propose(params)
#' @param params 
#'    The subset of input being proposed as a permutation
#'    
#' @return The value of the objective function 
#' 
#' @seealso 
#'    Method of the R6 class \code{\link{BayesLogLikelihood}};
#'    Overrides the method \code{\link{ObjectiveFunction_propose}}
NULL

# Roxygen Method BayesLogLikelihood_realize ####

#' @name 
#'    BayesLogLikelihood_realize
#' @title 
#'    Realize a synthetic observation
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
#' @usage 
#'    [Object]$realize()
#'    
#' @return 
#'    The synthetic observation created
#'    
#' @seealso 
#'    Method of the R6 class \code{\link{BayesLogLikelihood}}; 
#'    Overrides the method \code{\link{ObjectiveFunction_realize}}
NULL

# Roxygen Method BayesLogLikelihood_compare ####

#' @name 
#'    BayesLogLikelihood_compare
#' @title 
#'    Compare the prediction and observation
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
#' @seealso 
#'    Method of the R6 class \code{\link{BayesLogLikelihood}}; 
#'    Implements the abstract method \code{\link{ObjectiveFunction_compare}}
NULL

# Class StatsLoggerBayes (R6) ####

#' @export
#' 
#' @title 
#'    Markov Chain Bayesian stats logging tool
#' 
#' @description 
#' Extends \code{StatsLogger} to include stats specific to a Bayesian likelihood.
#' Usage below is for the class constructor method.
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
#' @seealso 
#'    Extends the R6 class \code{\link{StatsLogger}}
#'    
#'    Methods: \code{\link{StatsLoggerBayes_buildLog}}; 
#'       \code{StatsLoggerBayes_logAccepted};
#'       \code{StatsLoggerBayes_logProposed};
#'       \code{StatsLoggerBayes_logRejected};
StatsLoggerBayes <- R6Class(
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

# Roxygen Method StatsLoggerBayes_buildLog ####

#' @name 
#'    StatsLoggerBayes_buildLog
#' @title 
#'    Build the logger data structures and files
#' 
#' @description 
#' Overrides the superclass buildLog method to add the structures needed
#' for tracking Bayesian specific statistics
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
#' @seealso 
#'    Method of the R6 class \code{\link{StatsLoggerBayes}}; 
#'    Overrides the method \code{\link{StatsLogger_buildLog}}
NULL
