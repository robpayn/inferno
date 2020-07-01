# Package dependencies ####

#' @importFrom R6 R6Class
NULL

# Class LogLikelihoodAR1 (R6) ####

#' @export
#' 
#' @title 
#'   R6 class defining an AR1 log likelihood objective function
#' 
#' @description 
#'   An objective function based on a log likelihood considering a
#'   first-order autocorrelation in the residual error.
#'   
LogLikelihoodAR1 <- R6Class(
   classname = "LogLikelihoodAR1",
   inherit = LogLikelihood,
   public = list(
      
      #' @field arCoeff
      #'   The first-order autoregression coefficient
      arCoeff = NULL,
      
      # Method LogLikelihoodAR1$new ####
      # 
      #' @description 
      #'   Construct a new instance of the class
      #'   
      #' @param ...
      #'   Arguments to pass to the constructor of the super class
      #' @param arCoeff
      #'   The autoregression coefficient
      #'   
      initialize = function(..., arCoeff)
      {
         super$initialize(...);
         self$arCoeff <- arCoeff;
      },
      
      # Method LogLikelihoodAR1$compare ####
      #
      #' @description 
      #'   Overrides the compare method in the super class to add consideration
      #'   of autocorrelation.
      #' 
      #'   Compares a list of prediction vectors to an associated list of
      #'   observation vectors based on a formal log likelihood function
      #'   and assumption of a normal distribution of residual error with
      #'   first-order autocorrelation.
      #' 
      #'   Note that handling of NA values depend on the "ignore.na" attribute
      #'   that is configurable in the constructor. By default NAs are ignored.
      #'   
      #' @param params
      #'   Vector of values for parameters being estimated
      #'   
      #' @return 
      #'   The log likelihood with consideration of first-order autocorrelation
      #'   in error (will be negated if negate attribute is TRUE)
      #'     
      compare = function(params)
      {
         # Replace any estimated error paramaters with their values
         # from the proposed parameter vector
         
         sd <- self$sd;
         replaceSDIndices <- which(is.nan(sd));
         numSDIndices <- length(replaceSDIndices);
         
         ar1 <- self$arCoeff;
         replaceAR1Indices <- which(is.nan(ar1));
         numAR1Indices <- length(replaceAR1Indices);
         
         numParams <- length(params);
         
         if(numSDIndices > 0) {
            paramIndices <- 
               (numParams - numAR1Indices - numSDIndices + 1):
               (numParams - numAR1Indices);
            sd[replaceSDIndices] <- params[paramIndices];
         }
         if(numAR1Indices > 0) {
            paramIndices <- (numParams - numAR1Indices + 1):numParams;
            ar1[replaceAR1Indices] <- params[paramIndices];
         }
         
         logLike <- 0;
         for (column in 1:length(self$observation)) {
            prevResid <- self$observation[[column]][1] - 
               self$prediction[[column]][1];
            logLike <- logLike + dnorm(
               x = prevResid,
               mean = 0,
               sd = sd[column],
               log = TRUE
            );
            
            for (row in 2:length(self$observation[[column]])) {
               residual <- self$observation[[column]][row] - 
                  self$prediction[[column]][row];
               logLike <- logLike + dnorm(
                  x = residual - (ar1[column] * prevResid),
                  mean = 0,
                  sd = sd[column],
                  log = TRUE
               );
               prevResid <- residual;
            }
         }
         
         if (self$negate) {
            return(-logLike);
         } else {
            return(logLike);
         }
      }

   )
)
