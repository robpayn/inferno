# Package dependencies ####

#' @importFrom R6 R6Class
NULL

# Class LogLikelihoodAR1 (R6) ####

#' @export
#' 
#' @title 
#'   Log likelihood based on first order autocorrelation
#'   
LogLikelihoodAR1 <- R6Class(
   classname = "LogLikelihoodAR1",
   inherit = LogLikelihood,
   public = list(
         arCoeff = NULL,
         initialize = function(..., arCoeff)
            {
               super$initialize(...);
               self$arCoeff <- arCoeff;
            }
      )
);

LogLikelihoodAR1$set(
   which = "public",
   name = "compare",
   value = function(params)
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
);
