# Package dependencies ####

#' @importFrom R6 R6Class
#' @importFrom MASS mvrnorm
NULL

# Class RandomVariable (R6) ####

#' @export
#' 
#' @title 
#'    R6 class defining a random variable
#' 
#' @description
#'    A class representing a random variable with a given distribution.
#'    This class is abstract and is not intended to be instantiated
#'    directly.
#' 
RandomVariable <- R6Class(
   classname = "RandomVariable",
   public = list(
      
      # Method RandomVarible$density ####
      #
      #' @description 
      #'    Provides the probability density for a given value in the 
      #'    distribution of the random variable.
      #' 
      #' @param val 
      #'    The value for which the probability density is requested
      #' @param log 
      #'    A boolean switch for requesting the log probability density
      #'    
      #' @return 
      #'    The probability density of the value requested 
      #'    
      density = function(val, log)
      {
         stop("Method RandomVariable$density has not been implemented");   
      },
      
      # Method RandomVarible$markovStep ####
      #
      #' @description 
      #'    Provides a new location in multivariate space based on a previous
      #'    location provided as a vector of variable values
      #' 
      #' @param vector 
      #'    Previous location in multivariate space
      #' @param ...
      #'    Generic arguments needed by internal functions
      #'    
      #' @return 
      #'    Next location in multivariate space
      #'    
      markovStep = function(vector, ...)
      {
         stop("Method RandomVariable$markovStep has not been implemented");   
      },
      
      
      # Method RandomVarible$normalizedFit ####
      #
      #' @description 
      #'    Resets the variance statistics for the distribuiton that match
      #'    the structure of provided data
      #' 
      #' @param x 
      #'    Data to be fit
      #'    
      #' @return 
      #'    The variance statistics providing the best fit of the 
      #'    distribution to the data
      #'    
      normalizedFit = function(x)
      {
         stop("Method RandomVarible$normalizedFit has not been implemented");   
      }
   )
);


# Class RVUniform (R6) ####

#' @export
#' 
#' @title 
#'    R6 class defining a uniform random variable
#' 
#' @description 
#'    Provides tools for working with a random variable with
#'    a uniform distribution defined by minimum and maximum
#'    values
#' 
RVUniform <- R6Class(
   classname = "RVUniform",
   inherit = RandomVariable,
   public = list(
      
      #' @field min
      #'   Minimum value
      min = NULL,
      
      #' @field max
      #'   Maximum value
      max = NULL,
      
      # Method RVUniform$new ####
      #
      #' @description 
      #'   Construct a new instance of the class
      #' 
      #' @param min 
      #'    Mininum value for the uniform distribution
      #' @param max 
      #'    Maximum value for the uniform distribution
      #'    
      initialize = function(min, max)
      {
         self$min <- min;
         self$max <- max;
      },
      
      # Method RVUniform$density ####
      #
      #' @description 
      #'    Method to provide the probability density for a given value in the 
      #'    uniform distribution represented by the object
      #' 
      #' @param val 
      #'    The value for which the probability density is requested (numeric vector)
      #' @param log 
      #'    A boolean switch for requesting the log probability density.
      #'    Defaults to FALSE.
      #'    
      #' @return 
      #'    The probability density of the value requested (numeric vector).
      #'    
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

# Class RVNormal (R6) ####

#' @export
#' 
#' @title 
#'    R6 class defining a normal random variable
#' 
#' @description 
#'    Provides tools for working with a random variable with
#'    a normal distribution defined by a mean and standard deviation
#' 
RVNormal <- R6Class(
   classname = "RVNormal",
   inherit = RandomVariable,
   public = list(
      
      #' @field mean
      #'   The mean of the normal distribution
      mean = NULL,
      
      #' @field sd
      #'   The standard deviation of the normal distribution
      sd = NULL,
      
      # Method RVNormal$new ####
      #
      #' @description 
      #'    Construct a new instance of the class.
      #'    
      #' @param mean 
      #'    Mean of the distribution
      #' @param sd 
      #'    Standard deviation of the uniform distribution
      #'    
      initialize = function(mean, sd)
      {
         self$mean <- mean;
         self$sd <- sd;
      },
      
      # Method RVNormal$density ####
      #
      #' @description 
      #'    Method to provide the probability density for a given value in the 
      #'    normal distribution represented by the object
      #' 
      #' @param val 
      #'    The value for which the probability density is requested
      #' @param log 
      #'    A boolean switch for requesting the log probability density.
      #'    Default value is FALSE.
      #'    
      #' @return 
      #'    The probability density of the value requested.
      #'    
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


# Class RVMultivariateNormal (R6) ####

#' @export
#' 
#' @title 
#'    R6 class defining a multivariate normal distribution
#' 
#' @description 
#'    Provides tools for working with a multivariate random variable with
#'    a joint normal distribution defined by a covariance matrix
#' 
RVMultivariateNormal <- R6Class(
   classname = "RVMultivariateNormal",
   inherit = RandomVariable,
   public = list(
      
      #' @field means
      #'   Vector of means for the multivariate normal distribution
      means = NULL,
      
      #' @field covariances
      #'   Matrix of covariances for the multivariate normal distribution
      covariances = NULL,
      
      #' @field adjustCovarianceFactor
      #'   A scalar factor for adjusting the covariance in normalized fits
      adjustCovarianceFactor = NULL,
      
      #' @field tinyIdentMatrix
      #'   A identity matrix used to be sure normalized fits do not have
      #'   zero values
      tinyIdentMatrix = NULL,
      
      # Method RVMultivariateNormal$new ####
      #
      #' @description 
      #'   Construct a new instance of the class.
      #'   
      #' @param covariances 
      #'    Covariance matrix of the variables defining the
      #'    distribution
      #' @param means 
      #'    Optional means of the variables defining the distribution.
      #'    Default is all means are set to zero.
      #' @param tinyIdentFactor
      #'    A very small number used in a diagonal matrix to ensure
      #'    that no values on the diagonal of a fit covariance matrix
      #'    are zero. Defaults to 1E-20.
      #' @param adjustCovarianceFactor
      #'    Allows adjustment of a fit covariance matrix by a scalar
      #'    factor. Defaults to 1.
      #'    
      initialize = function
      (
         covariances,
         means = rep(0, nrow(covariances)), 
         tinyIdentFactor = 1e-20,
         adjustCovarianceFactor = 1
      )
      {
         self$means <- means;
         self$covariances <- covariances;
         self$adjustCovarianceFactor <- adjustCovarianceFactor;
         self$tinyIdentMatrix <- 
            diag(x = nrow(covariances)) * 
            tinyIdentFactor * 
            adjustCovarianceFactor;
      },
      
      # Method RVMultivariateNormal$markovStep ####
      #
      #' @description 
      #'    Provides a new location in multivariate space based on a previous
      #'    location provided as a vector of variable values
      #' 
      #' @param vector 
      #'    Previous location in multivariate space
      #' @param ...
      #'    Arguments passed to the mvrnorm function
      #'    
      #' @return 
      #'    Next location in multivariate space
      #'    
      markovStep = function(vector, ...) 
      {
         return(
            vector +
            mvrnorm(
               n = 1,
               mu = self$means,
               Sigma = self$covariances,
               ...
            )
         );
      },
      
      # Method RVMultivariateNormal$normalizedFit ####
      #
      #' @description 
      #'    Resets the variance statistics for the distribuiton that match
      #'    the structure of provided data
      #' 
      #' @param obsMatrix 
      #'    Data to be fit. Each column in the matrix represents a variable in the
      #'    multivariate distribution.  Each row represents an observation of the
      #'    variables.
      #' @param ...
      #'    Arguments passed to the cov() function
      #'    
      #' @return 
      #'    The variance statistics providing the best fit of the 
      #'    distribution to the data
      #'    
      normalizedFit = function(obsMatrix, ...)
      {
         self$covariances <- 
            cov(obsMatrix, ...) *
            self$adjustCovarianceFactor +
            self$tinyIdentMatrix;
         return(self$covariances);
      }
      
   )
)
