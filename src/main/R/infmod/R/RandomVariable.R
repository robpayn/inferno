# Package dependencies ####

#' @importFrom R6 R6Class
#' @importFrom MASS mvrnorm
NULL

# Class RandomVariable (R6) ####

#' @export
#' 
#' @title 
#'    Abstract random variable class (R6)
#' 
#' @description
#'    A class representing a random variable with a given distribution.
#'    This class is abstract and is not intended to be instantiated
#'    directly.
#' 
RandomVariable <- R6Class(
   classname = "RandomVariable",
   public = list(
      density = function(val, log)
         {
            stop("Abstract function 'density' has not been implemented");   
         },
      randomSample = function(n)
         {
            stop("Abstract function 'randomSample' has not been implemented");   
         },
      normalizedFit = function(x)
         {
            stop("Abstract function 'normalizedFit' has not been implemented");   
         }
   )
);

#' @name RandomVariable_density
#' 
#' @title 
#'    Provides the probability density 
#' 
#' @description 
#'    Provides the probability density for a given value in the 
#'    distribution of the random variable.
#' 
#' @param val 
#'    The value for which the probability density is requested
#' @param log 
#'    A boolean switch for requesting the log probability density
#' @return 
#'    The probability density of the value requested 
NULL

#' @name RandomVariable_randomSample
#' 
#' @title 
#'    Randomly samples the distribution 
#' 
#' @description 
#'    Provides a random sample from the distribution. Repeated sampling
#'    should converge on a population that exactly follows the distribution.
#' 
#' @param n 
#'    Number of random samples requested
#' @return 
#'    Random samples from the distribution. 
NULL

#' @name RandomVariable_normalizedFit
#' 
#' @title 
#'    Fit the distribution to data 
#' 
#' @description 
#'    Resets the variance statistics for the distribuiton that match
#'    the structure of provided data
#' 
#' @param x 
#'    Data to be fit
#' @return 
#'    The variance statistics providing the best fit of the 
#'    distribution to the data
NULL


# Class RVUniform (R6) ####

#' @export
#' 
#' @title 
#'    Unifrom random variable class (R6)
#' 
#' @description 
#'    Provides tools for working with a random variable with
#'    a uniform distribution defined by minimum and maximum
#'    values
#' 
#' @usage 
#'    RVUnifrom$new(min, max)
#' @param min 
#'    Mininum value for the uniform distribution
#' @param max 
#'    Maximum value for the uniform distribution
#' @return 
#'    The object of class \code{RVUniform} created
#'    by the constructor
RVUniform <- R6Class(
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

#' @name RVUniform_density
#' 
#' @title 
#'    Provides the probability density
#' 
#' @description 
#'    Method to provide the probability density for a given value in the 
#'    uniform distribution represented by the object
#' 
#' @param val 
#'    The value for which the probability density is requested (numeric vector)
#' @param log 
#'    A boolean switch for requesting the log probability density
#' @return 
#'    The probability density of the value requested (numeric vector)
NULL

# Class RVNormal (R6) ####

#' @export
#' 
#' @title 
#'    Normally distributed random variable class (R6)
#' 
#' @description 
#'    Provides tools for working with a random variable with
#'    a normal distribution defined by a mean and standard deviation
#' 
#' @usage 
#'    RVNormal$new(mean, sd)
#' @param mean 
#'    Mean of the distribution
#' @param sd 
#'    Standard deviation of the uniform distribution
#' @return 
#'    The object of class \code{RVNormal} created
#'    by the constructor
RVNormal <- R6Class(
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

#' @name RVNormal_density
#' 
#' @title 
#'    Provides the probability density
#' 
#' @description 
#'    Method to provide the probability density for a given value in the 
#'    normal distribution represented by the object
#' 
#' @param val 
#'    The value for which the probability density is requested
#' @param log 
#'    A boolean switch for requesting the log probability density
#' @return 
#'    The probability density of the value requested 
NULL

# Class RVMultivariateNormal (R6) ####

#' @export
#' 
#' @title 
#'    Multivariate normally distributed random variable class (R6)
#' 
#' @description 
#'    Provides tools for working with a multivariate random variable with
#'    a joint normal distribution defined by a covariance matrix
#' 
#' @usage 
#'    RVMultivariateNormal$new(means, covariances)
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
#' @return 
#'    The object of class \code{RVMultivariateNormal} created
#'    by the constructor
RVMultivariateNormal <- R6Class(
   classname = "RVMultivariateNormal",
   inherit = RandomVariable,
   public = list(
      means = NULL,
      covariances = NULL,
      adjustCovarianceFactor = NULL,
      tinyIdentMatrix = NULL,
      initialize = function(
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
      randomSample = function(n = 1, ...) 
         {
            return(mvrnorm(
               n = n,
               mu = self$means,
               Sigma = self$covariances,
               ...
            ));
         },
      normalizedFit = function(obsMatrix, zeroMeans = TRUE, ...)
      {
         self$covariances <- 
            cov(obsMatrix, ...) *
            self$adjustCovarianceFactor +
            self$tinyIdentMatrix;
         return(self$covariances);
      }
   )
);

#' @name RVMultivariateNormal_density
#' 
#' @title 
#'    Provides the probability density 
#' 
#' @description 
#'    Provides the probability density for a given value in the 
#'    distribution of the random variable.
#' 
#' @param val 
#'    The value for which the probability density is requested
#' @param log 
#'    A boolean switch for requesting the log probability density
#' @return 
#'    The probability density of the value requested 
NULL

#' @name RVMultivariateNormal_randomSample
#' 
#' @title 
#'    Randomly samples the distribution 
#' 
#' @description 
#'    Provides a random sample from the distribution. Repeated sampling
#'    should converge on a population that exactly follows the distribution.
#' 
#' @param n 
#'    Number of random samples requested
#' @return 
#'    Random samples from the distribution. 
NULL

#' @name RVMultivariateNormal_normalizedFit
#' 
#' @title 
#'    Fit the distribution to data 
#' 
#' @description 
#'    Resets the variance statistics for the distribuiton that match
#'    the structure of provided data
#' 
#' @param obsMatrix 
#'    Data to be fit. Each column in the matrix represents a variable in the
#'    multivariate distribution.  Each row represents an observation of the
#'    variables.
#' @return 
#'    The variance statistics providing the best fit of the 
#'    distribution to the data
NULL