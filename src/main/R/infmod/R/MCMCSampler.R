# Package dependencies ####

#' @importFrom R6 R6Class
NULL

# Class Criterion (R6) ####

#' @title Abstract selection criterion class
#' 
#' @description 
#' A selection criterion object can determine if a given proposal is
#' accepted given the probability of the proposal and a reference probability
#' 
#' This class is abstract and is not intended to be instantiated
#' directly. Inheriting parameter processors are specific to a given 
#' type of model input and the model that takes that input before execution.
#' 
#' @usage Abstract
#' @export
Criterion <- R6Class(
   classname = "Criterion",
   public = list(
      isAccepted = function(prob, probRef)
      {
         stop("Method 'isAccepted' has not been implemented");
      }
   )
);

# Class CriterionMetropLogLikelihood (R6) ####

#' @title Metropolis election criterion class
#' 
#' @description 
#' Uses the Metropolis algorithm to determine if the proposed
#' probability is accepted.
#' 
#' @usage \code{StatsLogger$new(...)} (constructor)
#' @export
CriterionMetropLogLikelihood <- R6Class(
   classname = "CriterionMetropLogLikelihood",
   inherit = Criterion,
   public = list(
      isAccepted = function(prob, probRef)
      {
         if(is.na(prob)) {
            delta <- 0;
         } else {
            delta <- exp(prob - probRef);
         }
         return(runif(n = 1) < delta);
      }
   )
);

# Class StatsLogger (R6) ####

#' @export
#' 
#' @title
#'    Markov Chain stats logging tool
#' 
#' @description
#' Logs the stats from a Markov Chain accpet/reject algorithm. Stats
#' are logged both to a matrix in memory and written to files, if configured
#' to do so.
#' 
#' @usage 
#'    StatsLogger$new(...)
#' @param filePath
#'    Optional argument to set the path to which output files are written
#' @param statsFile
#'    Name of the file with logged stats data
#'    
#' @return The object of class \code{StatsLogger} created
#'    by the constructor
StatsLogger <- R6Class(
   classname = "StatsLogger",
   public = list(
      numRows = NULL,
      objFunc = NULL,
      stats = NULL,
      statsFile = NULL,
      filePath = NULL,
      isLoggingFailedPredictions = NULL,
      initialize = function(
         filePath = NULL, 
         statsFile = "stats.csv",
         isLoggingFailedPredictions = FALSE
         )
         {
            self$filePath <- filePath;
            self$isLoggingFailedPredictions <- 
               isLoggingFailedPredictions;
            if(is.null(self$filePath)) {
               self$statsFile <- statsFile;
            } else {
               self$statsFile <- paste(
                  self$filePath,
                  statsFile,
                  sep = "/"
               );
            }
         },
      buildLog = function(numRows, objFunc, filePath = "./output")
         {
            self$numRows <- numRows;
            self$objFunc <- objFunc;
            self$stats <- data.frame(
               objective = numeric(length = self$numRows),
               propObjective = numeric(length = self$numRows),
               wasAccepted = logical(length = self$numRows)
            );
            if (self$isLoggingFailedPredictions) {
               self$stats <- cbind(
                  self$stats, 
                  data.frame(failedPred = logical(length = self$numRows))
               );
            }
            if(is.null(self$filePath)) {
               self$filePath <- filePath;
               self$statsFile <- paste(
                  self$filePath,
                  self$statsFile,
                  sep = "/"
               );
            }
         },
      logProposed = function(index)
         {
            self$stats[index, 2] <- self$objFunc$value;
         },
      logAccepted = function(index)
         {
            self$stats[index, 1] <- self$objFunc$value;
            self$stats[index, 3] <- TRUE;
            if (self$isLoggingFailedPredictions) {
               self$logFailedPredictions(index);
            }
         },
      logRejected = function(index)
         {
            self$stats[index, 1] <- self$stats[index - 1, 1];
            self$stats[index, 3] <- FALSE;
            if (self$isLoggingFailedPredictions) {
               self$logFailedPredictions(index);
            }
         },
      logFailedPredictions = function(index)
         {
            if (is.null(self$objFunc$prediction)) {
               self$stats[index, 4] <- TRUE;   
            } else {
               self$stats[index, 4] <- FALSE;
            }
         },
      writeFirstRow = function()
      {
         dir.create(
            path = self$filePath, 
            showWarnings = FALSE,
            recursive = TRUE
         );
         write.table(
            x = self$stats[1,], 
            file = self$statsFile, 
            append = FALSE,
            sep = ",",
            col.names = TRUE,
            row.names = FALSE,
            quote = TRUE
         );
      },
      writeRow = function(index)
      {
         write.table(
            x = self$stats[index,], 
            file = self$statsFile, 
            append = TRUE,
            sep = ",",
            col.names = FALSE,
            row.names = FALSE,
            quote = TRUE
         );
      }
   )
);

# Roxygen Method StatsLogger_buildLog ####

#' @name 
#'    StatsLogger_buildLog
#' @title 
#'    Build the logger data structures and files
#' 
#' @description 
#' Creates the data structures and files necessary to track
#' the statistics through a data analysis
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
#'    Method of the R6 class \code{\link{StatsLogger}}; 
NULL

# Class AdaptiveMCMCSampler (R6) ####

#' An Adaptive Markov Chain Monte Carlo Sampler
#' 
#' Provides the tools for executing an optimization using a
#' Markov Chain sampler with an adaptive covariance matrix to determine
#' the step size. the algorithm for accepting or rejecting a sample is
#' configurable, but by default is the Metropolis criterion for
#' log likelihoods.
#' 
#' @export
#' @usage \code{AdaptiveMCMCSampler$new(...)}
#' @param objFunc The objective function used to calculate the base
#'    likelihood.  The sum of the log prior likelihoods are added to this
#'    to generate the overall value of the objective function
#' @param initialParams A vector with initial values for the parameter 
#'    being estimated
#'    (initial location in parameter space for the Markov Chain)
#' @param burninCovariance The covariance matrix used to stochastically 
#'    constrain Markov Chain step sizes during the burnin phase
#' @param burninRealizations Number of realizations for the burnin phase
#' @param staticCovariance The convariance matrix used to stochastically
#'    constrain Markov Chain step sizes during the static Metropolis
#'    phase. This argument will default to be the same as burninCovariance.
#' @param staticRealizations Number of realizations for the static 
#'    Metropolis phase
#' @param adaptiveRealizations Number of realizations for the phase
#'    where the covariance used to constrain Markov Chain step size is
#'    adapted according to the covariance of previous accepted parameter
#'    sets in the ensemble.
#' @param criterion The critrerion object used for determining if a
#'    propsed parameter set is accepted or rejected. By default, a 
#'    criterion that assumes log likelihoods is used.
#' @param adaptiveCovarianceFactor A scalar factormultiplied by the raw 
#'    covariance matrix used for the Markov Chain proposal distribution.
#'    This attribute can be used to adjust for the acceptance rate during the
#'    adaptive phase. By default this is set to 1.
#' @param tinyIdentFactor A matrix added the covariance
#'    matrix to preven zero values in the diagonal.  
#'    By default the value of this argument is 1e-20.
#' @param writefiles A boolean switch used to determine if the output
#'    of the analysis is written to files as the algorithm progresses.
#' @param filesPath The path to which output files are written.
#'    By default, this path is "./output"
#' @param paramProposalsFile The file name for the proposed paramater
#'    output.  By default, "paramProposals.csv"
#' @param statsLogger The stats logger object to use for writing statistics
#'    to output. By default, an object of the \code{StatsLogger} class is
#'    created and used.  This logs the accepted probability, the proposed
#'    probability, and a boolean value indicating if the iteration was
#'    accepted or not.
#' @return The object of class \code{AdaptiveMCMCSampler} created
#'    by the constructor
AdaptiveMCMCSampler <- R6Class(
   classname = "AdaptiveMCMCSampler",
   public = list(
      objFunc = NULL,
      prevProb = NULL,
      maxProb = NULL,
      maxProbIndex = NULL,
      initialParams = NULL,
      numParams = NULL,
      burninRealizations = NULL,
      startCovarianceIndex = NULL,
      staticRealizations = NULL,
      totalStaticRealizations = NULL,
      adaptiveRealizations = NULL,
      totalRealizations = NULL,
      paramSamples = NULL,
      paramSamplesFile = NULL,
      paramProposals = NULL,
      paramProposalsFile = NULL,
      statsLogger = NULL,
      filesPath = NULL,
      writeFiles = NULL,
      burninProposalDist = NULL,
      staticProposalDist = NULL,
      criterion = NULL,
      initialize = function(
         objFunc, 
         initialParams, 
         burninProposalDist,
         burninRealizations,
         staticProposalDist = burninProposalDist,
         staticRealizations,
         adaptiveRealizations,
         criterion = CriterionMetropLogLikelihood$new(),
         writeFiles = TRUE,
         filesPath = "./output",
         paramProposalsFile = "paramProposals.csv",
         paramSamplesFile = "paramSamples.csv",
         statsLogger = StatsLogger$new()
      )
      {
         # Assign attributes according to arguments
         self$objFunc <- objFunc;   
         self$criterion <- criterion;
         self$initialParams <- initialParams;
         self$burninProposalDist <- burninProposalDist;
         self$burninRealizations <- burninRealizations;
         self$staticProposalDist <- staticProposalDist;
         self$staticRealizations <- staticRealizations;
         self$adaptiveRealizations <- adaptiveRealizations;
         self$filesPath <- filesPath;
         self$paramProposalsFile <- paste(
            filesPath,
            paramProposalsFile,
            sep = "/"
         );
         self$paramSamplesFile <- paste(
            filesPath,
            paramSamplesFile,
            sep = "/"
         );
         self$writeFiles <- writeFiles;
         
         # Derive attributes for indexing phases of AMMCMC algorithm
         self$numParams <- length(initialParams);
         self$startCovarianceIndex <- burninRealizations + 1;
         self$totalStaticRealizations <- 
            self$burninRealizations + self$staticRealizations;
         self$totalRealizations <-
            self$totalStaticRealizations + self$adaptiveRealizations;
         
         # Create the matrix for the parameter samples and
         # populate the first row
         self$paramSamples <- matrix(
            nrow = self$totalRealizations, 
            ncol = self$numParams,
            dimnames = list(NULL, names(initialParams))
         );
         self$paramSamples[1,] <- initialParams;
         
         # Create the matrix for the parameters proposed and
         # populate the first row
         self$paramProposals <- matrix(
            nrow = self$totalRealizations, 
            ncol = self$numParams,
            dimnames = list(NULL, names(initialParams))
         );
         self$paramProposals[1,] <- initialParams;
         
         # Configure the statistics logger object
         self$statsLogger <- statsLogger;
         self$statsLogger$buildLog(
            self$totalRealizations, 
            objFunc,
            self$filesPath
         );
         
         # Create the first row of samples and proposals from the
         # initial parameter set
         self$prevProb <- self$objFunc$propose(self$initialParams);
         self$maxProb <- self$prevProb;
         self$maxProbIndex <- 1;
         self$statsLogger$logProposed(1);
         self$statsLogger$logAccepted(1);
      },
      optimize = function()
      {
         # Set up the output files and write the first line
         if (self$writeFiles) {
            dir.create(
               path = self$filesPath, 
               showWarnings = FALSE,
               recursive = TRUE
            );
            write.table(
               x = data.frame(self$paramSamples[1:2,])[1,], 
               file = self$paramSamplesFile, 
               append = FALSE,
               sep = ",",
               col.names = TRUE,
               row.names = FALSE,
               quote = TRUE
            );
            write.table(
               x = data.frame(self$paramProposals[1:2,])[1,], 
               file = self$paramProposalsFile, 
               append = FALSE,
               sep = ",",
               col.names = TRUE,
               row.names = FALSE,
               quote = TRUE
            );
            self$statsLogger$writeFirstRow();
         }
         
         # Create a tiny identity matrix to avoid zeros in 
         # diagonal of covariance matrix
         tinyIdent = 
            
         
         # Start the static convariance burnin phase
         loop <- 2:self$burninRealizations;
         proposalDist <- self$burninProposalDist;
         for(realizationCount in loop) {
            # Take a Markov Chain step in parameter space based on a
            # static covariance and propose the new parameter set
            self$paramProposals[realizationCount,] <- 
               self$paramSamples[realizationCount - 1,] +
               proposalDist$randomSample();
            self$propose(realizationCount);
         }
         
         # Start the static covariance Metropolis phase
         loop <- (self$burninRealizations + 1):self$totalStaticRealizations;
         proposalDist <- self$staticProposalDist;
         for(realizationCount in loop) {
            # Take a Markov Chain step in parameter space based on a
            # static covariance and propose the new parameter set
            self$paramProposals[realizationCount,] <- 
               self$paramSamples[realizationCount - 1,] +
               proposalDist$randomSample();
            self$propose(realizationCount);
         }
         
         # Start the adaptive covariance Metropolis phase
         loop <- (self$totalStaticRealizations + 1):self$totalRealizations;
         for(realizationCount in loop) {
            prevRealization <- realizationCount - 1;
            # Adapt the covariance matrix based on the current parameter
            # ensemble
            covarianceIndices <- self$startCovarianceIndex:prevRealization;
            proposalDist$normalizedFit(self$paramSamples[covarianceIndices,]);

            # Take a Markov Chain step in parameter space based on an
            # adapted covariance and propose the new parameter set 
            self$paramProposals[realizationCount,] <- 
               self$paramSamples[prevRealization,] +
               proposalDist$randomSample();
            self$propose(realizationCount);
         }
      },
      propose = function(index, prevIndex = index - 1)
      {
         # Use the criterion object to determine if proposal
         # should be accepted
         accept <- self$criterion$isAccepted(
            self$objFunc$propose(self$paramProposals[index,]),
            self$prevProb
         );
         self$statsLogger$logProposed(index);
         
         # Record results of current realization depending on whether
         # proposed parameter set is accepted or rejected relative to
         # previous Markov Chain step
         if(accept) {
            # Accept the proposed parameters
            self$paramSamples[index,] <- self$paramProposals[index,];
            self$prevProb <- self$objFunc$value;
            if(self$prevProb > self$maxProb) {
               self$maxProb = self$prevProb;
               self$maxProbIndex = index;
            }
            self$statsLogger$logAccepted(index);
         } else {
            # Reject the proposed parameters
            self$paramSamples[index,] <- self$paramSamples[prevIndex,];
            self$statsLogger$logRejected(index);
         }
         
         # Write results to output files
         if(self$writeFiles) {
            write(
               self$paramSamples[index,], 
               file = self$paramSamplesFile, 
               ncolumns = self$numParams,
               sep = ",",
               append = TRUE
            );
            write(
               self$paramProposals[index,], 
               file = self$paramProposalsFile, 
               ncolumns = self$numParams,
               sep = ",",
               append = TRUE
            );
            self$statsLogger$writeRow(index);
         }
         
      },
      plotTraces = function(indices = NULL, ...)
      {
         if(is.null(indices)) {
            indices <- 1:self$totalRealizations;
         } else if (indices == "adaptive") {
            indices <- (self$totalStaticRealizations + 1):
               self$totalRealizations;
         } 
         par(mfrow = c(self$numParams, 1), mar = c(4, 5, 1, 1));
         for(paramIndex in 1:self$numParams) {
            plot(
               self$paramSamples[indices,paramIndex],
               ylab = colnames(self$paramSamples)[paramIndex],
               ...
            );
         }
      },
      plotPosteriorDensities = function(indices = "adaptive", ...)
      {
         if (indices == "adaptive") {
            indices <- (self$totalStaticRealizations + 1):
               self$totalRealizations;
         } else if(is.null(indices)) {
            indices <- 1:self$totalRealizations;
         }
         par(mfrow = c(self$numParams, 1), mar = c(4, 5, 1, 1));
         for(paramIndex in 1:self$numParams) {
            plot(
               density(self$paramSamples[indices,paramIndex]),
               main = "",
               xlab = colnames(self$paramSamples)[paramIndex],
               ...
            );   
         }
      },
      plotHighestPosterior = function(...)
      {
         self$objFunc$plotFit(
            params = self$paramSamples[self$maxProbIndex,],
            ...
         );
      },
      plotSummary = function(
         device = "pdf", 
         file = NULL,
         width = 8.5,
         height = 10
      ) 
      {
         if (device == "pdf") {
            pdf(file = file, width = width, height = height)
         }
         
         # Plot the full traces of the parameter samples
         if (device == "windows") {
            windows(width = width, height = height);
         } else if (device == "quartz") {
            quartz(width = width, height = height);
         }
         self$plotTraces();
         
         if (device == "windows") {
            windows(width = width, height = height);
         } else if (device == "quartz") {
            quartz(width = width, height = height);
         }
         self$plotTraces(indices = "adaptive");
         
         # Plot the posterior probability densities for parameter estimates
         if (device == "windows") {
            windows(width = width, height = height);
         } else if (device == "quartz") {
            quartz(width = width, height = height);
         }
         self$plotPosteriorDensities();
         
         # Plot the fit with the highest likelihood on the data
         if (device == "windows") {
            windows(width = width, height = height);
         } else if (device == "quartz") {
            quartz(width = width, height = height);
         }
         self$plotHighestPosterior();
         
         if (device == "pdf") {
            dev.off();
         }
      }
   )
);
