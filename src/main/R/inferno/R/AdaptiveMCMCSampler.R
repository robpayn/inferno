# Package dependencies ####

#' @importFrom R6 R6Class
NULL

# Class AdaptiveMCMCSampler (R6) ####

#' @export
#' 
#' @title
#'    An Adaptive Markov Chain Monte Carlo Sampler
#' 
#' @description 
#'    Provides the tools for executing an optimization using a
#'    Markov Chain sampler with an adaptive covariance matrix to determine
#'    the step size. the algorithm for accepting or rejecting a sample is
#'    configurable, but by default is the Metropolis criterion for
#'    log likelihoods.
#' 
#' @usage 
#'    AdaptiveMCMCSampler$new(<arguments>)
#' @param objFunc 
#'    The objective function used to calculate the base
#'    likelihood.  The sum of the log prior likelihoods are added to this
#'    to generate the overall value of the objective function
#' @param initialParams 
#'    A vector with initial values for the parameter 
#'    being estimated
#'    (initial location in parameter space for the Markov Chain)
#' @param burninProposalDist 
#'    The random variable used to generate random
#'    steps in the Markov Chain during the burnin phase. This should be 
#'    an R6 class that extends the RandomVariable class, and implements 
#'    the normalizedFit and markovStep methods.
#' @param burninRealizations 
#'    Number of realizations for the burnin phase
#' @param staticProposalDist 
#'    The random variable used to generate random
#'    steps in the Markov Chain during the static phase. This should be 
#'    an R6 class that extends the RandomVariable class, and implements 
#'    the normalizedFit and markovStep methods.
#' @param staticRealizations 
#'    Number of realizations for the static phase. The proposal distribution
#'    will remain static over this period, but the results of the sampling
#'    will begin to affect the proposal distribution that will be used during
#'    the following adaptive phase.
#' @param adaptiveRealizations 
#'    Number of realizations for the phase
#'    where the covariance used to constrain Markov Chain step size is
#'    adapted according to the covariance of previous accepted parameter
#'    sets in the ensemble.
#' @param criterion 
#'    The critrerion object used for determining if a
#'    propsed parameter set is accepted or rejected. By default, a 
#'    criterion that assumes log likelihoods is used.
#' @param writefiles 
#'    A boolean switch used to determine if the output
#'    of the analysis is written to files as the algorithm progresses.
#'    Default value is TRUE, which will cause files to be written.
#' @param outputPath 
#'    The path to which output files are written.
#'    By default, this path is "./output"
#' @param paramProposalsFile 
#'    The file name for the proposed paramater
#'    output.  By default, "paramProposals.csv"
#' @param statsLoggers 
#'    A list of stats logger objects to use for writing statistics
#'    to output. 
#'    By default, an object of the \code{\link{AdaptiveMCMCStatsLogger}} class is
#'    created and used.  This logs the accepted probability, the proposed
#'    probability, and a boolean value indicating if the iteration was
#'    accepted or not.
#' @return 
#'    The object of class \code{AdaptiveMCMCSampler} created
#'    by the constructor
#'    
#' @section Methods:
#'   \code{$new}\cr
#'   \code{$optimize} - 
#'     See \code{\link{AdaptiveMCMCSampler_optimize}}\cr
#'   \code{$optimize} - 
#'     See \code{\link{AdaptiveMCMCSampler_propose}}\cr
#'   \code{$plotPosteriorDensities} - 
#'     See \code{\link{AdaptiveMCMCSampler_plotPosteriorDensities}}\cr
#'   \code{$plotTraces} - 
#'     See \code{\link{AdaptiveMCMCSampler_plotTraces}}\cr
#'   \code{$plotHighestPosterior} - 
#'     See \code{\link{AdaptiveMCMCSampler_plotHighestPosterior}}\cr
#'   \code{$plotSummary} - 
#'     See \code{\link{AdaptiveMCMCSampler_plotSummary}}\cr
#'     
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
      statsLoggers = NULL,
      outputPath = NULL,
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
         outputPath = "./output",
         paramProposalsFile = "paramProposals.csv",
         paramSamplesFile = "paramSamples.csv",
         statsLoggers = list(basic = AdaptiveMCMCStatsLogger$new())
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
         self$outputPath <- outputPath;
         self$paramProposalsFile <- paste(
            outputPath,
            paramProposalsFile,
            sep = "/"
         );
         self$paramSamplesFile <- paste(
            outputPath,
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
         self$statsLoggers <- statsLoggers;
         lapply(
            X = self$statsLoggers,
            FUN = function(statsLogger, ...) 
               {
                  statsLogger$buildLog(...);
               },
            numRows = self$totalRealizations,
            objFunc = self$objFunc,
            filePath = self$outputPath
         );
         
         # Create the first row of samples and proposals from the
         # initial parameter set
         self$prevProb <- self$objFunc$propose(self$initialParams);
         self$maxProb <- self$prevProb;
         self$maxProbIndex <- 1;
         lapply(
            X = self$statsLoggers,
            FUN = function(statsLogger) 
            {
               statsLogger$logAccepted(1);
            }
         );
      }
   )
);

# Method AdaptiveMCMCSampler$optimize ####

#' @name AdaptiveMCMCSampler_optimize
#' 
#' @title
#'   Sample parameter space using an adaptive Markov Chain Monte Carlo algorithm
#' 
#' @usage 
#'   [Object]$optimize()
#' 
#' @return 
#'   No specific return value
#' 
#' @section Method of class:
#'   \code{\link{AdaptiveMCMCSampler}}
#'   
AdaptiveMCMCSampler$set(
   which = "public",
   name = "optimize",
   value = function()
      {
         # Set up the output files and write the first line
         if (self$writeFiles) {
            dir.create(
               path = self$outputPath, 
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
            lapply(
               X = self$statsLoggers,
               FUN = function(statsLogger) 
               {
                  statsLogger$writeFirstRow();
               }
            );
         }
         
         # Start the static convariance burnin phase
         loop <- 2:self$burninRealizations;
         proposalDist <- self$burninProposalDist;
         for(realizationCount in loop) {
            # Take a Markov Chain step in parameter space based on a
            # static covariance and propose the new parameter set
            self$paramProposals[realizationCount,] <- 
               proposalDist$markovStep(self$paramSamples[realizationCount - 1,]);
            self$propose(realizationCount);
         }
         
         # Start the static covariance Metropolis phase
         loop <- (self$burninRealizations + 1):self$totalStaticRealizations;
         proposalDist <- self$staticProposalDist;
         for(realizationCount in loop) {
            # Take a Markov Chain step in parameter space based on a
            # static covariance and propose the new parameter set
            self$paramProposals[realizationCount,] <- 
               proposalDist$markovStep(self$paramSamples[realizationCount - 1,]);
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
               proposalDist$markovStep(self$paramSamples[realizationCount - 1,]);
            self$propose(realizationCount);
         }
      }
);

# Method AdaptiveMCMCSampler$propose ####

#' @name AdaptiveMCMCSampler_propose
#' 
#' @title
#'   Propose a parameter set in the Markov Chain algorithm
#' 
#' @usage 
#'   [Object]$propose(<arguments>)
#' @param index
#'   Index of the iteration for the current realization
#' @param prevIndex
#'   Index of the previous iteration.
#'   Defaults to index - 1.
#' 
#' @return 
#'   No specific return value
#' 
#' @section Method of class:
#'   \code{\link{AdaptiveMCMCSampler}}
#'   
AdaptiveMCMCSampler$set(
   which = "public",
   name = "propose",
   value = function(index, prevIndex = index - 1)
      {
         # Use the criterion object to determine if proposal
         # should be accepted
         accept <- self$criterion$isAccepted(
            self$objFunc$propose(self$paramProposals[index,]),
            self$prevProb
         );

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
            lapply(
               X = self$statsLoggers,
               FUN = function(statsLogger) 
                  {
                     statsLogger$logAccepted(index);
                  }
            );
         } else {
            # Reject the proposed parameters
            self$paramSamples[index,] <- self$paramSamples[prevIndex,];
            lapply(
               X = self$statsLoggers,
               FUN = function(statsLogger) 
                  {
                     statsLogger$logRejected(index);
                  }
            );
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
            lapply(
               X = self$statsLoggers,
               FUN = function(statsLogger) 
                  {
                     statsLogger$writeRow(index);
                  }
            );
         }
         
      }
);

# Method AdaptiveMCMCSampler$plotPosteriorDensities ####

#' @name AdaptiveMCMCSampler_plotPosteriorDensities
#' 
#' @title
#'   Plot the parameter distribution densities from a Markov Chain ensemble
#' 
#' @usage 
#'   [Object]$plotPosteriorDensities(<arguments>)
#' @param indices
#'   The string "adaptive" for the plotting the ensemble from the adaptive phase,
#'   or a vector of the specific indices to plot.
#'   Default value is "adaptive".
#' @param ...
#'   Arguments passed to the plot function
#' 
#' @return 
#'   No specific return value
#' 
#' @section Method of class:
#'   \code{\link{AdaptiveMCMCSampler}}
#'   

AdaptiveMCMCSampler$set(
   which = "public",
   name = "plotPosteriorDensities",
   value = function(indices = "adaptive", ...)
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
      }
);

# Method AdaptiveMCMCSampler$plotTraces ####

#' @name AdaptiveMCMCSampler_plotTraces
#' 
#' @title
#'   Plot the traces from a Markov Chain analysis
#'   
#' @description 
#'   Creates a series of trace plots, one panel per parameter
#'   being estimated.
#' 
#' @usage 
#'   [Object]$plotTraces(<arguments>)
#' @param indices
#'   The vector of indices to include in the plot
#' @param mfrow
#'   Two element vector containing the number of rows
#'   and columns in which to arrange the plots.
#'   Defaults to a single column with as many rows as
#'   parameters being estimated.
#' @param mar
#'   The size of margins (in lines) for the plots within each panel.
#'   Defaults to (bottom, left, top, right) = (4, 5, 1, 1).
#' @param ...
#'   Arguments passed on to the plot function
#' 
#' @return 
#'   No specific return value
#' 
#' @section Method of class:
#'   \code{\link{AdaptiveMCMCSampler}}
#'   
AdaptiveMCMCSampler$set(
   which = "public",
   name = "plotTraces",
   value = function
      (
         indices = NULL, 
         mfrow = c(self$numParams, 1),
         mar = c(4, 5, 1, 1),
         ...
      )
      {
         if(is.null(indices)) {
            indices <- 1:self$totalRealizations;
         } else if (indices == "adaptive") {
            indices <- (self$totalStaticRealizations + 1):
               self$totalRealizations;
         } 
         par(mfrow = mfrow, mar = mar);
         for(paramIndex in 1:self$numParams) {
            plot(
               self$paramSamples[indices,paramIndex],
               ylab = colnames(self$paramSamples)[paramIndex],
               ...
            );
         }
      }
);

# Method AdaptiveMCMCSampler$plotHighestPosterior ####

#' @name AdaptiveMCMCSampler_plotHighestPosterior
#' 
#' @title
#'   Generate a plot comparing the best fit model to observations
#' 
#' @usage 
#'   [Object]$plotHighestPosterior(<arguments>)
#' @param ...
#'   Arguments passed to the \code{\link{ObjectiveFunction_plotFit}} method
#' 
#' @return 
#'   No specific return value
#' 
#' @section Method of class:
#'   \code{\link{AdaptiveMCMCSampler}}
#'   
AdaptiveMCMCSampler$set(
   which = "public",
   name = "plotHighestPosterior",
   value = function(...)
      {
         self$objFunc$plotFit(
            params = self$paramSamples[self$maxProbIndex,],
            ...
         );
      }
);

# Method AdaptiveMCMCSampler$plotSummary ####

#' @name AdaptiveMCMCSampler_plotSummary
#' 
#' @title
#'   Plot a summary of the analysis
#' 
#' @usage 
#'   [Object]$plotSummary(<arguments>)
#' @param device
#'   Graphics device ("pdf", "windows", or "quartz")
#' @param file
#'   Path to the file to write (for "pdf" device only)
#' @param width
#'   Width of the device
#'   Defaults to 8.5.
#' @param height
#'   Height of the device
#'   Defaults to 10.
#' 
#' @return 
#'   No specific return value
#' 
#' @section Method of class:
#'   \code{\link{AdaptiveMCMCSampler}}
#'   
AdaptiveMCMCSampler$set(
   which = "public",
   name = "plotSummary",
   value = function
      (
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
);


# Class Criterion (R6) ####

#' @export
#' 
#' @title 
#'    Abstract selection criterion class
#' 
#' @description 
#'    A selection criterion object can determine if a given proposal is
#'    accepted given the probability of the proposal and a reference probability
#' 
#'    This class is abstract and is not intended to be instantiated
#'    directly. Inheriting parameter processors are specific to a given 
#'    type of model input and the model that takes that input before execution.
#' 
#' @usage Abstract
#' 
#' @section Methods:
#'   \code{$new}\cr
#'   \code{$isAccepted} - 
#'     See \code{\link{Criterion_isAccepted}}\cr
Criterion <- R6Class(classname = "Criterion");

# Abstract method Criterion$isAccepted ####

#' @name Criterion_isAccepted
#' 
#' @title 
#'   Determines if a probability justifies accepting a sample
#'   
#' @description 
#'   This method is abstract, indicating that implementations of 
#'   this class need to override this method or an error will be
#'   thrown when it is called.
#' 
#' @section Abstract method of class:
#'   \code{\link{Criterion}}
#'   
Criterion$set(
   which = "public",
   name = "isAccepted",
   value = function(prob, probRef)
      {
         stop("Method 'isAccepted' has not been implemented");
      }
);


# Class CriterionMetropLogLikelihood (R6) ####

#' @export
#' 
#' @title 
#'    Metropolis election criterion class
#' 
#' @description 
#'    Controls the criterion for a Metropolis algorithm
#' 
#' @usage 
#'    CriterionMetropLogLikelihood$new(<arguments>)
#'    
#' @section Methods:
#'   \code{$new}\cr
#'   \code{$isAccepted} - 
#'     See \code{\link{CriterionMetropLogLikelihood_isAccepted}}\cr
#'     
CriterionMetropLogLikelihood <- R6Class(
   classname = "CriterionMetropLogLikelihood",
   inherit = Criterion
);


# Method CriterionMetropLogLikelihood$isAccepted ####

#' @name CriterionMetropLogLikelihood_isAccepted
#' 
#' @title 
#'   Determines if a probability justifies accepting a sample
#'   
#' @description 
#'   Uses a Metropolis stochastic criterion to determine if the 
#'   probability of a sample is accepted based on a comparison 
#'   with a reference probability
#'   
#' @usage
#'   [Object]$isAccepted(<arguments>)
#' @param prob
#'   Probability to assess
#' @param probRef
#'   Reference probability for comparison
#'   
#' @return 
#'   TRUE if accepted, FALSE otherwise
#' 
#' @section Abstract method of class:
#'   \code{\link{CriterionMetropLogLikelihood}}
#'   
CriterionMetropLogLikelihood$set(
   which = "public",
   name = "isAccepted",
   value = function(prob, probRef)
      {
         if(is.na(prob)) {
            delta <- 0;
         } else {
            delta <- exp(prob - probRef);
         }
         return(runif(n = 1) < delta);
      }
);
