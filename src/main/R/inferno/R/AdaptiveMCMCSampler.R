# Package dependencies ####

#' @importFrom R6 R6Class
NULL

# Class AdaptiveMCMCSampler (R6) ####

#' @export
#' 
#' @title
#'    R6 class defining an adaptive MCMC sampler
#' 
#' @description 
#'    Provides the tools for executing an optimization using a
#'    Markov Chain sampler with an adaptive covariance matrix to determine
#'    the step size. the algorithm for accepting or rejecting a sample is
#'    configurable, but by default is the Metropolis criterion for
#'    log likelihoods.
#' 
AdaptiveMCMCSampler <- R6Class(
   classname = "AdaptiveMCMCSampler",
   public = list(
      
      #' @field objFunc
      #'   The objective function used to calculate the likelihood of a 
      #'   given sample of parameter values.
      objFunc = NULL,
      
      #' @field prevProb
      #'   The probablity from the previous iteration of the Markov Chain
      prevProb = NULL,
      
      #' @field maxProb
      #'   The maximum probability in the Markov Chain
      maxProb = NULL,
      
      #' @field maxProbIndex
      #'   The index of the iteration with the maximum probability
      maxProbIndex = NULL,
      
      #' @field initialParams
      #'   The initial parameter values used to start the Markov Chain
      initialParams = NULL,
      
      #' @field numParams
      #'   The number of parameters to be estimated
      numParams = NULL,
      
      #' @field burninRealizations
      #'   The number of iterations in the burnin phase
      burninRealizations = NULL,
      
      #' @field startCovarianceIndex
      #'   The index of the first iteration where the covariance of the
      #'   Markov Chain is calculated (start of static phase)
      startCovarianceIndex = NULL,
      
      #' @field staticRealizations
      #'   The number of iterations in the static phase
      staticRealizations = NULL,
       
      #' @field totalStaticRealizations
      #'   Total number of iterations with a static proposal distribution
      #'   (burnin and static phases)
      totalStaticRealizations = NULL,
      
      #' @field adaptiveRealizations
      #'   The number of iterations in the adaptive phase
      adaptiveRealizations = NULL,
      
      #' @field totalRealizations
      #'   The total number of iterations (burnin, static, and adaptive phases)
      totalRealizations = NULL,
      
      #' @field paramSamples
      #'   Data frame tracking the sampled (accepted) parameter value sets
      paramSamples = NULL,
      
      #' @field paramSamplesFile
      #'   File for tracking the parameter samples
      paramSamplesFile = NULL,
      
      #' @field paramProposals
      #'   Data frame tracking the proposed parameter value sets
      paramProposals = NULL,
      
      #' @field paramProposalsFile
      #'   File for tracking the proposed parameter value sets
      paramProposalsFile = NULL,
      
      #' @field statsLoggers
      #'   The object used to log the statistics from each iteration
      statsLoggers = NULL,
      
      #' @field outputPath
      #'   The path to the directory where output is written
      outputPath = NULL,
      
      #' @field writeFiles
      #'   A boolean flag indicating if progress should be tracked
      #'   in files (value of TRUE will cause files to be written)
      writeFiles = NULL,
      
      #' @field burninProposalDist
      #'   The proposal distribution to use during the burnin phase
      burninProposalDist = NULL,
      
      #' @field staticProposalDist
      #'   The proposal distribution to use during the static phase
      staticProposalDist = NULL,
      
      #' @field criterion
      #'   The criterion object to use for the decision to accept
      #'   or reject a proposed parameter set
      criterion = NULL,

      # Method AdaptiveMCMCSampler$new ####
      #
      #' @description 
      #'   Construct a new instance of the class
      #'   
      #' @param objFunc 
      #'    The objective function used to calculate the likelihood of a 
      #'    given sample of parameter values.
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
      #' @param writeFiles 
      #'    A boolean switch used to determine if the output
      #'    of the analysis is written to files as the algorithm progresses.
      #'    Default value is TRUE, which will cause files to be written.
      #' @param outputPath 
      #'    The path to which output files are written.
      #'    By default, this path is "./output"
      #' @param paramProposalsFile 
      #'    Optional file name for the proposed paramater values
      #'    output.  
      #'    Defaults to "paramProposals.csv"
      #' @param paramSamplesFile
      #'    Optional file name for the sampled parameter values
      #'    Defaults to "paramSamples.csv".
      #' @param statsLoggers 
      #'    A list of stats logger objects to use for writing statistics
      #'    to output. 
      #'    By default, an object of the \code{\link{AdaptiveMCMCStatsLogger}} class is
      #'    created and used.  This logs the accepted probability, the proposed
      #'    probability, and a boolean value indicating if the iteration was
      #'    accepted or not.
      #'    
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
      },
      
      # Method AdaptiveMCMCSampler$optimize ####
      #
      #' @description
      #'   Sample parameter space using an adaptive Markov Chain Monte Carlo algorithm
      #' 
      #' @return 
      #'   No defined return value
      #'   
      optimize = function()
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
      },
      
      # Method AdaptiveMCMCSampler$propose ####
      #
      #' @description
      #'   Propose a parameter set in the Markov Chain algorithm
      #' 
      #' @param index
      #'   Index of the iteration for the current realization
      #' @param prevIndex
      #'   Index of the previous iteration.
      #'   Defaults to index - 1.
      #' 
      #' @return 
      #'   No defined return value
      #'   
      propose = function(index, prevIndex = index - 1)
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
      },
      
      # Method AdaptiveMCMCSampler$plotPosteriorDensities ####
      #
      #' @description
      #'   Plot the parameter distribution densities from a Markov Chain ensemble
      #' 
      #' @param indices
      #'   The string "adaptive" for the plotting the ensemble from the adaptive phase,
      #'   or a vector of the specific indices to plot.
      #'   Default value is "adaptive".
      #' @param ...
      #'   Arguments passed to the plot.default function
      #' 
      #' @return 
      #'   No defined return value
      #'   
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
      
      # Method AdaptiveMCMCSampler$plotTraces ####
      #
      #' @description 
      #'   Creates a series of trace plots, one panel per parameter
      #'   being estimated.
      #' 
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
      #'   No defined return value
      #'   
      plotTraces = function
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
      },
      
      # Method AdaptiveMCMCSampler$plotHighestPosterior ####
      #
      #' @description
      #'   Generate a plot comparing the best fit model to observations
      #' 
      #' @param ...
      #'   Arguments passed to the \code{\link{ObjectiveFunction}} plotFit method
      #' 
      #' @return 
      #'   No defined return value
      #' 
      plotHighestPosterior = function(...)
      {
         self$objFunc$plotFit(
            params = self$paramSamples[self$maxProbIndex,],
            ...
         );
      },
      
      # Method AdaptiveMCMCSampler$plotSummary ####
      #
      #' @description
      #'   Plot a summary of the analysis
      #' 
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
      #'   No defined return value
      #' 
      plotSummary = function
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
   
   )
);


# Class Criterion (R6) ####

#' @export
#' 
#' @title 
#'    R6 class defining a criterion for a sampler
#' 
#' @description 
#'    A selection criterion object can determine if a given proposal is
#'    accepted given the probability of the proposal and a reference probability
#' 
#'    This class is abstract and is not intended to be instantiated
#'    directly. Inheriting parameter processors are specific to a given 
#'    type of model input and the model that takes that input before execution.
#' 
Criterion <- R6Class(
   classname = "Criterion",
   public = list(
      
      # Abstract method Criterion$isAccepted ####
      #
      #' @description 
      #'   Determine if the provided probability is acceptable or not
      #' 
      #'   This method is abstract, indicating that implementations of 
      #'   this class need to override this method or an error will be
      #'   thrown when it is called.
      #' 
      #' @param prob
      #'   Probability being evaluated
      #' @param probRef
      #'   Reference probability for comparison
      #' @param ...
      #' 
      #' @return 
      #'   TRUE if accepted, FALSE otherwise
      #'   
      isAccepted = function(...)
      {
         stop("Method Criterion$isAccepted has not been implemented.");
      }
      
   )
);


# Class CriterionMetropLogLikelihood (R6) ####

#' @export
#' 
#' @title 
#'    R6 class defining a metropolis log likelihood criterion
#' 
#' @description 
#'    Provides the ability to evaluate the criterion for a Metropolis algorithm
#' 
CriterionMetropLogLikelihood <- R6Class(
   classname = "CriterionMetropLogLikelihood",
   inherit = Criterion,
   public = list(
      # Method CriterionMetropLogLikelihood$isAccepted ####
      #
      #' @description 
      #'   Determine if the provided probability is acceptable or not
      #'   using a Metropolis criterion
      #'   
      #'   Uses a Metropolis stochastic criterion to determine if the 
      #'   probability of a sample is accepted based on a comparison 
      #'   with a reference probability
      #'   
      #' @param prob
      #'   Probability to assess
      #' @param probRef
      #'   Reference probability for comparison
      #'   
      #' @return 
      #'   TRUE if accepted, FALSE otherwise
      #' 
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
)
