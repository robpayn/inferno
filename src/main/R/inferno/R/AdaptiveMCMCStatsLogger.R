# Package dependencies ####

#' @importFrom R6 R6Class
NULL

# Class AdaptiveMCMCStatsLogger (R6) ####

#' @export
#' 
#' @title
#'    R6 class defining a logger for an adaptive MCMC algorithm
#' 
#' @description
#'    Logs the stats from a Markov Chain accept/reject algorithm. Stats
#'    are logged both to a matrix in memory and written to files, if configured
#'    to do so.
#' 
AdaptiveMCMCStatsLogger <- R6Class(
   classname = "AdaptiveMCMCStatsLogger",
   public = list(
      
      #' @field numRows
      #'   Number of rows of data
      numRows = NULL,
      
      #' @field objFunc
      #'   Objective function used to calculate statistics
      objFunc = NULL,
      
      #' @field stats
      #'   The data frame containing the statistics
      stats = NULL,
      
      #' @field statsFile
      #'   The file used to track the statistics
      statsFile = NULL,
      
      #' @field filePath
      #'   Path to the directory where the stats file should be written
      filePath = NULL,
      
      #' @field isLoggingFailedPredictions
      #'   Controls whether failed predictions are logged or not
      isLoggingFailedPredictions = NULL,
      
      # Method AdaptiveMCMCStatsLogger$new ####
      #
      #' @description 
      #'   Construct a new instance of the class
      #'   
      #' @param filePath
      #'   Optional argument to set the path to which output files are written
      #' @param statsFile
      #'   Name of the file with logged stats data
      #' @param isLoggingFailedPredictions
      #'   Controls whether failed predictions are logged or not
      #'    
      initialize = function
      (
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
      
      # Method AdaptiveMCMCStatsLogger$buildLog ####
      #
      #' @description 
      #'    Creates the data structures and files necessary to track
      #'    the statistics through a data analysis
      #' 
      #' @param numRows
      #'    Number of rows of data expected in the log
      #' @param objFunc
      #'    The objective function with the statistics to be logged
      #' @param filePath
      #'    The file path where log files should be created
      #'    
      #' @return 
      #'    No defined return value
      #'    
      buildLog = function
      (
         numRows, 
         objFunc, 
         filePath = "./output"
      )
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
      
      # Method AdaptiveMCMCStatsLogger$logAccepted ####
      #
      #' @description 
      #'   Create accepted log entries
      #' 
      #' @param index
      #'   Index of row for results
      #' 
      #' @return 
      #'   No defined return value.
      #'   
      logAccepted = function(index)
      {
         self$stats[index, 1] <- self$objFunc$value;
         self$stats[index, 2] <- self$objFunc$value;
         self$stats[index, 3] <- TRUE;
         if (self$isLoggingFailedPredictions) {
            self$logFailedPredictions(index);
         }
      },

      # Method AdaptiveMCMCStatsLogger$logRejected ####
      #
      #' @description 
      #'   Create rejected log entries
      #'   
      #' @param index
      #'   Index of row for results
      #' 
      #' @return 
      #'   No defined return value.
      #'   
      logRejected = function(index)
      {
         self$stats[index, 1] <- self$stats[index - 1, 1];
         self$stats[index, 2] <- self$objFunc$value;
         self$stats[index, 3] <- FALSE;
         if (self$isLoggingFailedPredictions) {
            self$logFailedPredictions(index);
         }
      },
      
      # Method AdaptiveMCMCStatsLogger$logFailedPredictions ####
      #
      #' @description 
      #'   Create failed prediction log entries
      #' 
      #' @param index
      #'   Index of row for results
      #' 
      #' @return 
      #'   No defined return value.
      #'   
      logFailedPredictions = function(index)
      {
         if (is.null(self$objFunc$prediction)) {
            self$stats[index, 4] <- TRUE;   
         } else {
            self$stats[index, 4] <- FALSE;
         }
      },
      
      # Method AdaptiveMCMCStatsLogger$writeFirstRow ####
      #
      #' @description 
      #'   Write the first row of output files
      #' 
      #' @return 
      #'   No defined return value
      #'   
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

      # Method AdaptiveMCMCStatsLogger$writeRow ####
      #
      #' @description 
      #'   Write a row to output files
      #' 
      #' @param index
      #'   Index of row for results
      #' 
      #' @return 
      #'   No defined return value.
      #'   
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
)
