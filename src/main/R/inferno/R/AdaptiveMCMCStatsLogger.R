# Package dependencies ####

#' @importFrom R6 R6Class
NULL

# Class AdaptiveMCMCStatsLogger (R6) ####

#' @export
#' 
#' @title
#'    Markov Chain stats logging tool
#' 
#' @description
#'    Logs the stats from a Markov Chain accept/reject algorithm. Stats
#'    are logged both to a matrix in memory and written to files, if configured
#'    to do so.
#' 
#' @usage 
#'    StatsLogger$new(<arguments>)
#' @param filePath
#'    Optional argument to set the path to which output files are written
#' @param statsFile
#'    Name of the file with logged stats data
#'    
#' @return The object of class \code{StatsLogger} created
#'    by the constructor
#'    
#' @section Methods:
#'   \code{$new}\cr
#'   \code{$buildLog} - 
#'     See \code{\link{AdaptiveMCMCStatsLogger_buildLog}}\cr
#'   \code{$logAccepted} - 
#'     See \code{\link{AdaptiveMCMCStatsLogger_logAccepted}}\cr
#'   \code{$logRejected} - 
#'     See \code{\link{AdaptiveMCMCStatsLogger_logRejected}}\cr
#'   \code{$logFailedPredictions} - 
#'     See \code{\link{AdaptiveMCMCStatsLogger_logFailedPredictions}}\cr
#'   \code{$writeFirstRow} - 
#'     See \code{\link{AdaptiveMCMCStatsLogger_writeFirstRow}}\cr
#'   \code{$writeRow} - 
#'     See \code{\link{AdaptiveMCMCStatsLogger_writeRow}}\cr
#'     
AdaptiveMCMCStatsLogger <- R6Class(
   classname = "AdaptiveMCMCStatsLogger",
   public = list(
      numRows = NULL,
      objFunc = NULL,
      stats = NULL,
      statsFile = NULL,
      filePath = NULL,
      isLoggingFailedPredictions = NULL,
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
         }
   )
);

# Method AdaptiveMCMCStatsLogger$buildLog ####

#' @name AdaptiveMCMCStatsLogger_buildLog
#' 
#' @title 
#'    Build the logger data structures and files
#' 
#' @description 
#'    Creates the data structures and files necessary to track
#'    the statistics through a data analysis
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
#'   \code{\link{AdaptiveMCMCStatsLogger}}
#'   
AdaptiveMCMCStatsLogger$set(
   which = "public",
   name = "buildLog",
   value = function(numRows, objFunc, filePath = "./output")
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
      }
);

# Method AdaptiveMCMCStatsLogger$logAccepted ####

#' @name AdaptiveMCMCStatsLogger_logAccepted
#' 
#' @title 
#'   Create accepted log entries
#' 
#' @section Method of class:
#'   \code{\link{AdaptiveMCMCStatsLogger}}
#'   
AdaptiveMCMCStatsLogger$set(
   which = "public",
   name = "logAccepted",
   value = function(index)
      {
         self$stats[index, 1] <- self$objFunc$value;
         self$stats[index, 2] <- self$objFunc$value;
         self$stats[index, 3] <- TRUE;
         if (self$isLoggingFailedPredictions) {
            self$logFailedPredictions(index);
         }
      }
);

# Method AdaptiveMCMCStatsLogger$logRejected ####

#' @name AdaptiveMCMCStatsLogger_logRejected
#' 
#' @title 
#'   Create rejected log entries
#' 
#' @section Method of class:
#'   \code{\link{AdaptiveMCMCStatsLogger}}
#'   
AdaptiveMCMCStatsLogger$set(
   which = "public",
   name = "logRejected",
   value = function(index)
      {
         self$stats[index, 1] <- self$stats[index - 1, 1];
         self$stats[index, 2] <- self$objFunc$value;
         self$stats[index, 3] <- FALSE;
         if (self$isLoggingFailedPredictions) {
            self$logFailedPredictions(index);
         }
      }
);

# Method AdaptiveMCMCStatsLogger$logFailedPredictions ####

#' @name AdaptiveMCMCStatsLogger_logFailedPredictions
#' 
#' @title 
#'   Create failed prediction log entries
#' 
#' @section Method of class:
#'   \code{\link{AdaptiveMCMCStatsLogger}}
#'   
AdaptiveMCMCStatsLogger$set(
   which = "public",
   name = "logFailedPredictions",
   value = function(index)
      {
         if (is.null(self$objFunc$prediction)) {
            self$stats[index, 4] <- TRUE;   
         } else {
            self$stats[index, 4] <- FALSE;
         }
      }
);

# Method AdaptiveMCMCStatsLogger$writeFirstRow ####

#' @name AdaptiveMCMCStatsLogger_writeFirstRow
#' 
#' @title 
#'   Write the first row of output files
#' 
#' @section Method of class:
#'   \code{\link{AdaptiveMCMCStatsLogger}}
#'   
AdaptiveMCMCStatsLogger$set(
   which = "public",
   name = "writeFirstRow",
   value = function()
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
      }
);

# Method AdaptiveMCMCStatsLogger$writeRow ####

#' @name AdaptiveMCMCStatsLogger_writeRow
#' 
#' @title 
#'   Write a row to output files
#' 
#' @section Method of class:
#'   \code{\link{AdaptiveMCMCStatsLogger}}
#'   
AdaptiveMCMCStatsLogger$set(
   which = "public",
   name = "writeRow",
   value = function(index)
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
);
