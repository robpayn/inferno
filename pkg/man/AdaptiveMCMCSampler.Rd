% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AdaptiveMCMCSampler.R
\name{AdaptiveMCMCSampler}
\alias{AdaptiveMCMCSampler}
\title{R6 class defining an adaptive MCMC sampler}
\description{
Provides the tools for executing an optimization using a
   Markov Chain sampler with an adaptive covariance matrix to determine
   the step size. the algorithm for accepting or rejecting a sample is
   configurable, but by default is the Metropolis criterion for
   log likelihoods.
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{objFunc}}{The objective function used to calculate the likelihood of a 
given sample of parameter values.}

\item{\code{prevProb}}{The probablity from the previous iteration of the Markov Chain}

\item{\code{maxProb}}{The maximum probability in the Markov Chain}

\item{\code{maxProbIndex}}{The index of the iteration with the maximum probability}

\item{\code{initialParams}}{The initial parameter values used to start the Markov Chain}

\item{\code{numParams}}{The number of parameters to be estimated}

\item{\code{burninRealizations}}{The number of iterations in the burnin phase}

\item{\code{startCovarianceIndex}}{The index of the first iteration where the covariance of the
Markov Chain is calculated (start of static phase)}

\item{\code{staticRealizations}}{The number of iterations in the static phase}

\item{\code{totalStaticRealizations}}{Total number of iterations with a static proposal distribution
(burnin and static phases)}

\item{\code{adaptiveRealizations}}{The number of iterations in the adaptive phase}

\item{\code{totalRealizations}}{The total number of iterations (burnin, static, and adaptive phases)}

\item{\code{paramSamples}}{Data frame tracking the sampled (accepted) parameter value sets}

\item{\code{paramSamplesFile}}{File for tracking the parameter samples}

\item{\code{paramProposals}}{Data frame tracking the proposed parameter value sets}

\item{\code{paramProposalsFile}}{File for tracking the proposed parameter value sets}

\item{\code{statsLoggers}}{The object used to log the statistics from each iteration}

\item{\code{outputPath}}{The path to the directory where output is written}

\item{\code{writeFiles}}{A boolean flag indicating if progress should be tracked
in files (value of TRUE will cause files to be written)}

\item{\code{burninProposalDist}}{The proposal distribution to use during the burnin phase}

\item{\code{staticProposalDist}}{The proposal distribution to use during the static phase}

\item{\code{criterion}}{The criterion object to use for the decision to accept
or reject a proposed parameter set}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{AdaptiveMCMCSampler$new()}}
\item \href{#method-optimize}{\code{AdaptiveMCMCSampler$optimize()}}
\item \href{#method-propose}{\code{AdaptiveMCMCSampler$propose()}}
\item \href{#method-plotPosteriorDensities}{\code{AdaptiveMCMCSampler$plotPosteriorDensities()}}
\item \href{#method-plotTraces}{\code{AdaptiveMCMCSampler$plotTraces()}}
\item \href{#method-plotHighestPosterior}{\code{AdaptiveMCMCSampler$plotHighestPosterior()}}
\item \href{#method-plotSummary}{\code{AdaptiveMCMCSampler$plotSummary()}}
\item \href{#method-clone}{\code{AdaptiveMCMCSampler$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
Construct a new instance of the class
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{AdaptiveMCMCSampler$new(
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
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{objFunc}}{The objective function used to calculate the likelihood of a 
given sample of parameter values.}

\item{\code{initialParams}}{A vector with initial values for the parameter 
being estimated
(initial location in parameter space for the Markov Chain)}

\item{\code{burninProposalDist}}{The random variable used to generate random
steps in the Markov Chain during the burnin phase. This should be 
an R6 class that extends the RandomVariable class, and implements 
the normalizedFit and markovStep methods.}

\item{\code{burninRealizations}}{Number of realizations for the burnin phase}

\item{\code{staticProposalDist}}{The random variable used to generate random
steps in the Markov Chain during the static phase. This should be 
an R6 class that extends the RandomVariable class, and implements 
the normalizedFit and markovStep methods.}

\item{\code{staticRealizations}}{Number of realizations for the static phase. The proposal distribution
will remain static over this period, but the results of the sampling
will begin to affect the proposal distribution that will be used during
the following adaptive phase.}

\item{\code{adaptiveRealizations}}{Number of realizations for the phase
where the covariance used to constrain Markov Chain step size is
adapted according to the covariance of previous accepted parameter
sets in the ensemble.}

\item{\code{criterion}}{The critrerion object used for determining if a
propsed parameter set is accepted or rejected. By default, a 
criterion that assumes log likelihoods is used.}

\item{\code{writeFiles}}{A boolean switch used to determine if the output
of the analysis is written to files as the algorithm progresses.
Default value is TRUE, which will cause files to be written.}

\item{\code{outputPath}}{The path to which output files are written.
By default, this path is "./output"}

\item{\code{paramProposalsFile}}{Optional file name for the proposed paramater values
output.  
Defaults to "paramProposals.csv"}

\item{\code{paramSamplesFile}}{Optional file name for the sampled parameter values
Defaults to "paramSamples.csv".}

\item{\code{statsLoggers}}{A list of stats logger objects to use for writing statistics
to output. 
By default, an object of the \code{\link{AdaptiveMCMCStatsLogger}} class is
created and used.  This logs the accepted probability, the proposed
probability, and a boolean value indicating if the iteration was
accepted or not.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-optimize"></a>}}
\if{latex}{\out{\hypertarget{method-optimize}{}}}
\subsection{Method \code{optimize()}}{
Sample parameter space using an adaptive Markov Chain Monte Carlo algorithm
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{AdaptiveMCMCSampler$optimize()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
No defined return value
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-propose"></a>}}
\if{latex}{\out{\hypertarget{method-propose}{}}}
\subsection{Method \code{propose()}}{
Propose a parameter set in the Markov Chain algorithm
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{AdaptiveMCMCSampler$propose(index, prevIndex = index - 1)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{index}}{Index of the iteration for the current realization}

\item{\code{prevIndex}}{Index of the previous iteration.
Defaults to index - 1.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
No defined return value
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-plotPosteriorDensities"></a>}}
\if{latex}{\out{\hypertarget{method-plotPosteriorDensities}{}}}
\subsection{Method \code{plotPosteriorDensities()}}{
Plot the parameter distribution densities from a Markov Chain ensemble
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{AdaptiveMCMCSampler$plotPosteriorDensities(indices = "adaptive", ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{indices}}{The string "adaptive" for the plotting the ensemble from the adaptive phase,
or a vector of the specific indices to plot.
Default value is "adaptive".}

\item{\code{...}}{Arguments passed to the plot.default function}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
No defined return value
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-plotTraces"></a>}}
\if{latex}{\out{\hypertarget{method-plotTraces}{}}}
\subsection{Method \code{plotTraces()}}{
Creates a series of trace plots, one panel per parameter
  being estimated.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{AdaptiveMCMCSampler$plotTraces(
  indices = NULL,
  mfrow = c(self$numParams, 1),
  mar = c(4, 5, 1, 1),
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{indices}}{The vector of indices to include in the plot}

\item{\code{mfrow}}{Two element vector containing the number of rows
and columns in which to arrange the plots.
Defaults to a single column with as many rows as
parameters being estimated.}

\item{\code{mar}}{The size of margins (in lines) for the plots within each panel.
Defaults to (bottom, left, top, right) = (4, 5, 1, 1).}

\item{\code{...}}{Arguments passed on to the plot function}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
No defined return value
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-plotHighestPosterior"></a>}}
\if{latex}{\out{\hypertarget{method-plotHighestPosterior}{}}}
\subsection{Method \code{plotHighestPosterior()}}{
Generate a plot comparing the best fit model to observations
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{AdaptiveMCMCSampler$plotHighestPosterior(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{Arguments passed to the \code{\link{ObjectiveFunction}} plotFit method}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
No defined return value
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-plotSummary"></a>}}
\if{latex}{\out{\hypertarget{method-plotSummary}{}}}
\subsection{Method \code{plotSummary()}}{
Plot a summary of the analysis
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{AdaptiveMCMCSampler$plotSummary(
  device = "pdf",
  file = NULL,
  width = 8.5,
  height = 10
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{device}}{Graphics device ("pdf", "windows", or "quartz")}

\item{\code{file}}{Path to the file to write (for "pdf" device only)}

\item{\code{width}}{Width of the device
Defaults to 8.5.}

\item{\code{height}}{Height of the device
Defaults to 10.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
No defined return value
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{AdaptiveMCMCSampler$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
