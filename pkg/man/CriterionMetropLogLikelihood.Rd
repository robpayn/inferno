% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AdaptiveMCMCSampler.R
\name{CriterionMetropLogLikelihood}
\alias{CriterionMetropLogLikelihood}
\title{R6 class defining a metropolis log likelihood criterion}
\description{
Provides the ability to evaluate the criterion for a Metropolis algorithm
}
\section{Super class}{
\code{\link[inferno:Criterion]{inferno::Criterion}} -> \code{CriterionMetropLogLikelihood}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-isAccepted}{\code{CriterionMetropLogLikelihood$isAccepted()}}
\item \href{#method-clone}{\code{CriterionMetropLogLikelihood$clone()}}
}
}
\if{html}{
\out{<details open ><summary>Inherited methods</summary>}
\itemize{
}
\out{</details>}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-isAccepted"></a>}}
\if{latex}{\out{\hypertarget{method-isAccepted}{}}}
\subsection{Method \code{isAccepted()}}{
Determine if the provided probability is acceptable or not
  using a Metropolis criterion
  
  Uses a Metropolis stochastic criterion to determine if the 
  probability of a sample is accepted based on a comparison 
  with a reference probability
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CriterionMetropLogLikelihood$isAccepted(prob, probRef)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{prob}}{Probability to assess}

\item{\code{probRef}}{Reference probability for comparison}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
TRUE if accepted, FALSE otherwise
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CriterionMetropLogLikelihood$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
