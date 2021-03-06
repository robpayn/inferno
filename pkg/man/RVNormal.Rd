% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RandomVariable.R
\name{RVNormal}
\alias{RVNormal}
\title{R6 class defining a normal random variable}
\description{
Provides tools for working with a random variable with
   a normal distribution defined by a mean and standard deviation
}
\section{Super class}{
\code{\link[inferno:RandomVariable]{inferno::RandomVariable}} -> \code{RVNormal}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{mean}}{The mean of the normal distribution}

\item{\code{sd}}{The standard deviation of the normal distribution}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{RVNormal$new()}}
\item \href{#method-density}{\code{RVNormal$density()}}
\item \href{#method-clone}{\code{RVNormal$clone()}}
}
}
\if{html}{
\out{<details open ><summary>Inherited methods</summary>}
\itemize{
\item \out{<span class="pkg-link" data-pkg="inferno" data-topic="RandomVariable" data-id="markovStep">}\href{../../inferno/html/RandomVariable.html#method-markovStep}{\code{inferno::RandomVariable$markovStep()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="inferno" data-topic="RandomVariable" data-id="normalizedFit">}\href{../../inferno/html/RandomVariable.html#method-normalizedFit}{\code{inferno::RandomVariable$normalizedFit()}}\out{</span>}
}
\out{</details>}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
Construct a new instance of the class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RVNormal$new(mean, sd)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{mean}}{Mean of the distribution}

\item{\code{sd}}{Standard deviation of the uniform distribution}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-density"></a>}}
\if{latex}{\out{\hypertarget{method-density}{}}}
\subsection{Method \code{density()}}{
Method to provide the probability density for a given value in the 
   normal distribution represented by the object
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RVNormal$density(val, log = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{val}}{The value for which the probability density is requested}

\item{\code{log}}{A boolean switch for requesting the log probability density.
Default value is FALSE.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The probability density of the value requested.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RVNormal$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
