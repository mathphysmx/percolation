% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/indicator.R
\name{indicator}
\alias{indicator}
\title{indicator function of an interval in the real line}
\usage{
indicator(x, min = -Inf, max = Inf, liq = ">=", riq = "<=")
}
\arguments{
\item{x}{numeric vector of values to be tested.}

\item{min}{numeric, scalar lower bound. -\code{Inf} is a valid value.}

\item{max}{numeric, scalar upper bound. \code{Inf} is a valid value.}

\item{liq}{character. Left inequality. ">" or ">=".}

\item{riq}{characher. Right inequality. "<" or "<=".}
}
\value{
a logical vector with \link[base]{TRUE} when \code{x[i]} is in the interval given by the parameters of this function. Logical values were used for memory efficiency when large scale usage of this function.
}
\description{
indicator function of an interval in the real line
}
\examples{
xe <- c(-Inf, -3, 2, 6, Inf)
1 * indicator(x = xe, min = 2, max = 6, liq = ">=", riq = "<=")
    indicator(x = xe, min = 2, max = 6, liq = ">=", riq = "<=")
indicator(x = xe, min = 2, max = 6, liq = ">" , riq = "<=")
indicator(x = xe, min = 2, max = Inf, liq = ">=", riq = "<")
indicator(x = xe, min = 2, max = Inf, liq = ">=", riq = "<=")

# Also works in the case that x is unique and min and max are vectors
xl <- c(-1, 0.3, -4)
xu <- c( 1,   1, -2)
indicator(0, xl, xu)
}
\author{
Francisco Mendoza-Torres (\email{mentofran@gmail.com})
}
