% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sdhelper.r
\name{sample.fixed}
\alias{sample.fixed}
\title{Sample (fixed)}
\usage{
sample.fixed(x, size)
}
\arguments{
\item{x}{Vector to be sampled}

\item{size}{Number of samples to take.  Must be an integer less than x but
greater than zero.}
}
\value{
A vector of length size sampled from x.  If the length of x is 1,
this returns x.
}
\description{
Goal is to correct the default behavior of the "sample" function in R.
The default behavior changes if only one sample is needed, but some of my
code depends on consistent behavior.  Consequently, this function avoids
the change in behavior.
}

