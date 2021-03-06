% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sdhelper.r
\name{check.outputs}
\alias{check.outputs}
\title{Check outputs}
\usage{
check.outputs(run.name, RunLog)
}
\arguments{
\item{run.name}{The name of the run}

\item{RunLog}{A log for the run}
}
\description{
Check that the outputs generated by the test run match their expected values
}
\details{
This will ensure that changes to the code, or to the user's operating system
do not change the overall model outcomes in unexpected ways.  The goal is to
test the main model options, and ensure that they are still producing the 
outputs one would expect from the definition of the code implementation.
}

