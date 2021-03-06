% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sdhelper.r
\name{GetTimes}
\alias{GetTimes}
\title{Get model timing}
\usage{
GetTimes(start.time, run.times, run.lbl, timefile, write.timefile, Model.Name)
}
\arguments{
\item{start.time}{The starting time for the model run}

\item{run.times}{A list of intermediate times during the model run}

\item{run.lbl}{A list of labels for each of the intermediate times}

\item{timefile}{The file (including path) to be created}

\item{write.timefile}{An indicator for whether or not to write to file. If 0, no file will be output, if 1 only the total model time will be output, if 2 all timing will be output.}

\item{Model.Name}{The model being timed}
}
\value{
Nothing is returned, function is run to create a file with run times.
}
\description{
This function takes a start time, and different run times,
and outputs the timing to a file.  The purpose is to allow one to see 
how long different model processes take, and prioritize sections of code for
optimization or for implementation in C++.
}
\note{
run.times and run.lbl need to be the same length and in the same order,
or you will get corrupted/strange results!
}

