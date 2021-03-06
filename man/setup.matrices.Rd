% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sdhelper.r
\name{setup.matrices}
\alias{setup.matrices}
\title{Set up model matrices}
\usage{
setup.matrices(K_g, spe, SpTraits, p, S, landscape, landscape.identifiers,
  distances, B1.template, B2.template, P, M_sub, DispPath, vdb.data,
  change.count, outpath.base, num.sim, run.times = NA, run.lbl = NA,
  multi.species.K = 0, edge.type = "TORUS")
}
\arguments{
\item{K_g}{Carrying capacity in grams}

\item{spe}{Total number of species in the species pool}

\item{SpTraits}{A dataframe containing species traits}

\item{p}{The number of cells in a square landscape}

\item{S}{The number of stages in the matrix model}

\item{landscape}{a list of environmental layers, each with a value for each cell in the landscape}

\item{landscape.identifiers}{A list of single-letter identifiers for each environmental layer}

\item{distances}{Distances between each cell in the landscape}

\item{B1.template}{A template for the B1 demography matrix}

\item{B2.template}{A template for the B2 demography matrix}

\item{P}{The vec-permutation matrix}

\item{M_sub}{The M matrix template}

\item{DispPath}{The path to the dispersal tables}

\item{vdb.data}{An indicator for whether or not to create visual debugger data}

\item{change.count}{The change step}

\item{outpath.base}{The base output path.}

\item{num.sim}{The number of simulations to use in creating dispersal probabilities}

\item{run.times}{(optional, defaults to NA) For timing the function}

\item{run.lbl}{= (optional, defaults to NA) For labeling the timing of the function}

\item{multi.species.K}{(default is 0) An indicator for whether a multispecies carrying capacity is desired}

\item{edge.type}{(default is "TORUS") An indicator for what type of landscape edge is desired.}
}
\description{
This function sets up the core matrices for the matrix diagnostics and the
simulation model.
}

