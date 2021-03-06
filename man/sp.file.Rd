% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spatialdemography.r
\name{sp.file}
\alias{SpTraits}
\alias{dispersal}
\alias{sp.file}
\alias{species.file}
\alias{spfile}
\title{Species File}
\description{
Input file giving the vital rates and response traits for each species.
Either this file OR both the Species Instructions File
\link[spatialdemography]{sp.instr.file} and the Species Response 
Instructions file \link[spatialdemography]{sp.resp.instr.file} are required.
}
\details{
\tabular{ll}{
Input              \tab Description \cr
sp                 \tab A numeric indicator for species \cr
p01                \tab Transition rate from stage 0 to stage 1.\cr
p02                \tab Transition rate from stage 0 to stage 2.\cr
p11                \tab Survival rate of stage 1 (proportion of stage 1 
                        individuals that remains in stage 1).\cr
p12                \tab Transition rate from stage 1 to stage 2.\cr
p22                \tab Survival rate of stage 2 (proportion of stage 2 
                        individuals that remains in stage 2).\cr
p23                \tab Transition rate from stage 2 to stage 3.\cr
p30                \tab Number of stage 0 individuals produced by stage 3.\cr
p32                \tab Number of stage 2 individuals produced by stage 3.\cr
p33                \tab Survival rate of stage 3 (proportion of stage 3 
                        individuals that remain in stage 3).\cr
biomass.adult      \tab The species' adult biomass (g).\cr
biomass.juv        \tab The species juvenile biomass (g), only needed if 
                        assigning initial numbers by biomass. \cr
biomass.seed       \tab The species' seed mass (g), only needed if assigning 
                        initial numbers by biomass. \cr
dispersalfunction  \tab A number indicating which dispersal function to use.  
                        Log-normal is indi-cated by 1.\cr
disppar1           \tab The first input parameter for the dispersal function.  
                        For log-normal, this is the mean dispersal distance, 
                        in cells (not log transformed).\cr
disppar2           \tab The second input parameter for the dispersal function.  
                        For log-normal, this is the dispersal standard 
                        deviation in cells (log-transformed).\cr
<Response Traits>  \tab Zero or more response traits describing how the 
                        species will respond to environmental layers. Each 
                        entry is delimited by ';', and has four components: 
                        the 'optimal' value, a code for the function for how 
                        the vital rate will change relative to the optimal 
                        value, and two parameters for the function.  
}
}

