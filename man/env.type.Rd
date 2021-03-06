% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spatialdemography.r
\name{env.type}
\alias{env.type}
\title{env.type input}
\description{
env.type determines how each landscape element will be set up.
}
\details{
Any of the distribution functions (see 
         \link[spatialdemography]{distribution.functions} will work here. 
         Additionally, the following options are available:
\itemize{
\item cover specifying cover will set up the element based on percent cover, 
      with required parameters being specified in the cover.levels and param1 
      fields. The field cover.levels specifies the cover levels to be 
      generated, each separated by a semicolon (no semicolon is needed if 
      only one cover class is present).  This should be the proportion of 
      cells in each cover type (e.g., 1 would give one cover type with 100% 
      cover, 0.5;0.5 would give two cover types each with 50% cover, and 0.5;
      0.25;0.25 would give 3 cover types the first with 50% cover, the second 
      with 25% cover, and the third with 25% cover.  The first number 
      assigned is determined by param1, and numbers follow sequentially and 
      increasingly from there.  (e.g., if param1 = 1, then the third example 
      above would generate cover levels of 1,2,3). The param2 field should be 
      set to NA. 
\item rnorm will set the landscape element up based on values drawn from a 
      normal distribution where the mean is given by the value of param1 and 
      the standard deviation by the value of param2. The cover.levels field 
      should be set to NA.
}
}

