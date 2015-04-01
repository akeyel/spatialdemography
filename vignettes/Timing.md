---
title: "SpatialDemography Timing"
author: "Alexander C. Keyel <skeyel@gmail.com>"
date: "2015-02-13"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Vignette Title}
  %\VignetteEngine{knitr::rmarkdown}
  \usepackage[utf8]{inputenc}
---
## Introduction

This Vignette was constructed primarily to test the timing of the model, and secondarily as a vignette. It will demonstrate how to run multiple scenarios and how to use R objects as inputs instead of files (but we will still generate those R objects from files for convenience).

This example will demonstrate how to automatically generate species, how to automatically generate a landscape, and how to assign the species to the landscape without specifying the species locations. We will consider different landscape extents, different species richnesses, different numbers of time steps and different frequencies of environmental change.

Running all scenarios in this code will be time intensive and will generate a large number of output files.
Only the first scenario is run by default.

The code includes an example for how to run multiple scenarios in parallel using a computing cluster. This will require the parallel package (included with the base R distribution)

## Set up paths and labels
In order to run SpatialDemography, we need to set up several variables to ensure that the code runs properly. Mainly these inputs have to do with paths and file locations.


```r
# Optionally set the working directory. The working directory should default to the vignettes directory. If it is changed, some of the inputs from the input files will need to be copied into the changed directory.
#setwd("C:/docs/beplants/Scripts/spatialdemography/vignettes") 

dev.mode = 1
if (dev.mode == 1){
    source("C:/docs/beplants/Scripts/spatialdemography/R/sdhelper.r")
    source("C:/docs/beplants/Scripts/spatialdemography/R/spatialdemography.r")
    source("C:/docs/beplants/Scripts/spatialdemography/R/simulation.r")
    library(Matrix)
    library(multirich)
}else{#Load the spatialdemography package
    library(spatialdemography)
    }

# Set up a RunName. Not actually an input, but we'll use it to build some of the required inputs
run.name = "spdem_timing"

# DispPath The path of the dispersal tables (if existing) or the path where dispersal tables should be created.
# Note that dispersal tables may take a long time to generate, so it is often desirable to share the same dispersal tables across multiple models.
DispPath = "dt/"

# run.path The main path for the specific overall model run.
run.path = sprintf("%s/",run.name) 

#The path for model outputs.
opath = sprintf("%soutputs/",run.path)

#Create the output path
dir.create(opath,showWarnings = F, recursive = T)

# A file to be created containing the model results.
# If there is an existing file, the results will be appended to the existing file.
ResultsFile = sprintf("%sResults_%s.csv",opath,run.name)

## Set up timing scenarios

# Set up the levels of variables for evaluation of model timing
species.vec = c(1,10,100) #1, 10, and 100 species
extent.vec = c(1,4,20) # 1 cell, 16 cells, 400 cells
change.vec = c(0,5,1) # No change, change every 5 timesteps, change every single timestep
timestep.vec = c(1,11,21) #One timestep, 11 timesteps, 21 timesteps (increment by 10)
vdb.vec = c(0,1) # Whether or not to generate many of the outputs in the model
matrix.diagnostics.vec = c(0,1) #Whether or not to generate many of the matrix diagnostic outputs

# Set default values for evaluation of other variables
spv = 10
exv = 4
chv = 5
tsv = 5
vdv = 0
mdv = 0

#Get the number of scenarios to be run
num.scn = length(species.vec) + length(extent.vec) + length(change.vec) + length(timestep.vec) + length(vdb.vec) + length(matrix.diagnostics.vec)

# Create an index of scenario numbers
in.seq = seq(1,num.scn)

# Set up vectors of inputs to be parallel with one another
sp.vec.pre = c()
sp.vec.val = species.vec #Create a vector of species values, with most scenarios run for 10 sp.
sp.vec.post = rep(spv, (num.scn - length(sp.vec.val)))
sp.vec = c(sp.vec.pre,sp.vec.val,sp.vec.post)

extent.vec.pre = rep(exv,length(sp.vec.pre) + length(sp.vec.val))
extent.vec.val = extent.vec
extent.vec.post = rep(exv, (num.scn - length(extent.vec.pre) - length(extent.vec.val)))
ex.vec = c(extent.vec.pre, extent.vec.val, extent.vec.post)

ch.vec.pre = rep(chv,num.scn - length(extent.vec.post))
ch.vec.val = change.vec
ch.vec.post = rep(chv, (num.scn - length(ch.vec.pre) - length(ch.vec.val)))
ch.vec = c(ch.vec.pre,ch.vec.val,ch.vec.post)

ts.vec.pre = rep(tsv, num.scn - length(ch.vec.post))
ts.vec.val = timestep.vec
ts.vec.post = rep(tsv, num.scn - length(ts.vec.pre)- length(ts.vec.val))
ts.vec = c(ts.vec.pre,ts.vec.val,ts.vec.post)

vd.vec.pre = rep(vdv, num.scn - length(ts.vec.post))
vd.vec.val = vdb.vec
vd.vec.post = rep(vdv, num.scn - length(vd.vec.pre) - length(vd.vec.val))
vd.vec = c(vd.vec.pre,vd.vec.val,vd.vec.post)

md.vec.pre = rep(mdv, num.scn - length(vd.vec.post))
md.vec.val = matrix.diagnostics.vec
md.vec.post = rep(mdv, num.scn - length(md.vec.pre) - length(md.vec.val))
md.vec = c(md.vec.pre,md.vec.val,md.vec.post)

#Check that vectors were correctly constructed. Note how only one vector has non-default values at once.
sp.vec
```

```
##  [1]   1  10 100  10  10  10  10  10  10  10  10  10  10  10  10  10
```

```r
ex.vec
```

```
##  [1]  4  4  4  1  4 20  4  4  4  4  4  4  4  4  4  4
```

```r
ch.vec
```

```
##  [1] 5 5 5 5 5 5 0 5 1 5 5 5 5 5 5 5
```

```r
ts.vec
```

```
##  [1]  5  5  5  5  5  5  5  5  5  1 11 21  5  5  5  5
```

```r
vd.vec
```

```
##  [1] 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0
```

```r
md.vec
```

```
##  [1] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1
```

```r
## Set up initial.conditions and settings using Example1 as a template.
#Set the path where your inputs are stored
input.path = sprintf("spdem_ex1/inputs/")
ex1.ending = "_spdem_ex1"

# Read in the initial conditions file
initial.conditions.file = sprintf("%sInitial_conditions%s.csv",input.path,ex1.ending)
ic.ex1 = read.csv(initial.conditions.file)
#head(ic)

# Read in the settings file
settings.file = sprintf("%sSettings%s.csv",input.path,ex1.ending)
set.ex1 = read.csv(settings.file)
#head(set)

## Set up the other input files: env.file, species.instr.file, species.resp.file

#Read in environmental layers file
input.path = sprintf("spdem_timing/inputs/") #Change input path to correspond to the current folder.
env.file = sprintf("%sEnvironmental_layers_%s.csv",input.path,run.name)
ef = read.csv(env.file)
#ef #This will display the values of the environmental layers file
#?env.file #This will provide more details on the environmental layers file

#Read in species instructions file
sp.instr.file = sprintf("%sSpecies_instructions_spdem_timing.csv",input.path)
sif = read.csv(sp.instr.file)
#sp.instr.file  #This will display the values for the timing scenario 
#?sp.instr.file #This will provide more details on the format and codes used in the species instructions file

#Read in species response instructions file
sp.resp.instr.file = sprintf("%sSpecies_response_instr_spdem_timing.csv", input.path)
srif = read.csv(sp.resp.instr.file)
#srif #This will display the species response file values
#?sp.resp.instr.file #This will give more details on what the species response file values mean.

# Modify the base settings to match the timing scenario requirements
ic.ex1$num.adults = 1000 #Add num.adults field and initialize with 1000 of each species

set.ex1$GenerateNewSpecies = 1
set.ex1$GenerateNewLandscape = 1
set.ex1$loc.extraction = NULL
set.ex1$timestep.extraction = NULL
set.ex1$locations = NULL
set.ex1$ignore.rtd = 1
set.ex1$Write.Timefile = 1 #Only output the total model run time (2 would also reveal which sections of the model scale the worst) #**# Add to cluster version & set to 2 there!

ic = ic.ex1
set = set.ex1
#Expand the dataframes to contain the requisite number of scenarios (environmental layers does not need to be expanded)
for (i in 2:num.scn){
    ic = rbind(ic,ic.ex1)
    set = rbind(set,set.ex1)
    }

#Update the dataframes with the vectors
ic$Scenario = in.seq
ic$ModelName = sprintf("Timing%s",in.seq)
ic$Extent = ex.vec
ic$MaxTime = ts.vec
#set total species richness to equal landscape species richness to equal patch species richness.
#Not ecologically realisitc, but this model is being run for timing purposes, and not ecological realisim.
ic$Tot.sp.rich = ic$Land.sp.rich = ic$Loc.sp.rich = sp.vec  

set$Scenario = in.seq
set$GenerateVisualDebuggerData = vd.vec
set$RunMatrixDiagnostics = md.vec
set$lnd.lbl = sprintf("scn%s",in.seq)


#Code to (optionally) delete previous versions of the ResultsFile and TimeFile.
#**#
do.del = 1
if (do.del == 1){
  unlink(ResultsFile) #Delete any previous ResultsFile
  }

#Create a function to apply on the cluster
do.spdem = function(x, DispPath,run.path,opath,ResultsFile,ic,set,ef,sp.instr.file,sp.resp.instr.file,ch.vec){
    library(spatialdemography)
    scn = x 

    # s.lbl A label used in the construction of the Species file and new landscape files.
    s.lbl = sprintf("scn%s",x)

    # file.ending A text string that matches the end of the species file (either to be generated or that already exists).
    file.ending = sprintf("_%s_%s", run.name, s.lbl)
    
    #Update env.file to correspond to current change scenario
    ef$env.change.freq[1] = ch.vec[x]
    # Note: most scenarios will have more than one environmental layer, so it is not possible to structure the environmental
    # layers file in the same way as the initial.conditions file or the settings file. Hence why this data frame is updated here.

    # Create a file name & directory for the species file
    sp.path = sprintf("%sSpecies/",run.path)
    dir.create(sp.path,showWarnings = F, recursive = T)
    spfile = sprintf("%sspecies_file%s.csv", sp.path, file.ending)
    
    # set.seed function makes the stochastic results deterministic for repeatability purposes
    set.seed(567891234)

    #Run SpatialDemography. This is the main command to run the model, and uses the inputs defined above.
    SpatialDemography(scn,s.lbl,file.ending,DispPath,run.path,opath,ResultsFile,ic,set,ef,spfile,sp.instr.file,sp.resp.instr.file)    
    }

cpu = "PC"
#cpu = "cluster"

if (cpu == "cluster"){
    cores = 1
    workspace = "Timing_prerun.RData"
  
    library(parallel)
    workers      <- makeCluster(getOption("cl.cores", cores))     # Start worker processes
    save.image("Timing_prerun.RData")
  
    clusterEvalQ(workers, load("Timing_prerun.RData")) # Add workspace to each worker
    clusterApplyLB(cl=workers, x= in.seq, fun=do.spdem, DispPath,run.path,opath,ResultsFile,ic,set,ef,sp.instr.file,sp.resp.instr.file,ch.vec) # This is the main call for the analysis
  
    stopCluster(cl=workers)    # Once all analyses are done the workers are closed
}else{
    #Only run once if not running it on a cluster. This will prevent the vignette from running all scenarios automatically.
    #If not using a cluster, 1:1 can be replaced by in.seq. That will run all scenarios.
    for (i in 1:1){
        do.spdem(i, DispPath,run.path,opath,ResultsFile,ic,set,ef,sp.instr.file,sp.resp.instr.file,ch.vec)
        }
    }
```

```
## 
## Attaching package: 'spatialdemography'
## 
## The following objects are masked _by_ '.GlobalEnv':
## 
##     calc.beta, calculate.traits, check.outputs, Digit,
##     do.input.rename, import.env.lyr, make.disp.prob.v2, quant.reg,
##     setup.env.change.mag, setup.inputs, SpatialDemography,
##     stoch.plant.setup, test.setup
```

```r
#Timefile 
#This file records the timing of different steps of the model, and can be used for optimization purposes.
TimeFile = sprintf("%sTimefile.csv",opath)

timing = read.csv(TimeFile)
knitr::kable(timing)
```



|Model   | ElapsedTime|
|:-------|-----------:|
|Timing1 |        0.38|
|Timing1 |        0.33|


