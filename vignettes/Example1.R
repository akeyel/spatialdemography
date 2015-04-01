## ------------------------------------------------------------------------
# Optionally set the working directory. The working directory should default to the vignettes directory. If it is changed, some of the inputs from the input files will need to be copied into the changed directory.
#setwd("C:/docs/beplants/Scripts/spatialdemography/vignettes") 

#Load the spatialdemography package
library(spatialdemography)

# Set up a RunName. Not actually an input, but we'll use it to build some of the required inputs
run.name = "spdem_ex1"

## Set up required inputs

# scn = indicator variable to denote scenario and to extract appropriate row from initial.conditions.file and settings.file.
# If only one scenario is run, this should be 1, otherwise it should be updated inside a loop with one SpatialDemography command for each scenario.
scn = 1 

# s.lbl A label used in the construction of the Species file and new landscape files.
# Here we will leave it blank, but it may be relevant when running multiple scenarios.
s.lbl = ""

# file.ending A text string that matches the end of the species file (either to be generated or that already exists).
file.ending = sprintf("_%s", run.name)

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
ResultsFile = sprintf("%sResults%s.csv",opath,file.ending)

#Code to (optionally) delete previous versions of the ResultsFile and TimeFile.
#**#
do.del = 1
if (do.del == 1){
  unlink(ResultsFile) #Delete any previous ResultsFile
  }

## ------------------------------------------------------------------------
#Set the path where your inputs are stored
input.path = sprintf("%sinputs/",run.path)

# initial.conditions.file A file to specify the initial model conditions.
initial.conditions.file = sprintf("%sInitial_conditions%s.csv",input.path,file.ending)

# Here we will read in the file, to look at its contents
ic = read.csv(initial.conditions.file)

## ------------------------------------------------------------------------
knitr::kable(head(ic[1:5]))

## ------------------------------------------------------------------------
knitr::kable(head(ic[6:9]))

## ------------------------------------------------------------------------
knitr::kable(head(ic[10:13]))

## ------------------------------------------------------------------------
knitr::kable(head(ic[14:20]))

## ------------------------------------------------------------------------
# settings.file A file to specify overall model settings.
settings.file = sprintf("%sSettings%s.csv",input.path,file.ending)

set = read.csv(settings.file)
knitr::kable(head(set[1:5]))

## ------------------------------------------------------------------------
knitr::kable(head(set[6:7]))

## ------------------------------------------------------------------------
knitr::kable(head(set[8:12]))

## ------------------------------------------------------------------------
# env.file A file to describe environmental layers and how they change.
env.file = sprintf("%sEnvironmental_layers%s.csv",input.path,file.ending)

ef = read.csv(env.file)
knitr::kable(head(ef[1:2]))

## ------------------------------------------------------------------------
knitr::kable(head(ef[3:8]))

## ------------------------------------------------------------------------
knitr::kable(head(ef[9:11]))

## ------------------------------------------------------------------------
lpath = "spdem_ex1/landscape/"
lc.file = sprintf("%slandcover_scn1.csv",lpath)
h.file = sprintf("%sherbicide_scn1.csv",lpath)

lc = read.csv(lc.file)
knitr::kable(lc)
#Above is the basic file structure

lc2 = matrix(lc[1,2:length(lc)], nrow = 2, byrow = TRUE) #starts at 2 to avoid the change.step
lc2
#Above is the layer in landscape format for the first change step (landcover in this example only has one change step)

#We will now examine the herbicide layer:
#Please note that the change.step in the landscape files is equal to the timestep + 1, as the change takes effect in the NEXT model run
#Forest cells (2) are not treated with herbicides.

#Herbicides
#The herbicides layer has many change steps specified because it does not change predictably at a regular interval (i.e. herbicides are applied twice, and then the layer reverts back to no application.)
herb = read.csv(h.file)
knitr::kable(herb)

## ------------------------------------------------------------------------
# spfile A file specifying species traits.
# This file should be in the folder Species (it is not in the inputs folder because sometimes a species file is an output of SpatialDemography)
spfile = sprintf("%sSpecies/Species%s.csv",run.path, file.ending)

# sp.instr.file A file to describe how generation of species' base vital rates is to occur
# Optional file only needed if SpatialDemography is generating species so we'll set it to NA (it can also be absent entirely)
sp.instr.file = NA      

# sp.resp.instr.file A file to describe how generation of species' response traits is to occur
# Optional file only needed if SpatialDemography is generating species, so we'll set it to NA (it can also be absent entirely)
sp.resp.instr.rile = NA #Optional file only needed if SpatialDemography is generating species

#Examine the species file
spf = read.csv(spfile)
knitr::kable(head(spf))

## ------------------------------------------------------------------------
locations.file = sprintf("%slocations%s.csv", input.path, file.ending)
locs = read.csv(locations.file)
knitr::kable(locs)


## ------------------------------------------------------------------------
#**# Only run when troubleshooting code. Delete from final vignette
dev.mode = 1
if (dev.mode == 1){
  source("C:/docs/beplants/Scripts/spatialdemography/R/sdhelper.r")
  source("C:/docs/beplants/Scripts/spatialdemography/R/spatialdemography.r")
  source("C:/docs/beplants/Scripts/spatialdemography/R/simulation.r")
  library(Matrix)
  library(multirich)
  }

restart = 0
if (restart == 1){
  myworkspace = "TS.RData"
  load(myworkspace)
  }


## ------------------------------------------------------------------------
# set.seed function makes the stochastic results deterministic for repeatability purposes
# Not actually relevant for this example, as there are no stochastic processes included.
set.seed(567891234)

#Run SpatialDemography. This is the main command to run the model, and uses the inputs defined above.
SpatialDemography(scn,s.lbl,file.ending,DispPath,run.path,opath,ResultsFile,initial.conditions.file,settings.file,env.file,spfile,sp.instr.file,sp.resp.instr.file,lpath,locations.file)


## ------------------------------------------------------------------------
#Results file
out.results = read.csv(ResultsFile)
knitr::kable(out.results)

## ------------------------------------------------------------------------
#Next, we will consider the scenario-specific files.
epath = "spdem_ex1/Example1/"

#SpeciesData.csv will contain species abundances of each lifestage (and all lifestages together) for every cell in the landscape.
sp.dat = read.csv(sprintf("%sSpeciesData.csv",epath))
knitr::kable(head(sp.dat))

#These data are summarized in the following files (in the Example1 folder):
#Cells_occupied.csv indicates how many cells are occupied by each species at each timestep
#SpeciesStats_Adults_v2.csv indicates how many adults of each species there is in the landscape for each timestep, and also the log of lambda (growth rate).
#SpeciesStats_All_v2.csv is the same as SpeciesStats_Adults_v2.csv, except for all life stages pooled.
#SpeciesStats_Juvs.csv indicates how many juveniles there are of each species in the landscape, but does not include loglambda values
#SpeciesStats_Seeds.csv as with juveniles, but for seeds

#One other file in this folder bears note: change_count_lookup.csv provides an index for which change steps correspond to which timesteps (because a change in environmental conditions may be less frequent than a change in timesteps).


#Finally, we will consider the diagnostic files.
dpath = sprintf("%sDiagnostics/", epath)

#There are four files in this folder:
# AMatrices.csv contains the entire matrix A for each species for each timestep. This can be useful for checking that vital rates are as intended and that the matrix was properly constructed.

#TransitionMatrices.csv gives the transition matrices within a cell for each cell in the landscape, and can be useful for examining local dynamics exclusive of immigration and emigration.
# AMatricesSummaries.csv summarizes the matrix content for each cell
mat.sum = read.csv(sprintf("%sAMatricesSummaries.csv",dpath))
knitr::kable(head(mat.sum))

#MatrixDiagnostics.csv summarizes the matrices across all cells for a species for each change step.
mat.dia = read.csv(sprintf("%sMatrixDiagnostics.csv",dpath))
knitr::kable(mat.dia)



## ------------------------------------------------------------------------
#We'll start by looking at the SpeciesData.csv a bit more closely.
#Specifically, we'll look at the adult numbers for each species (rounded for ease of interpretation)
sp.dat.ad = sp.dat[sp.dat$LifeStage == "Adults", ]
sp.dat.ad$LifeStage = NULL
#sp.dat.ad = sp.dat.ad[order(sp.dat.ad$Species), ]
sp.dat.ad1 = sp.dat.ad[sp.dat.ad$Species == 1, ]
sp.dat.ad2 = sp.dat.ad[sp.dat.ad$Species == 2, ]
sp.dat.ad3 = sp.dat.ad[sp.dat.ad$Species == 3, ]

knitr::kable(round(sp.dat.ad1,0)) #Examine Species 1 Adult data


## ------------------------------------------------------------------------
knitr::kable(round(sp.dat.ad2,0)) #Examine Species 2 Adult data

## ------------------------------------------------------------------------
knitr::kable(round(sp.dat.ad3,0)) #Examine Species 3 Adult data

## ----, fig.show = 'hold'-------------------------------------------------
#Graphically examine the overall patterns by species
# Read in SpeciesStats_Adults_v2.csv
ssa = "spdem_ex1/Example1/SpeciesStats_Adults_v2.csv"
my.df = read.csv(ssa)

x.limit = 15
y.limit = 20000

#Plot Species 1
plot(my.df$RunTime, my.df$Sp1, xlab = "Time Step", ylab = "Number of Adults", xlim = c(0,x.limit), ylim = c(0, y.limit), pch = 15, col = 4)

par(new = T) #Tell R to add next plot to existing plot

#Plot Species 2
plot(my.df$RunTime, my.df$Sp2, xlim = c(0,x.limit), ylim = c(0, y.limit), pch = 16, col = 2, xlab = "", ylab = "", xaxt = 'n', yaxt = 'n')

par(new = T) #Tell R to add next plot to existing plot

#Plot Species 3
plot(my.df$RunTime, my.df$Sp3, xlim = c(0,x.limit), ylim = c(0, y.limit), pch = 17, col = 3, xlab = "", ylab = "", xaxt = 'n', yaxt = 'n')

## Examine Species 3's performance by cell
x.limit = 15
y.limit = log(7000,10)

#Plot Cell1
plot(sp.dat.ad3$TimeStep,log(sp.dat.ad3$Cell1,10), xlim = c(0,x.limit), ylim = c(0,y.limit), pch = 1, col = "orange", xlab = "Time Step", ylab = "Log10 Number of Adults" )

#Plot Cells 2 - 4
for (i in 2:4){
    pch.vec = c(NA,6,20,0)
    par(new = T) #Tell R to add next plot to existing plot
    this.cell = sprintf("Cell%s", i)
    plot(sp.dat.ad3$TimeStep, log(sp.dat.ad3[[this.cell]],10), xlim = c(0,x.limit), ylim = c(0, y.limit), pch = pch.vec[i], col = i, xlab = "", ylab = "", xaxt = 'n', yaxt = 'n')
    }



