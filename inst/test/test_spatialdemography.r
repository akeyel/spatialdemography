# CODE FOR TESTING THAT THE MODEL IS FUNCTIONING PROPERLY IN TESTABLE RESPECTS

#Main body of code borrowed from MC_model.r

#**# Think about converting the test code to vignettes?

#**# REVELATION: ../ refers to the parent directory with relative paths.  THIS IS COOL!  And probably utterly obvious to anyone who is actually a programmer...

#Tests should test all options, but not necessarily all interactions of options (because that would be a lot of testing)

#**# Figure out how to remove path dependency (will be easier for non-development mode code)
#**# Need to add clean up code that deletes the generated files
#**# Would file deletion be easier if the base code were all living somewhere else, and got copied into the appropriate folder?)
  #**# make that happen.

#TestRun1:
  # Test species generation
  # Test landscape generation
  # Test conditional landscape generation
  # Test initial location assignment (numeric)
  # Test with species invasion
  # Test with environmental change
  # Edge type = TORUS
  # 10 species at regional
  # 5 species in landscape
  # 1 species local
  # competition type = 0
  # multispecies.K = 1

#TestRun2
  # Test reading in species from file
  # Test read in landscape from file
  # Test initial location assignment (from file)
  # No species invasion
  # Test with environmental change
  #Edge type = TORUS
  # 2 species regional, landscape, and local
  # competition.type = 1
  # multispecies.K = 0

#TestRun3 #**# STILL NEEDS TO BE SET UP
  # Read species from file
  # Test with 1 species
  # test with 1 environmental layer
  #Test with landscape extent of 1
  #Edge type = ABSORBING
 
#TestRun4 #**# STILL NEEDS TO BE SET UP - USE A RUN FROM STOCHASTIC PLANTS
  # Test copula function
  # Include more variation in vital rates


#TestRun5 #**# STILL NEEDS TO BE SET UP - Add when doing theoretical traits paper
  # Generate species
  # Generate landscape
  # Test initial location assignment (biomass)
  # Edge type = ABSORBING
  # competition.type = 2
  # 32 species regional,  16 landscape and 4 local
  # RTD: 8 RTD total, 4 landscape, 2 local (4 x 4 landscape)
  # Have multiple scenarios, have each scenario test an environmental change type


#**# ALSO NEED TO SET UP SOME TESTS THAT WILL FAIL
  #Do so below, and wrap with a try/catch

# Fail RTD generation
# Fail conditional landscape variable
# other important fails?

#**# How to make this more general?? E.g., figure out where R can read example files, and use these as examples
setwd("C:/docs/beplants/Scripts/spatialdemography/inst/test/")
DispPath = "dt/"

dev = 0

if (dev == 0) library(spatialdemography)
if (dev == 1){
    #For development mode
    source("../../R/spatialdemography.r")
    source("../../R/simulation.r")
    source("../../R/sdhelper.r")
    #library(multirich)
    library(Matrix)
    }

#Source functions that ought to be in popdemo package, but don't work there.
#source("../../stottmatrix.r") #**# No longer necessary - these functions are in the package now

RunLog = "RunLog.csv"
if (file.exists(RunLog)){
    unlink(RunLog)
    }

to.run = c("TestRun1","TestRun2") #,"TestRun3","TestRun4","TestRun5" 
run.lbls = c("tr1","tr2","tr3","tr4","tr5")
for (r in 1:length(to.run)){
#r = 2

    #Set up run name and labels
    run.name = to.run[r]
    run.lbl = run.lbls[r]

    # Delete any previous test-run folder/files
    if (file.exists(run.name)){
        cat(sprintf("Old test run data for %s deleted\n", run.name), file = RunLog, append =T)
        unlink(run.name, recursive = T)
        }

    # Copy set up files into the appropriate location(s)
    # get listing of previous files
    base.dir = sprintf("testfiles/%s/", run.lbl)
    my.files = test.setup(base.dir, run.name)
      initial.conditions.file = my.files[[1]]
      settings.file = my.files[[2]]
      env.file = my.files[[3]]
      spfile = my.files[[4]]
      sp.instr.file = my.files[[5]]
      sp.resp.instr.file = my.files[[6]]
      locations.file = my.files[[7]]

    ## Run test & generate output ##
    # Create a path to store results & create results file
    file.ending = run.name #Could just put run name into the code, but this makes it faster when troubleshooting
    run.path = sprintf("%s/",run.name) #Set run path to run name
    opath = sprintf("%s/outputs/",run.name)
    dir.create(opath,showWarnings = F)    
    ResultsFile = sprintf("%sResults%s.csv",opath,file.ending)
    if (file.exists(ResultsFile)) file.remove(ResultsFile) #delete any existing results file

    landscape.dir = "default"
    if (r == 2){
        landscape.dir = "TestRun2/landscape/"
        }

    # set up scenarios #sc = 1 #**# Set up for more scenarios when testing multiple scenarios
    scn = 1 #scns[sc] #Scenario to run
    s.lbl = "" #s.lbls[sc]     #Label for the species file to be created

    #Set seed for random processes, to ensure consistent results
    set.seed(123456789) #Set seed, to allow consistent results from the random numbers generators.

    #Run SpatialDemography with testing mode (will load seeds to ensure repeatability
    out.res = SpatialDemography(scn,s.lbl,file.ending,DispPath,run.path,opath,ResultsFile,initial.conditions.file,settings.file,env.file,spfile, sp.instr.file,sp.resp.instr.file, landscape.dir = landscape.dir, locations.file = locations.file, testing = T)
    
    # Check generated output for accuracy & report any errors
    check.outputs(run.name,RunLog)
    
    # If no errors, delete test files
    cat(sprintf("%s run successfully with no errors. Deleting generated files.\n", run.name), file = RunLog, append = T)
    unlink(run.name, recursive = T)

    }                 

