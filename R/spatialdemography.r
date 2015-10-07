# This file is part of the spatialdemography package.  
# See package information for license & details

# Set up function to evaluate script
#' SpatialDemography = Main function for running the spatially explicit, 
#' matrix-based, metacommunity model
#' 
#' This function implements a spatially explicit matrix model based on input
#' options. Inputs are pointers to files, which contain additional inputs used
#' by the function to run the model.
#' 
#' Details for each of the inputs in the input files: Please see Overview,
#' Design concepts, Details (ODD, Grimm et al. 2006, 2010) description for
#' SpatialDemography %% Figure out how to link to this here
#' 
#' @param scn Indicator variable to denote scenario and to extract appropriate
#' row from initial.conditions.file and settings.file.
#' @param s.lbl A label used in the construction of the Species file and new 
#' landscape files.
#' @param file.ending A text string that matches the end of the species file
#' (either to be gener-ated or that already exists).
#' @param DispPath The path of the dispersal tables (if existing) or the path 
#' where dispersal tables should be created.
#' @param run.path The main path for the specific overall model run.
#' @param opath The path for model outputs.
#' @param ResultsFile A file to be created containing the model results.
#' @param initial.conditions.file A file to specify the initial model 
#' conditions. See \link[spatialdemography]{initial.conditions.file} for details.
#' @param settings.file A file to specify overall model settings. See 
#' \link[spatialdemography]{settings.file} for details.
#' @param env.file A file to describe environmental layers and how they change.
#' See \link[spatialdemography]{env.file} for details.
#' @param spfile A file specifying species traits.
#' See \link[spatialdemography]{sp.file} for details.
#' @param sp.instr.file A file to describe how generation of species' base 
#' vital rates is to occur (Optional - not needed if using pre-existing species
#' file). See \link[spatialdemography]{sp.instr.file} for details.
#' @param sp.resp.instr.file A file to describe how generation of species' 
#' response traits is to occur (Optional - not needed if using pre-existing 
#' species file). See \link[spatialdemography]{sp.resp.instr.file} for details.
#' @param landscape.dir (not used if generating new landscapes) The directory  
#' where the landscape files may be found or a list object containing the
#' landscape information. See \link[spatialdemography]{landscape.files} for
#' more information on the files, and see
#' \link[spatialdemography]{landscape.object} for information on using a
#' landscape generated in R.
#' @param locations.file (should be 'none' if spatialdemography is assigning  
#' species to the landscape) A file containing initial locations of species.  
#' See (\link[spatialdemography]{locations.file}) for more information
#' about the species locations file itself.
#' @param scale.vec A list of spatial scales at which to evaluate the model. 
#' See \link[spatialdemography]{scale.vec} for more details.
#' @param scale.cells.lst A list of cells from the landscape included in each of  
#' the above spatial scales.  
#' See \link[spatialdemography]{scale.vec} for more details.
#' @param out.metrics Metrics to calculate for the model run. 
#' See \link[spatialdemography]{out.metrics} for details.
#' @param testing An indicator for whether test files are being run. Defaults to F.
#' @return No direct results are returned from the function.  However, the 
#' function writes several files.
#' See \link[spatialdemography]{Results.Outputs} for a summary.
#' @author A.C. Keyel
#' @template mycitation
#' @export SpatialDemography
SpatialDemography = function(scn, s.lbl, file.ending, DispPath, run.path, opath, 
                             ResultsFile, initial.conditions.file, settings.file, 
                             env.file, spfile, 
                             sp.instr.file = "none", 
                             sp.resp.instr.file = "none", 
                             landscape.dir = "default", 
                             locations.file = "none", 
                             scale.vec = c("landscape"), 
                             scale.cells.lst = "default", 
                             out.metrics = c("Sp.Rich", "Beta.Div", "Biomass", 
                                             "FTD.UTC", "RTD.UTC"), 
                             testing = F) {
    
    # For troubleshooting TODO(sten): you may want to remove the following in the published version         
    #landscape.dir = 'default'; 
    #locations.file = 'none'; 
    #scale.vec = c('landscape'); 
    #scale.cells.lst = 'default';
    #out.metrics = c('Sp.Rich','Beta.Div','Biomass','FTD.UTC','RTD.UTC'); 
    #testing = F;
    #initial.conditions.file = ic; settings.file = set; env.file = ef;
    
    # Set up script settings that are fixed & load required packages
    
    # Create a variable for accessing the origianl working directory.  
    # Needed by shiny part of code
    # TODO(sten): uncomment or delete it 
    #bpath = getwd()
    
    S = 4  # Number of stages in the model
    c.gen = NA  # This currently does nothing TODO(sten): But what is it ment to do? 
    rtd.c = 0  # Other options are not yet scripted
    
    # Size of cells in the landscape. This is no longer needed - only affects 
    # dispersal probabilities and carrying capacity, and those should be put in 
    # terms of the cell size external to the model.
    # TODO(sten): You may want to remove this 
    #cell.size = 50
    
    # input for calculation of UTC - tells it not to use a log-transform
    log.trans = 0
    
    # Number of decimal places for which to round trait values. 2 is recommended 
    # with a log-transform, but that may not apply anymore #**# This is still 
    # inappropriate for some columns.  Except if they do not change, it doesn't 
    # matter. & only really matters for scaled UTC, which I'm not currently using.
    # TODO(sten): Modify this comment so other may understand it easyly.
    resolution = 2
    start.time = proc.time()[3]  # Get starting time for model run
    
    # Set up timing information for simulation 
    # (or move this outside too, and pass in as arguments?)
    run.times = c()
    run.lbl = c()
    #run.times = c(run.times,gettime()) 
    #run.lbl = c(run.lbl,'loading files and packages completed')
    
    # Check that scn is a number TODO(sten): name what scn is
    if (is.na(as.num(scn))) {
        stop("scn (scenario identifier) input must be numeric, not character.")
    }
    
    # Check if initial.conditions.file is a string object or has already been 
    # read in as an R object
    if (typeof(initial.conditions.file) == "character") {
        # Read in initial conditions file
        ic = read.csv(initial.conditions.file, row.names = 1, na.strings = "NA")
    } else {
        # Use the initial.conditions.file input as is without reading in.
        ic = initial.conditions.file
    }
    
    # Restrict data in memory to only that pertaining to the scenario at hand
    ic = ic[scn, ]
    
    # Convert initial conditions to variable names 
    # Note: some of these columns should only be read in if they are applicable. 
    # Then you can have them be optional. TODO(sten): explain this more in detail
    Model.Name = as.character(ic[["ModelName"]])
    extent = as.num(ic[["Extent"]])  # as.num is a custom function from myr.r TODO(sten): make this function publically available
    
    # Not from ic, but defined here because it is related to the extent
    p = extent^2  
    edge.type = as.character(ic[["Edge.Type"]])  # Options are TORUS and ABSORBING
    MaxTime = as.num(ic[["MaxTime"]])
    K_g = as.num(ic[["K_g"]])
    multi.species.K = as.num(ic[["multi.species.K"]])
    competitiontype = as.num(ic[["competitiontype"]])
    microsites = as.num(ic[["microsites"]])
    invasion = as.num(ic[["invasion"]])
    num.invaders = as.num(ic[["num.invaders"]])
    cells.to.invade = as.num(ic[["cells.to.invade"]])
    repro.proportion = as.num(ic[["invader.repro.proportion"]])
    
    # num.adult and starting.biomass are extracted if/when needed 
    # TODO(sten): explain that more in detail i.e. where in the code
    tot.sp.num = as.num(ic[["Tot.sp.rich"]])
    # land.sp.num, patch.sp.num, target.tot.rtd, land.rtd, loc.rtd 
    # now extracted at the relevant point in species assignment.  
    # TODO(sten): explain that more in detail
    
    # Read in settings file (or leave as is, if it is an R object)
    if (typeof(settings.file) == "character") {
        settings = read.csv(settings.file, row.names = 1)
    } else {
        settings = settings.file
    }
    # Restrict settings to only the scenario at hand.
    settings = settings[scn, ]
    
    # Convert initial settings to variable names (note: some settings are read 
    # in later in the script, only if needed.  This maintains backwards 
    # compatibility and allows for smaller file sizes (optional columns then are 
    # not required.)
    vdb.data = settings[["GenerateVisualDebuggerData"]]
    #vdb.run = settings[[2]] TODO(sten): delete or explain why it is commented out
    generate.spp = settings[["GenerateNewSpecies"]]
    new.landscape = settings[["GenerateNewLandscape"]]
    
    # Will come in as null if field is missing
    lnd.lbl = as.character(settings[["lnd.lbl"]])
    
    # This will come in as NULL if there is no 'include.copula' column.  
    # For once in my life, that is convenient. TODO(sten): remove convessions
    
    include.copula = settings[["include.copula"]]
    #rtd.classification = rtd.c = settings[['rtd.c']] TODO(sten): delete or explain why it is commented out
    #do.eigen.maps = settings[['GenerateEigenMaps']] TODO(sten): delete or explain why it is commented out
    #do.disp.maps = settings[['GenerateDispersalMaps']] TODO(sten): delete or explain why it is commented out
    
    # May not be of interest if transient dynamics from the simulation is the goal.
    do.diagnostics = settings[["RunMatrixDiagnostics"]]
    
    # May not be of interest if only looking at model diagnostics
    do.simulation = settings[["RunSimulation"]]
    
    # Number of draws for the generation of dispersal tables.  
    # 1,000,000 seems like a good number, but will take a while to generate.
    num.sim = as.num(settings[["N.sim.dispersal.table"]])
    
    # Options 1 = just total time, 
    # 2 = section run times (primarily for optimization)
    write.timefile = settings[["Write.Timefile"]]
    
    # Set write.timefile to default to 0.
    if (length(write.timefile) == 0) {
        write.timefile = 0
    }
    
  # Check if opath exists, if not, create it #**# May want an option to turn this off - people may not want the model writing to their computers without permission! But that's the least of my infractions at this point!
  dir.create(opath, showWarnings = F, recursive = T)
  dir.create(run.path, showWarnings = F, recursive = T)

  #Check if required packages are installed
	check.packages(out.metrics, include.copula)
    if (write.timefile > 0) {
        # Set up file for timing results
        timefile = sprintf("%sTimefile.csv", opath)
        # if (file.exists('timefile')){ file.remove(timefile) } TODO(sten): explain or delete this
    }
    
    # Create a pdf to store graphical outputs
    in.pdf = sprintf("%sPlots_%s.pdf", opath, Model.Name)
    
    # Set up output path for environmental layers
    outpath.base = sprintf("%s%s/", run.path, Model.Name)
    dir.create(outpath.base, showWarnings = F)
    
    ### Set up indicator variables ### 
    # Create a counter to keep track of number of environmental changes
    change.count = 1
    run.times = c(run.times, gettime())
    run.lbl = c(run.lbl, "MCsetup complete")
    
    ## Set up output fields 
    #scale.vec = c('landscape') TODO(sten): delete or explain why it is commented out
    #scale.cells.lst = list(rep(T,p)) TODO(sten): delete or explain why it is commented out
    timepoint.vec = c("Initial", "Final", "Change")
    nrow.results = length(scale.vec) * length(timepoint.vec)
    
    ## Set up results file 
    #Results = setup.results(ResultsFile,out.metrics,scale.vec) TODO(sten): delete or explain why it is commented out
    Results = setup.results.v2(ResultsFile, out.metrics, nrow.results)  
    #ResultsFile,out.metrics,scale.vec, timepoint.vec TODO(sten): delete or explain why it is commented out
    
    # Set Scenario-specific results
    Results$Scenario[1:nrow.results] = Model.Name
    # Remember scn indicates the scenarios used in the loop assigning response trait diversity
    Results$Scenario.Number[1:nrow.results] = scn
    
    # Loop through scale vec and timepoint vec to setup Results file
    count = 0
    for (a.scale in scale.vec) {
        for (a.time in timepoint.vec) {
            count = count + 1
            Results$Scale[count] = a.scale
            Results$Timepoint[count] = a.time
        }
    }
    
    # Create a matrix of euclidean distances between all cells in the landscape.  
    # REQUIRES A SQUARE LANDSCAPE (not even rectangular)
    Distances <- Connectance(p, extent)  #Comes out as a matrix
    # Converts to a vector with each matrix column being stored sequentially 
    # from left to right,
    distances <- as.vector(Distances)
    
    run.times = c(run.times, gettime())
    run.lbl = c(run.lbl, "Setup prior to scenario-setup loops complete")
    
    # Generate or read in species data
    if (generate.spp == 1) {
        # Generate species
        SpTraits = gen.spp(sp.instr.file, sp.resp.instr.file, spfile, tot.sp.num)
    } else {
        # If a file path is specified, read the species in from file
        if (typeof(spfile) == "character") {
            SpTraits = read.csv(spfile, row.names = 1)
        # If not, treat the input as an R object.
        } else {
            SpTraits = spfile
            # Check that the first column was read in as row names. 
            # If not, assign as row names.
            if (length(SpTraits$sp) != 0) {
                rownames(SpTraits) = SpTraits$sp
                SpTraits$sp = NULL
            }
        }
    }
    
    # Check that the number of species in SpTraits is greater than the number of 
    # Species required for the model run
    if (nrow(SpTraits) < tot.sp.num) {
        stop(sprintf("Total species number specified in initial conditions                     
                     (%s)\nis greater than the number of species in the species
                     file used by the model (%s)\nPlease ensure there are
                     sufficient species to accomodate the specified total
                     species number.", 
                     tot.sp.num, nrow(SpTraits)))
    }
    
    run.times = c(run.times, gettime())
    run.lbl = c(run.lbl, "Species Generated")
    
    # Species locations & intial numbers were formerly handled here, now moved 
    # to simulation2.r function
    
    # Note that <<- assigns to the global variable, and because we are within a 
    # function, this is actually necessary. Note: .Random.seed does not exist by 
    # default. It is initialized by setting the seed or using a random number. 
    # (seed is set in testing file). So in many model runs, this will just 
    # bypass this part of code.
    if (exists(".Random.seed")) {
        # Only loads a seed if testing == T
        .Random.seed <<- load.seed(Model.Name, 2, testing)
    }
    
    ## Set up environment 
    # Read in environment file (or leave as is, if it is already an R object)
    if (typeof(env.file) == "character") {
        my.env = read.csv(env.file, na.strings = "NA")
    } else {
        my.env = env.file
    }
    
    # Convert first column to row names (not done with row.names = 1 in read.csv 
    # because they were coming in as factors!
    rownames(my.env) = as.character(my.env$env.lbl)
    my.env$env.lbl = NULL
    
    # Convert from factor to character to avoid problems. TODO(sten): delete or explain why commented out
    # my.env$env.change.mag = as.character(my.env$env.change.mag)
    
    env.c.freq = as.num(my.env[["env.change.freq"]])
    # Check if any environmental layers will change
    is.change = change.check(env.c.freq)
    
    # TODO(sten): explain env.lbl
    env.lbl = row.names(my.env)
    landscape.identifiers = as.character(my.env[["landscape.identifiers"]])
    
    # Check if landscape files need to be created based on a copula Check if 
    # column is included in settings file
    if (length(include.copula) > 0) {
        # check if a copula should be included.  
        # (> 0 indicates a copula should be done, with number indicating
        # the number of copulas desired)
        if (include.copula > 0) {
            setup.copula(my.env, MaxTime, p, include.copula, 
                         run.path, env.lbl, s.lbl)
        }
    }
    
    # If using landscape directory is missing or in the default location, 
    # set to default conditions.
    if (landscape.dir[1] == "default") {
        landscape.dir = sprintf("%s/landscape/", run.path)
    }
    
    # Create list to contain landscape layers
    landscape = list()
    
    if (new.landscape == 1) {
        for (y in 1:nrow(my.env)) {
            env.type = as.character(my.env[y, "env.type"])
            env.info = setup.env.info(env.type, my.env, y)
            cond.lyr = as.character(my.env[y, "cond.lyr"])
            
            if (length(cond.lyr) > 1) {
                stop("Environmental_layers cond.lyr field needs to contain
                      only a single letter (the landscape identifier)")
            }
            
            # If the landscape.identifiers does not need conditional assignment:
            if (is.na(cond.lyr)) {
                base.lyr = get.base.env.lyr(env.info, p)
                act.lyr = get.env.lyr(base.lyr)
                # This was separated into two steps to allow for multiple 
                # configurations of the same underlying values. I've broken that 
                # functionality in the code rewrite at the moment. Otherwise do 
                # conditional assignment.
            } else {
                cond.vals = as.character(my.env[y, "cond.vals"])
                
                # What an awful piece of code.  Outside nesting is it needs to 
                # be a list at the end. Next level is as.num, which requires a 
                # non-list input. Hence the [[1]] after strsplit, because 
                # strsplit creates a list!  Splits on ';'
                cond.vals = list(as.num(strsplit(cond.vals, ";", fixed = T)[[1]]))
                
                # This re-assigns cond.lyr! Just so you know.
                cond.lyr = cv.setup(cond.lyr, landscape, landscape.identifiers)
                act.lyr = get.cond.env.lyr(env.info, p, cond.lyr, cond.vals)
            }
            
            # Append created layer to the landscape
            landscape = append(landscape, list(act.lyr))
            
            # Save initial setup conditions if visual debugging is desired
            if (vdb.data == 1) {
                # Set an indicator for whether or not a new landscape 
                # should be created
                create.new = 1
                outpath = sprintf("%slandscape/", run.path)
                dir.create(outpath, recursive = T, showWarnings = F)
                outfile = sprintf("%s%s_%s.csv", outpath, env.lbl[y], s.lbl)
                write.env.lyr(act.lyr, change.count, extent, outfile, create.new)
                paths = outpath
            }
        }
    } else {
        # new.landscape variable should contain the path to use lnd.lbl 
        # should contain the scenario from the path to use
        
        # Patch to allow no label option
        if (lnd.lbl == "no.lbl") {
            lnd.lbl = ""
        }
        
        # TODO(sten): explain or delete inpath = sprintf('%s/landscape/',new.landscape)
        
        if (typeof(landscape.dir) == "character") {
            inpath = landscape.dir
            
            # Set up landscape from file
            for (y in 1:nrow(my.env)) {
                lbl = env.lbl[y]
                act.lyr = read.landscape.lyr(inpath, lbl, lnd.lbl, 
                                             change.count, p)
                
                landscape = append(landscape, list(act.lyr))
                if (vdb.data == 1) {
                    paths = inpath
                }
            }
        } else {
            landscape = landscape.dir
        }
    }
    
    ## Options to speed up the creation of scale.cells.lst
    if (length(scale.cells.lst) == 1) {
        # If using default of entire landscape, then set to extract results 
        # based on entire landscape.
        if (scale.cells.lst == "default") {
            # T indicates that the cell values should be included in the 
            # calculations
            scale.cells.lst = list(rep(T, p))  
        }
        
        # If only considering one value of an environmental layer 
        #**# This could use improvement/generalization in case someone wanted to 
        #**# look at multiple scales or group multiple values!
        if (substr(scale.cells.lst, 1, 6) == "subset") {
            scale.cells.lst = scl.subset(scale.cells.lst, 
                                         landscape.identifiers, 
                                         landscape)
        }
        
    }
    
    run.times = c(run.times, gettime())
    run.lbl = c(run.lbl, "Scenario Setup complete")
    
    #### Run the simulation for the input parameters #### #disp.pdf,eigen.pdf,
    # TODO (sten): Explain the above mentioned file names
    Results = Simulation(Model.Name, ResultsFile, vdb.data, 
                         timefile, write.timefile, run.times, 
                         run.lbl, start.time, run.path, 
                         DispPath, outpath.base, num.sim, 
                         S, extent, p, 
                         landscape, landscape.identifiers, distances, 
                         settings, ic, rtd.c, 
                         locations.file, landscape.dir, SpTraits, 
                         tot.sp.num, my.env, env.c.freq, 
                         is.change, change.count, env.lbl, 
                         s.lbl, lnd.lbl, competitiontype, 
                         microsites, Results, out.metrics, 
                         scale.vec, timepoint.vec, scale.cells.lst, 
                         resolution, log.trans, MaxTime, 
                         invasion, num.invaders, cells.to.invade, 
                         repro.proportion, K_g, multi.species.K, 
                         edge.type, do.simulation, do.diagnostics, 
                         testing)  #,do.eigen.maps,do.disp.maps #Removed: species.locs, n.seed,n.juv,n.adult,
                                   # TODO (sten) explain why these parameters are commented out.
    return(Results)
}  #END OF FUNCTION


#' Initial Conditions File
#'
#' This file contains the initial model conditions for one or more scenarios.
#' Each row in this file corresponds to a scenario.
#'
#' @details   \tabular{ll}{
#' Input            \tab Description \cr
#' Scenario         \tab A label for each scenario \cr
#' ModelName        \tab A name for each scenario \cr
#' Extent           \tab The number of cells on one edge of the landscape 
#'                       (landscapes are square, and contain Extent2 cells). \cr
#' Edge.Type        \tab The type of edge to use for the simulation. 
#'                       Options are TORUS and ABSORBING. \cr
#' MaxTime          \tab The number of time steps to include in the model. \cr
#' K_g              \tab The adult biomass carrying capacity, in grams.  
#'                       To get K in terms of abun-dance, divide K_g by 
#'                       the biomass of each species. \cr
#' multi.species.K  \tab An variable indicating carrying capacity type.  
#'                       When set to 0 the carrying capacity is calculated 
#'                       for each species separately, when set to 1, 
#'                       the carry-ing capacity is applied to the biomass of 
#'                       ALL species combined. \cr
#' competitiontype  \tab A variable indicating competition type.  
#'                       When 0, stage 0 individuals do not compete for microsites.  
#'                       When 1, stage 0 individuals compete with other individuals 
#'                       of the same species for microsites, 
#'                       when 2, stage 0 individuals compete with all other 
#'                       stage 0 individuals for microsites. \cr
#' microsites       \tab Total number of microsites that can be occupied. \cr
#' invasion         \tab An indicator for whether or not invasion should take 
#'                       place. No invasion takes place when 0, otherwise, the 
#'                       variable sets the invasion frequency in number of 
#'                       timesteps (e.g., 1 would have invasion at every time, 2 
#'                       would have invasion at every 2nd timestep). \cr
#' num.invaders     \tab The number of invaders to be added to a cell. \cr
#' cells.to.invade  \tab The number of cells in the landscape to be invaded. \cr
#' invader.repro.potential  \tab the propagule pressure of the invaders 
#'                       (based on their reproductive potential). \cr
#' starting.biomass \tab The starting biomass to be initialized in each 
#'                       cell. Will be divided evenly among non-dispersing life 
#'                       stages. Only used/needed if initial.n == 2 in 
#'                       \link[spatialdemography]{settings.file}.\cr
#' num.adults       \tab The number of adults to be initialized in each cell. 
#'                       Only applies when automatically assigning species to 
#'                       the landscape. \cr
#' Tot.sp.rich      \tab The number of species to include in the regional 
#'                       species pool.  Species in the regional species pool can 
#'                       be assigned to the landscape, and are available to 
#'                       invade the landscape. When generating species, this 
#'                       will set the number of species to create; when reading 
#'                       in species from file, this will serve as a check that 
#'                       the appropriate number is generated. this species 
#'                       richness will reflect the regional species pool, and 
#'                       can be used for assignment to the landscape or for 
#'                       subsequent invasion. \cr
#' Land.sp.rich     \tab The number of species initially in the landscape. Only 
#'                       relevant when generating species automatically \cr
#' Loc.sp.rich      \tab The number of species assigned per cell. (currently the 
#'                       same number for automatic generation). \cr
#' Tot.rtd          \tab Total species pool response trait diversity. This 
#'                       includes dispersal traits as well as response traits 
#'                       from the species responses file. \cr
#' Land.rtd         \tab Total landscape response trait diversity. \cr
#' Loc.rtd          \tab Total local response trait diversity. \cr
#' }
#' @name initial.conditions.file
NULL


#' Settings File
#'
#' This file contains settings for one or more model scenairios.  Each row
#' in this file corresponds to a scenario
#'
#' @details \tabular{ll}{
#' Input                \tab Description \cr
#' Scenario             \tab A label for each scenario.  Needs to correspond & 
#'                           be in the same order as the Scenario input in the 
#'                           initial conditions file. \cr
#' GenerateVisualDebuggerData \tab An indicator variable. 
#'                           When 0, additional diagnostics are not produced.  
#'                           When 1, additional diagnostics are produced.\cr
#' GenerateNewSpecies   \tab When 1, new species are generated based on the 
#'                           Species Instructions File 
#'                           \link[spatialdemography]{sp.instr.file} and the 
#'                           Species Response Instructions File 
#'                           \link[spatialdemography]{sp.resp.instr.file}. 
#'                           Otherwise, a pointer should be given to indicate 
#'                           which existing Species File 
#'                           (\link[spatialdemography]{sp.file}) to use 
#'                           (the pointer is a short piece of code in the 
#'                           Species file name that follows species_file_ and 
#'                           precedes any ending (e.g., date).\cr
#' GenerateNewLandscape \tab When 1, new landscape files are generated based on 
#'                           the Environmental Layers File 
#'                           (\link[spatialdemography]{env.file}).\cr
#' ignore.rtd           \tab (optional) An indicator variable for whether 
#'                           response trait diversity should be taken into 
#'                           account when assigning species.  
#'                           If 1, specified response trait diversity levels 
#'                           will be met (or an error will be given if they 
#'                           cannot be met), 
#'                           if 0, the model will proceed without respect to 
#'                           those inputs. Not applicable when species locations 
#'                           are read in from file. \cr
#' initial.n            \tab (optional, if column is absent, defaults to a value 
#'                           of 1.) An indicator for how initial species numbers 
#'                           should be assigned.  If absent or 1, 10 times as 
#'                           many juveniles and 100 times as many seeds will be 
#'                           produced (relative to adult numbers). If 2, a 
#'                           starting biomass is specified in the 
#'                           \link[spatialdemography]{initial.conditions.file} 
#'                           and this biomass is divided evenly among all three 
#'                           life stages.  Each life stage starts with a number 
#'                           of individuals equal to 1/3 starting biomass 
#'                           divided by the biomass of an individual 
#'                           (floor division). \cr
#' loc.extraction       \tab (optional) Indicates how extraction from file 
#'                           should proceed. Extraction types are: 1. Read in 
#'                           all life stages from file. 2. Read in locations 
#'                           (presence) from file, but set up abundances based 
#'                           on settings in initial.n. 3. Read in adult 
#'                           locations from file, and set up juveniles and seeds 
#'                           based on settings in initial.n. \cr
#' timestep.extraction  \tab The time step to be extracted from file \cr
#' include.copula       \tab (optional) An indicator for whether generation via 
#'                           a copula is desired.  Number should indicate the 
#'                           number of different copulas desired. \cr
#' N.sim.dispersal.table \tab The number of random draws to be used for 
#'                           obtaining simu-lated dispersal probabilities.\cr
#' RunSimulation        \tab When 1, this indicates that the simulation portion 
#'                           of the model should be run, otherwise the 
#'                           simulation is not con-ducted.\cr
#' RunMatrixDiagnostics \tab When 1, this indicates that additional matrix 
#'                           diagnostics should be computed and written to file, 
#'                           otherwise these are not computed.\cr
#' Write.Timefile       \tab When 0 or absent, no timing is done. 
#'                           When 1, the total scenario time is written to file. 
#'                           When 2, total scenario time and run time details 
#'                           are output to file.\cr
#' }
#' @name settings.file
NULL

#' Environmental Layers File
#'
#' Environmental settings. There should be a row for every environmental layer
#' included in the model. In the case of pre-generated, unchanging landscapes,
#' only the env.lbl, landscape.identifiers, and env.change.freq fields are 
#' important.
#'
#' @details \tabular{lll}{
#' Input          \tab Example  \tab Description\cr
#' env.lbl        \tab mowing   \tab A label for the environmental layer. \cr
#' landscape.identifiers \tab M \tab A single letter identifier for each 
#'                                   environmental layer.\cr
#' env.type       \tab cover    \tab A code for the type of environmental layer. 
#'                                   See \link[spatialdemography]{env.type} 
#'                                   for more details.\cr
#' cover.levels   \tab 0.75;0.25 \tab Cover levels for each value of the 
#'                                   environmental variable (separated by a 
#'                                   semi-colon). Only applicable if env.type is 
#'                                   set to cover. See 
#'                                   \link[spatialdemography]{env.type} for more 
#'                                   details. \cr
#' param1         \tab 0;1      \tab A parameter guiding the construction of the 
#'                                   environ-mental layer. 
#'                                   See \link[spatialdemography]{env.type} for 
#'                                   more details.\cr
#' param2         \tab NA       \tab A second parameter guiding the construction 
#'                                   of the en-vironmental layer. See 
#'                                   \link[spatialdemography]{env.type} for more 
#'                                   details.\cr
#' cond.var       \tab landcover \tab If NA, the current landscape element will 
#'                                   be produced independently.  Otherwise, this 
#'                                   generation of this environmental layer will 
#'                                   depend on the environmental layer specified 
#'                                   here. In theory multiple environmental 
#'                                   layers could be specified. In practice, this 
#'                                   functionality is not fully supported.\cr
#' cond.vals      \tab 1        \tab Indicates which values in the other layer 
#'                                   should be con-sidered potentially suitable 
#'                                   (this layer will be assigned values of 0 
#'                                   for all other values of the conditioning layer). 
#'                                   If cond.var is NA, this should also be NA.\cr
#' env.change.freq \tab 10      \tab Indicates how often the environment should 
#'                                   change. A 0 turns off environmental change. 
#'                                   There is currently no way to get an 
#'                                   irregular change interval, or to stop 
#'                                   environmental change mid-scenario based on 
#'                                   this parameter.\cr
#' env.change.type \tab swap    \tab Indicates what type of environmental change 
#'                                   should take place. See 
#'                                   \link[spatialdemography]{env.change.type} 
#'                                   for more details. \cr
#' env.change.mag \tab NA       \tab Describes the magnitude of the environmental 
#'                                   change. See 
#'                                   \link[spatialdemography]{env.change.type} 
#'                                   for more details.\cr
#' }
#' @name env.file
NULL

#' Species Instructions File
#'
#' Optional input file giving instructions for generation of species base vital 
#' rates. Each vital rate should be a row entry. For each vital rate, three 
#' columns need to be filled in: Function, parameter1, and parameter2 see 
#' \link[spatialdemography]{distribution.functions} for more information. Either 
#' this file and the \link[spatialdemography]{sp.resp.instr.file}
#' OR the \link[spatialdemography]{sp.file} are required.
#'
#' @details \tabular{ll}{
#' Input            \tab Description \cr
#' p01              \tab Transition rate from stage 0 to stage 1.\cr
#' p02              \tab Transition rate from stage 0 to stage 2.\cr
#' p11              \tab Survival rate of stage 1 (proportion of stage 1 
#'                       individuals that remains in stage 1).\cr
#' p12              \tab Transition rate from stage 1 to stage 2.\cr
#' p22              \tab Survival rate of stage 2 (proportion of stage 2 
#'                       individuals that remains in stage 2).\cr
#' p23              \tab Transition rate from stage 2 to stage 3.\cr
#' p30              \tab Number of stage 0 individuals produced by stage 3.\cr
#' p32              \tab Number of stage 2 individuals produced by stage 3.\cr
#' p33              \tab Survival rate of stage 3 (proportion of stage 3 
#'                       individuals that remain in stage 3).\cr
#' biomass.adult    \tab The species' adult biomass (g).\cr
#' biomass.juv      \tab The species juvenile biomass (g), only needed if 
#'                       assigning initial numbers by biomass. \cr
#' biomass.seed     \tab The species' seed mass (g), only needed if assigning 
#'                       initial numbers by biomass. \cr
#' dispersalfunction \tab A number indicating which dispersal function to use.
#'                       Log-normal is indi-cated by 1.\cr
#' disppar1         \tab The first input parameter for the dispersal function.  
#'                       For log-normal, this is the mean dispersal distance, 
#'                       in cells (not log transformed).\cr
#' disppar2         \tab The second input parameter for the dispersal function.  
#'                       For log-normal, this is the dispersal standard 
#'                       deviation in cells (log-transformed).\cr
#' }
#' @name sp.instr.file
NULL

#' Species Response Traits Instructions File
#'
#' Input file giving instructions for generation of species response traits.
#' Each row should corre-spond to a response trait.  Either this file and the\
#' Species Instructions File \link[spatialdemography]{sp.instr.file} OR the
#' Species File \link[spatialdemography]{sp.file} are required.
#'
#' @details \tabular{lll}{
#' Label                \tab L.p23  \tab This field consists of two parts, 
#'                                       separated by a period. The first part is 
#'                                       the landscape.identifier for the 
#'                                       landscape layer of interest, the second 
#'                                       part is the vital rate that should be 
#'                                       modified.  Duplicate labels are allowed 
#'                                       that code for dif-ferent response traits. \cr
#' DistributionFunction \tab 10     \tab A numeric indicator that codes for a 
#'                                       function for how the response trait 
#'                                       should be distributed among species. 
#'                                       See \link[spatialdemography]{distribution.functions} 
#'                                       for more details. \cr
#' distfunpar1          \tab 0.5    \tab The first parameter for the distribution 
#'                                       function. See 
#'                                       \link[spatialdemography]{distribution.functions} 
#'                                       for more details. \cr
#' distfunpar2          \tab 2.5    \tab The second parameter for the 
#'                                       distribution function. 
#'                                       See \link[spatialdemography]{distribution.functions} 
#'                                       for more details. \cr
#' ResponseFunction     \tab 100    \tab A numeric indicator that codes for how 
#'                                       the response trait will function for 
#'                                       each species. See 
#'                                       \link[spatialdemography]{response.functions} 
#'                                       for more details. \cr
#' respfunpar1          \tab 1      \tab The first parameter for the response 
#'                                       function. See 
#'                                       \link[spatialdemography]{response.functions} 
#'                                       for more details.\cr
#' respfunpar2          \tab 0.64   \tab The second parameter for the response 
#'                                       function. See 
#'                                       \link[spatialdemography]{response.functions} 
#'                                       for more details.\cr
#' }
#' @name sp.resp.instr.file
NULL

#' Species File
#'
#' Input file giving the vital rates and response traits for each species.
#' Either this file OR both the Species Instructions File
#' \link[spatialdemography]{sp.instr.file} and the Species Response 
#' Instructions file \link[spatialdemography]{sp.resp.instr.file} are required.
#' 
#' @details \tabular{ll}{
#' Input              \tab Description \cr
#' sp                 \tab A numeric indicator for species \cr
#' p01                \tab Transition rate from stage 0 to stage 1.\cr
#' p02                \tab Transition rate from stage 0 to stage 2.\cr
#' p11                \tab Survival rate of stage 1 (proportion of stage 1 
#'                         individuals that remains in stage 1).\cr
#' p12                \tab Transition rate from stage 1 to stage 2.\cr
#' p22                \tab Survival rate of stage 2 (proportion of stage 2 
#'                         individuals that remains in stage 2).\cr
#' p23                \tab Transition rate from stage 2 to stage 3.\cr
#' p30                \tab Number of stage 0 individuals produced by stage 3.\cr
#' p32                \tab Number of stage 2 individuals produced by stage 3.\cr
#' p33                \tab Survival rate of stage 3 (proportion of stage 3 
#'                         individuals that remain in stage 3).\cr
#' biomass.adult      \tab The species' adult biomass (g).\cr
#' biomass.juv        \tab The species juvenile biomass (g), only needed if 
#'                         assigning initial numbers by biomass. \cr
#' biomass.seed       \tab The species' seed mass (g), only needed if assigning 
#'                         initial numbers by biomass. \cr
#' dispersalfunction  \tab A number indicating which dispersal function to use.  
#'                         Log-normal is indi-cated by 1.\cr
#' disppar1           \tab The first input parameter for the dispersal function.  
#'                         For log-normal, this is the mean dispersal distance, 
#'                         in cells (not log transformed).\cr
#' disppar2           \tab The second input parameter for the dispersal function.  
#'                         For log-normal, this is the dispersal standard 
#'                         deviation in cells (log-transformed).\cr
#' <Response Traits>  \tab Zero or more response traits describing how the 
#'                         species will respond to environmental layers. Each 
#'                         entry is delimited by ';', and has four components: 
#'                         the 'optimal' value, a code for the function for how 
#'                         the vital rate will change relative to the optimal 
#'                         value, and two parameters for the function.  
#' }
#' @name sp.file
#' @aliases SpTraits spfile species.file dispersal
NULL

#' Landscape Files
#'
#' The landscape files are optional, and only are needed if a specific landscape
#' configuration is desired.  In this case, there should be a file for each
#' environmental layer in the landscape (with each file name matching an env.lbl
#' field  in the Environmental_layers file plus an _0.csv, e.g., for landcover,
#' the file name would be landcover_0.csv).  Within each file the values for the
#' environmental layer should be present in comma-separated format, with the
#' number of rows equaling the number of columns equaling the landscape extent
#' specified in the Initial_conditions file.  There should be no column or row
#' headers.
#'
#' During the model run, if there is environmental change, new landscapes can be
#' generated.  These will have a number appended after the file name, and the 
#' numbers will be linked to correpsonding timesteps in the 
#' \link[spatialdemography]{change.lookup}.
#'
#' @name landscape.files
NULL

#' The landscape object
#' 
#' An alternative to inputing a landscape directory is to construct the
#' landscape in R. To do this, there must be a list, containing a vector for
#' each environmental variable (in the same order as they are listed in the 
#' environmental layers file, see \link[spatialdemography]{env.file}). The 
#' vector should contain the values for each cell in a square landscape, given
#' by rows (i.e. starting at the top left and going to top right, then down
#' to next row, and so forth, until the bottom right corner is reached). Note
#' that the data are given in vector format, not as a square matrix.
#' 
#' @name landscape.object
NULL

#' Initial Species Locations File
#'
#' An Initial Species Locations File is also optional, and should only be
#' included if particular species starting locations are desired.
#' There are two formats for this file. In the old format, there should be one
#' row for each cell in the landscape.  In each row, the species that should be
#' present in the corresponding cell should be listed, separated by commas
#' All species will be initialized with the same number of adults as specified
#' in the num.adults field in the Initial Conditions File
#' \link[spatialdemography]{initial.conditions.file}). The file should not 
#' include header information. In the new format, the file should be in the 
#' format of the SpeciesData.csv file generated by the model. The header should 
#' include (in this order): LifeStage, Species, TimeStep, Cells (one column for 
#' each cell in the landscape) Only locations from a single (user-specified) 
#' timestep will be extracted.
#'
#' @name locations.file
NULL

#' Species Parameters File
#'
#' The file contains three headings: Trait, Min, and Max.  Trait lists traits,
#' while min and max give minimum and maximum values to sample from.  When a 
#' fixed value is desired, min and max should be equal.
#' See details for description of traits that should be included.
#'
#' @details \itemize{
#' \item seed.disp.ratio The proportion of seeds dispersing out of the cell. 
#'       %% This parameter is misnamed, should be proportion dispersing.
#' \item adult.longevity Adult survival %% This parameter is also misnamed.
#' \item g Proportion of reproduction allocated to clonal reproduction
#' \item w Probability of a seed germinating
#' \item biomass.seed Biomass of a seed
#' \item biomass.adult Biomass of an adult
#' \item biomass.clone Biomass of a clone
#' }
#'
#' @name sp.param.file
NULL

#' Environmental Parameters File
#'
#' The file contains parameter entries and minimum and maximum values for each 
#' parameter. The minimum and maximum values are used for latin hypercube sampling.  
#' If a constant value is desired, minimum value should equal maximum value
#' while min and max give minimum and maximum values to sample from.
#' See details for a description of parameters to include.
#'
#' @details \itemize{
#' \item k2 A parameter used in the Weibull function describing the relationship 
#'       between adult longevity and allocation to reproduction
#'       %% Jakob often referred to this as the 'Cost of Iteroparity'.
#' \item seed.mortality Seed mortality
#' \item Ss.mean Mean stochastic seedling survival (fixed across all species).  
#'       Stochastic seedling survival is determined by a beta distribution 
#'       (but this is not the Alpha parameter!)
#' \item Ss.var Variation in stochastic seedling survival (fixed across all species)
#' \item fertility.mean Mean fertility rate (fixed across all species).  
#'       Fertility is determined by a log-normal distribution
#' \item fertility.var Variation (sd) in fertility rate (fixed across all species)
#' \item Ss.fert.cor The correlation between variation in fertility and 
#'       stochastic seedling survival.
#' }
#'
#' @name env.param.file
NULL

#' env.type input
#'
#' env.type determines how each landscape element will be set up.
#'
#' @details Any of the distribution functions (see 
#'          \link[spatialdemography]{distribution.functions} will work here. 
#'          Additionally, the following options are available:
#' \itemize{
#' \item cover specifying cover will set up the element based on percent cover, 
#'       with required parameters being specified in the cover.levels and param1 
#'       fields. The field cover.levels specifies the cover levels to be 
#'       generated, each separated by a semicolon (no semicolon is needed if 
#'       only one cover class is present).  This should be the proportion of 
#'       cells in each cover type (e.g., 1 would give one cover type with 100% 
#'       cover, 0.5;0.5 would give two cover types each with 50% cover, and 0.5;
#'       0.25;0.25 would give 3 cover types the first with 50% cover, the second 
#'       with 25% cover, and the third with 25% cover.  The first number 
#'       assigned is determined by param1, and numbers follow sequentially and 
#'       increasingly from there.  (e.g., if param1 = 1, then the third example 
#'       above would generate cover levels of 1,2,3). The param2 field should be 
#'       set to NA. 
#' \item rnorm will set the landscape element up based on values drawn from a 
#'       normal distribution where the mean is given by the value of param1 and 
#'       the standard deviation by the value of param2. The cover.levels field 
#'       should be set to NA.
#' }
#' @name env.type
NULL

# DD# A few more words in here may be desirable
#' env.change.type & env.change.mag inputs
#'
#' env.change type specifies how the environment changes, while env.change.mag 
#' determines the magnitude of the change. The field env.change.mag often 
#' contains multiple pieces of information. Main components are separated by a 
#' semi-colon, sub-components are separated by a colon.
#'
#' @details
#' \itemize{ %% Begin first list
#' \item For Non-conditional Landscape Elements (i.e. environmental layers where 
#'       cond.var is set to NA) \cr
#' \itemize{ %% Begin sub-list for non-conditional landscape elements
#'  \item swap: this will randomize the locations of existing values in the 
#'       landscape element. env.change.mag should be 0 or NA for this env.change.type. 
#'  \item markov: this will change each value according to a Markov process, 
#'       based on traisition probabilities given in env.change.mag
#'  \itemize{ %% Begin third sub-list (for markov option)
#'    \item env.change.mag needs to have transition probabilities for each possible 
#'       value in the environmental layer
#'    \item The possible values are given first, followed by transitions for first 
#'       layer value (to first, to second, etc), then transitions for the 
#'       second layer value (to first, to second, etc)
#'    \item Transitions should be cumulative probabilities
#'    \item For example: 0:1;0.681:1:0.275:1
#'    \itemize{ %% Begin forth sub-list: example options
#'      \item 0:1 correspond to two possible values in the landscape, 0 and 1.
#'      \item 0.681 corresponds to the probability of going from 0 to 0.  The 1 
#'       indicates the cumulative transition probability (and will always be 1 
#'       for the last value). The probability of transitioning from 0 to 1 is 
#'       0.319 in this example (1 - 0.681, the preceding value. The probability 
#'       of transitioning from 1 to 0 is 0.275, and the probability of 
#'       transitioning from 1 to 1 is 0.725 (1 - 0.275).
#'    } %% End forth sublist
#'  } %% For markov option (third sublist)
#'  \item markov.changing.transitions: The same as markov above, except with an 
#'        extra part of env.change.mag describing how the transition rates 
#'        themselves change with time. 
#'  \itemize{ %% Begin fifth sublist
#'    \item For example 0:1;0.681:1:0.275:1;-0.05:0:-0.025:0 
#'    \item The last section -0.05:0:-0.025:0 corresponds to:
#'    \item -0.05 this decreases the probability of transitioning from 0 to 0.
#'    \item 0 This leaves the end point at 1. With the decrease in transition 
#'          probability for 0 to 0, this results in an increase in transition 
#'          probability of 0 to 1.
#'    \item -0.025 leads to a decrease in the transition probability of going 
#'          from 1 to 0
#'    \item 0 Keeps the cumulative probability distribution at 1, and 
#'          consequently will lead to increasing transition probabilities from 
#'          1 to 1.
#'  } %% end fifth sublist (markov.changing.transitions descriptions)
#'  \item cover.change.and.swap: (needs testing) this changes the initial cover 
#'        levels, then reassigns cover values to the landscape randomly based on 
#'        the new cover levels.  In this case, env.change.mag corresponds to the 
#'        incremental changes to overall cover levels.  These changes must 
#'        balance one another.  For example, if initial cover levels were 0.5,
#'        0.25,0.25, and the value of env.change.mag was 0.25;-0.125;-0.125, at 
#'        each change step, the overall cover level after the first change would 
#'        be 0.75,0.125,0.125. Note that changes that result in cover levels 
#'        that cannot be assigned to the landscape (e.g., in a landscape of 4 
#'        cells, you could not have probabilities other than 0,.25,.5,.75, and 1). 
#'        Also note that a change that would reduce a cover level below 0 or 
#'        above 1 will result in a warning and no environmental change. 
#'  \item rnorm.prob: (needs testing) each value of a landscape element will 
#'        change with a specified probability.  If it changes, the mean will be 
#'        adjusted by a set value, and a new value will be drawn from a random 
#'        normal distribution with the new mean and a specified standard deviation.  
#'        In this case, env.change.mag needs to contain: 
#'  \itemize{ %% Begin sixth sublist
#'    \item 1. the probability the cell will change
#'    \item 2. the change in mean for the new value
#'    \item 3. the overall standard deviation for drawing a new value
#'    \item 4. Example: 0.5;0.25;1
#'    \item 5. The first number gives the probability a cell value will change.  
#'          R will draw a random number, if the random number is less than or 
#'          equal to this value, the cell will change.
#'          The second number is added to the current cell value (e.g., if the 
#'          cell value had been 20, the new mean would be 20.25. A new value for 
#'          the cell would then be drawn from a normal distribution with a mean 
#'          of 20.25 and a standard deviation of 1.
#'  } %% end sixth sublist (rnorm prob options)
#'  \item f. landscape.wide.change: A change is applied to the entire landscape, 
#'        but the difference in values between cells remains the same.  
#'        In this case, env.change.mag contains four elements:
#'  \itemize{ %% begin 7th list, landscape.wide.change options
#'    \item i. the overall mean for the landscape.  This will be changed each 
#'          time there is an environmental change in this layer
#'    \item ii. the overall standard deviation for the landscape.  
#'          This will change each time there is an environmental change.
#'    \item iii.The change in overall mean.
#'    \item iv.The change in overall standard deviation.
#'    \item Explanation: First, the overall mean and standard deviation in 
#'          env.change.mag are updated based on elements iii. and iv.  
#'          Second, a change is drawn from a normal distribution the overall 
#'          standard deviation (ii) (with a mean of 0).  
#'          Next, residuals for each cell are calculated by subtracting each 
#'          cells value from the old landscape mean.  
#'          Finally, new cell values are created by taking the overall landscape 
#'          mean, adding in the cell specific residuals, and adding the random 
#'          change element.
#'  } %% End seventh sublist
#'  \item logn Change according to a log-normal distribution. Mean and variance 
#'        as for rnorm.prob.
#'  \item beta Change according to a beta distribution value with mean and 
#'        variance specified in c.mag (mean != alpha)
#'  \item fx Change using one of the distribution functions. See 
#'        \link[spatialdemography]{distribution.functions} for more details.
#'  \item from.file Change is read in from the landscape file based on the new 
#'        change.count 
#' } %% End second sublist, for non-conditional landscape elements
#' \item For conditional landscape elements (cond.var is not set to NA):
#' \itemize{ %% begin eight sublist - conditional landscape elements
#'  \item swap: (only valid for env.type == 'cover', will not work for 
#'        env.type == 'rnorm'):  this will randomize the locations of existing 
#'        values in the landscape elements, with the condition that values 
#'        cannot be assigned to cells determined to be inappropriate based on 
#'        another landscape element.  E.g. this can prevent mowing from 
#'        occurring in forests, even when patterns of mowing or of forests 
#'        changes (however if the proportion of the conditioning layer (e.g., 
#'        forest) changes, this will not work properly (because there will be 
#'        more or fewer suitable habitat patches than there are values to assign).  
#'        It only works if the conditional layer does not change, or changes via swap.  
#'        env.change.mag should be set to 0 or to NA for this setting. 
#'  \item markov: b.markov: See above under non-conditional landscape elements.  
#'        The markov transitions are only applied to cells that are suitable 
#'        based on the other habitat type, non-suitable cells are automatically 
#'        zero.
#'  \item markov.changing.transitions: c.markov.changing.transition: See above 
#'        under non-conditional landscape elements. The markov transitions are 
#'        only applied to cells that are suitable based on the other habitat type, 
#'        non-suitable cells are automatically zero.
#'  \item from.file Change is read in from the landscape file based on the new 
#'        change.count 
#'  } %% end eight sublist (conditional landscape elements) 
#' } %% End overall list
#' @name env.change.type
#' @aliases env.change.mag
NULL

# DD# Update this documentation - I think it is out of date
#' Distribution and Response Functions
#'
#' Distribution functions govern the creation (e.g., of species, of dispersal
#' distances, or environmental layer values), while response functions define
#' how species respond to environmental conditions.  This is accomplished
#' through creation of a modifier (constrained to be between 0 and 1)
#'
#' @details Distribution functions are given in the following table:
#' \tabular{lll}{
#' Function Code  \tab Function   \tab Description \cr
#' 1              \tab log-normal \tab Draw values from a log-normal 
#'                                     distribution; par1 specifies the mean and 
#'                                     par2 specifies the SD (mean & SD in 
#'                                     log-space, but see note below) \cr
#' 2              \tab normal     \tab Draw values from a normal distribution; 
#'                                     par1 specifies the mean and par2 
#'                                     specifies the SD \cr
#' 3              \tab constant   \tab Only enter a constant value. Only par1 is 
#'                                     used. \cr
#' 4              \tab uniform    \tab Draw values from a uniform distribution.  
#'                                     The lower bound is given by par1 and the 
#'                                     upper by par2 \cr
#' 5              \tab uniform, rounded to nearest integer \tab Draw values from 
#'                                     a uniform distribution, but round to the 
#'                                     nearest integer. The lower bound is given 
#'                                     by par1 and the upper by par2. Both 
#'                                     bounds must be integers. \cr
#' 'cover'        \tab cover      \tab Values are assigned relative to 
#'                                     proporition of cover, with par1 
#'                                     specifying cover proportions. Only can be 
#'                                     used for environmental layer setup. \cr
#' }
#' Response functions are given here:
#' \tabular{lll}{
#' Function Code \tab Function \tab Description \cr
#' 101  \tab matching               \tab if the value of an environmental layer 
#'                                       in a given cell matches the species' 
#'                                       target value, then the appropriate 
#'                                       vital rate is multiplied by param1.  
#'                                       If the value does not match, then the 
#'                                       appropriate vital rate is multiplied by 
#'                                       param2. \cr
#' 102  \tab quadratic, decreasing  \tab -par1 * (env.val - target.val)^2 + 1 
#'                                       Where env.val is the value of the 
#'                                       environmental layer in a given cell, 
#'                                       and target.val is the species' target 
#'                                       value. \cr
#' 103  \tab environmental          \tab Take modifier value directly from 
#'                                       cell's environmental layer
#'                                       Note that these modifiers are not 
#'                                       constrained to be between 0 and 1, and 
#'                                       choosing biologically 
#'                                       plausible/realistic/possible values is 
#'                                       left to the investigator \cr
#' 104  \tab seed fertility         \tab Get seed number based on the approach 
#'                                       in the Stochastic Plants paper
#'                                       opt.val = value specified as base vr 
#'                                       for clone production (allows 
#'                                       calculation of the modifier)
#'                                       par1 = biomass.adult:biomass.seed:biomass.clone
#'                                       par2 = relative.allocation.reproduction (a):ratio.sex.allocation (g) TODO (sten): this is hard to understand intuitively 
#'                                       q = biomass.adult * a * fertility   
#'                                       #fertility is given by the environmental 
#'                                       layer and is in grams
#'                                       num.seeds = q * (1 - g) / biomass.seeds 
#'                                       (For funct == 105, num.clones = q * g / biomass.clones)
#'                                       modifier = num.seeds / opt.val 
#'                                       #This will later be multiplied by 
#'                                       opt.val, allowing that part to cancel 
#'                                       out leaving the new seed number. \cr
#' 105  \tab clone fertility        \tab Get clone number based on the approach 
#'                                       in the Stochastic Plants paper.  
#'                                       See details in function 104. \cr
#' 106  \tab threshold              \tab Apply lower and upper thresholds.
#'                                       Format is "NA;106;LT;UT" If environmental value is
#'                                       lower than the lower threshold (LT) or higher than the
#'                                       upper threshold (UT), the vital rate is set to 0. Otherwise
#'                                       it is unaffected. No optimal value is needed.
#' }
#' @name DFRF
#' @aliases distribution.functions response.functions
NULL

#' Results Outputs
#'
#' File outputs generated by the model (including some optional outputs
#' by requesting visual debugger data or matrix diagnostics.  
#'
#' @details Table summarizes how many files are produced
#' per model run (MR), scenario (SC), timestep (TS), cell (CL), species (SP),
#' environmental layer (EL), environmental conditions (EC). \cr
#' The file  production is multiplicative
#' (e.g., 100 timesteps x 2 species = 200 files) 
#' \tabular{lll}{
#' File type \tab Number of Files created \tab Link to File description \cr
#' Seeds in landscape \tab MR, SC, TS, SP \tab \link[spatialdemography]{stage.abundances} \cr
#' Juveniles in landscape \tab  MR, SC, TS, SP \tab \link[spatialdemography]{stage.abundances} \cr
#' Adults in landscape \tab  MR, SC, TS, SP \tab \link[spatialdemography]{stage.abundances} \cr
#' Local transition matrices \tab MR, SC, CL, SP, EC \tab \link[spatialdemography]{local.transition.matrices} \cr
#' Landscape-wide matrices \tab  MR, SC, SP, EC \tab \link[spatialdemography]{landscape.wide.matrix} \cr 
#' Local matrix summary info \tab MR, SC, SP, EC \tab \link[spatialdemography]{local.matrix.summary.info} \cr
#' Overall matrix diagnostics \tab MR, SC, EC \tab \link[spatialdemography]{overall.matrix.diagnostics} \cr
#' Landscape files \tab MR*, SC*, EL, EC; *can be reused \tab \link[spatialdemography]{landscape.files} \cr
#' Change lookup \tab MR, SC \tab \link[spatialdemography]{change.lookup} \cr
#' Cell occupancy \tab MR, SC \tab \link[spatialdemography]{cell.occupancy} \cr
#' Species statistics seeds \tab MR, SC \tab \link[spatialdemography]{species.statistics} \cr
#' Species statistics juveniles \tab MR, SC \tab \link[spatialdemography]{species.statistics} \cr
#' Species statistics adults \tab MR, SC \tab \link[spatialdemography]{species.statistics} \cr
#' Scenario Timing \tab MR, SC \tab \link[spatialdemography]{scenario.timing} \cr
#' Model Results \tab MR \tab \link[spatialdemography]{results.file} \cr
#' }
#' @name Results.Outputs
NULL

#' Stage Abundances
#'
#' These files are in a folder labeled *_matrices, where * corresponds to the 
#' lifestage of interest.  This gives the abundance of the lifestage of interest
#' in every cell of the landscape.  There is one file per species per timestep,
#' for each scenario. \cr
#' File names are Stage_species_timestep.csv
#'
#' @name stage.abundances
#' @aliases seed.abundances juvenile.abundances adult.abundances
NULL

#' Local Transition Matrices
#'
#' These files give the local transition matrices for each species for each cell
#' in the landscape for each set of environmental conditions.  See Appendix S1
#' for more details.
#'
#' @name local.transition.matrices
NULL

#' Landscape-wide Matrix
#'
#' This provides the landscape-wide transition matrix.
#' See Appendix S1 for more details.
#'
#' @name landscape.wide.matrix
NULL

#' Local Matrix Summary Info
#'
#' Found in Diagnostics/Sp*/ folder, where * corresponds to the species of 
#' interest. For each cell, this file gives the Dominant eigenvalue from the 
#' local transition matrix, the per capita seeds the cell exports, and the per 
#' capita seeds the cell imports. Per capita imports is misleading, as different 
#' cells that contribute to this cell may have different population sizes, and 
#' this is a sum across all cells. Finally, the proportion of the seeds produced 
#' that are exported is given. There is a local matrix summary info for each 
#' species, and for each environmental change.
#'
#' @name local.matrix.summary.info
NULL

#' Overall Matrix Diagnostics
#'
#' This summary file provides the landscape-wide growth rate, irreducibility,
#' ergodicity, maximum, median, and minimum local growth rates, maximum, minimum,
#' and median seed imports and exports, and the proportion of seeds dispersing.
#' (the model says max, min and median, but as dispersal is a species trait, it
#' the proportion dispersing will not change). \cr
#' Each row corresponds to a species.
#'
#' @name overall.matrix.diagnostics
NULL

#' Change Lookup
#'
#' One lookup file is created, which tells spatialdemography which landscape 
#' files correspond to which timesteps.
#'
#' @name change.lookup
NULL

#' Cell Occupancy
#'
#' The number of cells occupied at each time step.
#' Rows correspond to time steps, while columns indicate species.
#'
#' @name cell.occupancy
NULL

#' Species Statistics
#'
#' Summary statistics by time step for each species in the landscape.
#' Time steps correspond to rows, and species correspond to columns (this is
#' somewhat non-intuitive and confusing in the file)
#'
#' @name species.statistics
NULL

#' Scenario Timing
#'
#' The Scenario timing file gives the time and overall elapsed time for 
#' different model processes, as well as an overall model time.  The purpose of
#' this file is to know how long a model run took, and to identify rate-limiting
#' processes that can either be reimplemented to be faster in R, or implemented
#' in C or C++
#'
#' @name scenario.timing
NULL

#' Results File
#'
#' The results file contains a row for each scenario, with the scenario, scenario
#' number, and the last timestep completed (in case the model crashes). \cr
#' The model provides initial, final, and change for species richness Sp.Rich,
#' (multiplicative) beta diversity (Beta.Div), biomass.adult, functional trait 
#' diversity (FTD.UTC, calculated with the unscaled multivariate richness index), 
#' and response trait diversity (also calculated with the unscaled multivariate 
#' richness index. (at present these are fixed to these specific options and are 
#' calculated at the landscape scale. In future versions these may be 
#' user-specified. \cr
#'
#' @name results.file
#' @aliases model.results
NULL

#' Calculated Metrics
#'
#' These are metrics that can be calculated by spatialdemogrphay as the primary
#' model results.
#' %% These options are not yet available:
#' %% 'FTD.PGFD' \tab Functional trait diversity calculated with Petchey & Gaston's FD \cr
#' %% 'RTD.PGFD' \tab Response trait diversity calculated with Petchey & Gaston's FD \cr
#'
#' @details Options are given in the below table:
#' \tabular{ll}{
#' 'Sp.Rich'  \tab Species Richness \cr
#' 'Beta.Div' \tab Multiplicative Beta Diversity \cr
#' 'Biomass'  \tab Total Biomass \cr
#' 'FTD.UTC'  \tab Functional trait richness calculated with Unique Trait 
#'                 Combinations \cr
#' 'RTD.UTC'  \tab Response trait richness calculated with Unique Trait 
#'                 Combinations \cr
#' 'FTD.FRIC' \tab Functional trait richness calculated with the convex hull\cr
#' 'RTD.FRIC' \tab Response trait richness calculated with the convex hull\cr
#' }
#'
#' @name out.metrics
NULL

#' Setting the Scale of output results
#'
#' SpatialDemography has the ability to examine model results at multiple 
#' spatial scales. The default settings scale.vec = c('landscape') and 
#' scale.cells.lst = 'default' examine model results at the scale of the entire 
#' landscape. The entries in scale.vec are just labels, they do not actually 
#' serve any functional purpose. scale.cells.lst actually determines which cells 
#' are evaluated, in the default condition, a vector of all cells is included. 
#' This vector can just select specific cells (i.e. a value of 1 for cells to be 
#' evaluated and 0 for cells not to be evaluated. In addition, a new option has 
#' been added where only cells of a particular environmental layer value can be 
#' evaluated. In this case, scale.cells.lst should be set to 'subset;X;Y' where 
#' X stands for a landscape identifier, and Y stands for the value of that 
#' environmental layer to be considered. For example, 'subset;L;1' would look 
#' for the environmental layer with landscape.identifier of L (typically 
#' landcover or land use for my simulations), and would only calculate
#' output results for a value of 1 (typically grassland cells in my simulations).
#' Unfortunately this cannot be used with evaluations of multiple scales.
#' But that limitation may change in the future.
#'
#' @name scale.vec
#' @aliases scale.cells.lst
NULL 
