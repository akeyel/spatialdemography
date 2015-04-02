#This file contains the main simulation function for SpatialDemography.
#It is in its own r script for ease of editing and for clarity.
#See spatialdemography package for license info and more details.

#' Function to run the simulation portion of SpatialDemography
#' 
#' This function does the model setup, matrix diagonstics, and simulation portion of spatialdemography. 
#' %DD% ADD INPUT DESCRIPTIONS %%s.lbl is only really required for reading in changed landscape values from file.
#' %% disp.pdf,eigen.pdf,rtd.c
#'
#' @param Model.Name The name of the model run
#' @param ResultsFile The file that results will be written to.
#' @param vdb.data An indicator for whether or not to create visual debugger data
#' @param timefile Timing results are written to this file.
#' @param write.timefile An indicator for whether the time file should be output.
#' @param run.times A list of intermediate times during the model run
#' @param run.lbl A list of labels for each of the intermediate times
#' @param start.time The starting time for the model
#' @param run.path The path for the model run
#' @param DispPath The path for the dispersal tables
#' @param outpath.base A base path for outputs
#' @param num.sim The number of simulations to run
#' @param S The number of life stages in the model (MUST BE 4)
#' @param extent The length of one side of the square landscape
#' @param p The number of cells in the landscape (extent ^ 2)
#' @param landscape A list object containing the environmental layer values for each cell in the landscape
#' @param landscape.identifiers A list of single letter identifiers for each environmental layer
#' @param distances The distances between every pair of cells
#' @param settings A dataframe containing the information from the settings file
#' @param ic A dataframe containing the information from the initial conditions file
#' @param rtd.c An indicator for how to handle response trait diversity. Currently non-functional, use the hard-wired default
#' @param locations.file A file indicating species locations. May not be applicable in many circumstances.
#' @param landscape.dir A directory pointing to where the landscape files should be written.
#' @param SpTraits Species trait data read in from the species file
#' @param tot.sp.num The number of species in the species pool (=spe)
#' @param my.env Information about the environmental layers
#' @param env.c.freq The frequency of environmental change
#' @param is.change An indicator for whether or not change will occur in the model (for computational efficiency)
#' @param change.count An indicator for the current environmental change step.
#' @param env.lbl A vector of labels containing the landscape layer names
#' @param s.lbl Another label
#' @param lnd.lbl Another label
#' @param competitiontype An indicator for what type of competition should be implemented
#' @param microsites The number of microsites in the model
#' @param Results A dataframe to hold the results from the model
#' @param out.metrics A list of metrics to be calculated for the model
#' @param scale.vec A list of spatial scales at which to evaluate the model
#' @param timepoint.vec A list of timepoints at which to calculate the metrics given in out.metrics
#' @param scale.cells.lst A list of cells corresponding to each spatial scale
#' @param resolution The resolution to use for rounding (for calculating UTC values)
#' @param log.trans Whether or not to log-transform the data. Currently not recommended, as this functionality is untested in multirich package
#' @param MaxTime The maximum number of timesteps to run the model for.
#' @param invasion How frequently invasion occurs
#' @param num.invaders The number of species to have invade
#' @param cells.to.invade The number of cells each species will invade
#' @param repro.proportion The proportion of the species reproductive potential with which to invade
#' @param K_g The carrying capacity in grams
#' @param multi.species.K An indicator for whether or not to include a multispecies carrying capacity
#' @param edge.type The edge type. Currently only Absorbing and Torus are supported
#' @param do.simulation An indicator for whether or not to run the simulation
#' @param do.diagnostics An indicator for whether or not to compute diagnostics
#' @param testing An indicator for whether or not a testing run is being conducted
#' @author Alexander "Sasha" Keyel & Jakob Gerstenlauer
Simulation<-function(Model.Name,
                      ResultsFile,
                      vdb.data,timefile,write.timefile,
                      run.times,run.lbl,start.time,
                      run.path, DispPath,outpath.base,num.sim,
                      S,extent,p,landscape,landscape.identifiers,distances,
                      settings, ic,rtd.c,locations.file,landscape.dir,
                      SpTraits,tot.sp.num,
                      my.env,env.c.freq,is.change,
                      change.count,env.lbl,
                      s.lbl, lnd.lbl,
                      competitiontype,microsites,
                      Results,out.metrics,scale.vec,timepoint.vec,scale.cells.lst,
                      resolution,log.trans,
                      MaxTime,
                      invasion, num.invaders, cells.to.invade,repro.proportion,
                      K_g, multi.species.K,
                      edge.type,
                      do.simulation,do.diagnostics, testing){ #,do.eigen.maps,do.disp.maps

    #shorten total species number to species end to spe
    spe = tot.sp.num

    #Create the appropriate vec-permutation matrix.  #Compare Hunter and Caswell 2005, http://www.montana.edu/rotella/501/HC2005.pdf #Could be moved out of simulation function to speed things up (i.e., only needs to be done once)
    cpm.out = CreatePermutationMatrix(p,S)
      M_sub = cpm.out[[1]]
      P = cpm.out[[2]]
      
    #run.times = c(run.times,gettime());run.lbl = c(run.lbl,"Permuation.Matrix.made")
    
    #Set up demography matrix templates; create a sparse vector for the composite demography matrix with p*3 times p*3 elements #Could be moved out of simulation function to speed things up (i.e. only needs doing once.)
    Dim<-S*S*p^2
    B1.template = sparseMatrix(dims = c(1,Dim), i={}, j={},x=rep(0.0,Dim))
    B2.template = B1.template
    dim(B2.template)<-rep(S*p,2)#Reformat for allowing the diagonal to be assigned
    diag(B2.template) = 1 #Set the diagonal to 1.  Will need to replace the 1st of every S elements on the diagonal with 0.
    dim(B2.template) = c(1,Dim) #Put back as a 1 row matrix to maintain same format as B1.template
    
    run.times = c(run.times,gettime()); run.lbl = c(run.lbl,"Preliminary setup completed")
    
    #set up matrices for simulation model.  If environmental change is set to change every time step, this setup will be undone   
    mat.info = setup.matrices(K_g,spe,SpTraits,p,S,landscape,landscape.identifiers,distances,B1.template,B2.template,P,M_sub,DispPath,vdb.data,change.count,outpath.base,num.sim,run.times,run.lbl,multi.species.K, edge.type)
      #Unpack mat.info for each species.
      if (multi.species.K == 0){
          #Append results to list if each species has a unique K.
          K.lst = mat.info[[1]]
          }
      #else: doesn't matter - a multi-species K will be applied later on.
      B1.lst = mat.info[[2]]
      B2.lst = mat.info[[3]]
      DispersalProbabilities.lst = mat.info[[4]]
      SeedDispersalRatio.lst = mat.info[[5]]
      M.lst = mat.info[[6]]
      run.times = mat.info[[7]]
      run.lbl = mat.info[[8]]
    
      run.times = c(run.times,gettime()); run.lbl = c(run.lbl,"setup.matrices completed")    
    
    #Create map displaying initial dispersal probabilities
    #if (do.disp.maps == 1){
    #    #outpath.base
    #    #NOT YET SCRIPTED
    #    }
    
    #Run basic matrix diagnostics & write results to file
    if (do.diagnostics == 1){            
        Gave.Warn = 0 #indicator for whether a warning was issued. If so, the warning will not be re-issued.
        cd.out = compute.diagnostics(spe,B1.lst,B2.lst,M.lst,P,S,p,outpath.base,vdb.data,change.count,run.times,run.lbl,1,Gave.Warn) #Last 1 indicates this is the first time for creating diagnostics - need to set up files.
          run.times = cd.out[[1]]
          run.lbl = cd.out[[2]]
          Gave.Warn = cd.out[[3]]
        }

    #Create map displaying initial eigen values
    #if (do.eigen.maps == 1){
    #    #outpath.base - needed for writing file outputs
    #    #Not Yet Scripted
    #    }
    
        
    ##Time first two species matrix assignments (first may be slower due to generation of dispersal probabilities)
    #if (calc.times == 1){
    #    run.times = c(run.times,gettime()); run.lbl = c(run.lbl,sprintf("%s species matrix assigned",sp))              
    #    }
    #
    #    } #end species loop, turn off when troubleshooting
    
    run.times = c(run.times,gettime()); run.lbl = c(run.lbl,"All species matrices assigned")

    #******# Begin Simulation #******#
    if (do.simulation == 1){
    
        run.times = c(run.times,gettime()); run.lbl = c(run.lbl,"Simulation began")

        #determine indices for three stages
        index<-1:(S*p)                  #This is a vector
        IAdults    <-  index %% S == 0  #This is a vector, with Trues where index divided by number of stages has remainder 0 and falses everywhere else
        ISeedlings <-  index %% S == 1
        ISeedBank  <-  index %% S == 2
        IJuveniles <-  index %% S == 3
        #use:  n_Seedlings <- n_t1[Seedlings]
        
        #Create a vector for micro-sites for each cell for competition scenarios
        if (competitiontype != 0){
            #check that microsites is not set to NA
            if (is.na(microsites)){
                stop("Number of microsites must be specified for this competition type")
                }
            
            microsites.vec = rep(microsites,p)
            }
                
        #Set variables for initialization of other variables                                                                                                               
        n_0<- rep(0, S*p) #NOTE: THIS IS MIS-NAMED - n_0 only ever contains 0's in the current implementation of the code
        
        #Set up vectors for the life stages & outputs
        life.stage.vector <-vector(MaxTime,mode="numeric")
        #OutputTemplate <-array(data=rep(0.0, Replications * MaxTime), dim=c(Replications,MaxTime))
        OutputTemplate = array(data = rep(0.0,MaxTime),dim = c(1,MaxTime))
        
        #Set up lists
        n0.lst = rep(list(n_0),spe)   #List for initial population vectors
        nt1a.lst = rep(list(n_0),spe) #List for intermediate time step in matrix model
        nt1.lst = rep(list(n_0),spe)  #List for population vector at time t + 1
        lifestage.lst = rep(list(life.stage.vector),spe) #Create a list to use as a template for the life stage summaries
        OutputTemplate.lst = rep(list(OutputTemplate),spe) #Create a list to use as a template for some of the outputs
    
        #Reset seed to what it would have been in previous version of code
        if (exists(".Random.seed")){
            .Random.seed <<- load.seed(Model.Name, 1, testing) #Only loads a seed if testing == T  #set.seed(111)       
            }
    
        #Initialize species in each cell
        #n0.lst = OccSetup(n0.lst,spe,species.locs,S,n.seed,n.juv,n.adult)
        n0.lst = setup.locations(n0.lst, spe, S, p, ic, settings, locations.file, SpTraits, rtd.c, run.path)
        #the initialize.species function wasn't doing anything setup.locations wasn't already doing. Change this if that changes
        #is.out = initialize.species(n0.lst,spe,S,SpTraits, settings, ic, run.times, run.lbl, first.run) 
        #  n0.lst = is.out[[1]]
        #  run.times = is.out[[2]]
        #  run.lbl = is.out[[3]]
        
        #Only loads a seed if testing == T #set.seed(444)
        if (exists(".Random.seed")){
            .Random.seed <<- load.seed(Model.Name, 3, testing)
            }
        
        run.times = c(run.times,gettime()); run.lbl = c(run.lbl,"Initial simulation setup complete")
    
        ## Get initial values for metrics for the model
        num.tp = length(timepoint.vec) #This could be moved somewhere else. Also used in calcuation of final metrics

        ini.abund.lst = list()
        for (sv in 1:length(scale.vec)){

            #Set up for subsetting (if any)
            scale.cells = extract.scale.cells(scale.cells.lst, sv, S) #The label in scale.vec is not needed, as the indices are all positional.

            #out.metric = calc.metrics(Results,"Initial",n0.lst,IAdults,spe,p,SpTraits)
            out.metric = calc.metrics.v2(Results,"Initial",n0.lst,IAdults,spe,p,SpTraits, scale.cells, sv, num.tp)
            #Update species richness, beta-diversity, and biomass & get abundance vector for calculating functional diversity
            Results = out.metric[[1]]
            ini.abund.lst = append(ini.abund.lst, list(out.metric[[2]])) #This is used to calculate functional diversity at the end of the simulation
                
            run.times = c(run.times,gettime()); run.lbl = c(run.lbl,"Initial results recorded")
            }
    
        # set up a look up table to record which values of change count correspond to which runtimes    
        change.count.lookup = sprintf("%schange_count_lookup.csv",outpath.base)
        cat("RunTime,ChangeCount\n", file = change.count.lookup)
        
        if (vdb.data == 1){
            ## Set up output file for stats for each timestep
            seed.stats = sprintf("%sSpeciesStats_Seeds.csv",outpath.base)
            juv.stats = sprintf("%sSpeciesStats_Juvs.csv",outpath.base)
            ad.stats = sprintf("%sSpeciesStats_Adults.csv",outpath.base)
            all.stats = sprintf("%sSpeciesStats_All.csv",outpath.base)
            cells.occupied = sprintf("%sCells_occupied.csv",outpath.base)
            
            #Create a text list of all the species to use as a header
            the.spp.lbl = sprintf("Sp%s", seq(1,spe)) 
            the.spp.lbl = listtotext(the.spp.lbl,",")
            the.hdr = sprintf("RunTime,%s\n",the.spp.lbl) 
            
            #Write the headers for the output files
            cat(the.hdr,file = seed.stats)
            cat(the.hdr,file = juv.stats)
            cat(the.hdr,file = ad.stats)
            cat(the.hdr,file = all.stats)
            cat(the.hdr,file = cells.occupied)

            #Set up to write species data to file (one big file instead of many small files!)
            #NA's are because only the header is being written (last 1)
            species.data = sprintf("%sSpeciesData.csv",outpath.base)
            write.sp.hdr.data(species.data, p)           
            }
        
        #simulate sequence
        for (RunTime in 1:MaxTime){
        
            ## Break the popoulation model into steps
            #Step 1                    
            for (sp in 1:spe){
                 
                #Demography 1 & migration for all species
                nt1a.lst[[sp]] <- t(P) %*% M.lst[[sp]] %*% P %*% B1.lst[[sp]] %*% n0.lst[[sp]]
                }
                
            if (RunTime == 1){
                run.times = c(run.times,gettime()); run.lbl = c(run.lbl,"First demography step completed (includes matrix setup for environmental change scenarios")
                }

           ## If invasion is allowed, carry out invasion
            if (invasion != 0){
                # Only do invasion if it is an invasion year (i.e. RunTime matches the invasion interval e.g., if invasion happen every 5 years, when RunTime is 5,10,15, etc. this will cause invasion        
                if (RunTime %% invasion == 0){
                    
                    #Note: Currently the magnitude of invasion is hardwired.  This could be adjusted to change in the code as well.
                      #Magnitude could be in terms of numbers invading or in terms of number of species invading!  Or number of cells invaded!
                    nt1a.lst = do.invasion(nt1a.lst,spe,p,S,SpTraits,num.invaders,cells.to.invade,repro.proportion)
                    }
    
                }
            
            ## Enforce seedling competition 
            
            #If competitiontype == 0, do nothing.
            if (competitiontype != 0){
                nt1a.lst = do.competition(competitiontype, microsites.vec, nt1a.lst, spe, ISeedBank, ISeedlings, p)
                }

    
            if (RunTime ==1 ){
                run.times = c(run.times,gettime()); run.lbl = c(run.lbl,"Seedling competition step finished")
                }
    
            
            #Step 2
            for (sp in 1:spe){
                #Demography 2
                nt1.lst[[sp]] <- B2.lst[[sp]] %*% nt1a.lst[[sp]]
                }
    
            if (RunTime ==1 ){
                run.times = c(run.times,gettime()); run.lbl = c(run.lbl,"Second demography step finished")
                }
    
            
            if (multi.species.K == 0){
                for (sp in 1:spe){
                    #enforce species specific adult carrying capacity K 
                    K = K.lst[[sp]]
                    nt1.lst[[sp]][IAdults]<-ifelse(nt1.lst[[sp]][IAdults] > K, K, nt1.lst[[sp]][IAdults])
                    }
            }else { 
                #Impose multispecies carrying capacity
                  #Currently only proportional to abundance of adults, no competitive advantages
                          
                #Expand ms.K to be for the whole landscape
                K_g.land = rep(K_g,p) #K_g.land is to denote that it is for the entire landscape
                
                #Sum total biomass of adults present.  If < K, do nothing
                BiomassperCell = rep(0,p) #Create a separate entry for each cell
                
                #**# This for loop could probably be replaced
                for (sp in 1:spe){
                    sp.txt = sprintf("%s",sp)
                    #Add previous biomass to (number of adults of the current species * biomass of an adult)
                    BiomassperCell = BiomassperCell + nt1a.lst[[sp]][IAdults] * SpTraits[sp.txt,"biomass.adult"]
                    }
                
                #If greater than K, figure out proportional reduction needed to reach K for each cell
                    #find proportion to reduce by
                    #Remember - you are doing this per cell, per species.  Here is the per cell part
                    #AdultsperCell * prop.reduction = K
                    #K / AdultsperCell = prop.reduction
    
                prop.reduction = rep(1,p) #This might actually not be needed - set up a vector to be filled
                prop.reduction = ifelse(BiomassperCell > K_g.land, (K_g.land / BiomassperCell),1)
                
                #**# This one too, perhaps?                   
                #Apply proportional reduction to each species
                for (sp in 1:spe){
                    #Don't actually need biomass here since this would be (adults * biomass * reduction) / biomass.  Biomass cancels, leaving adults * reduction
                    nt1.lst[[sp]][IAdults] = nt1.lst[[sp]][IAdults] * prop.reduction
                    }
                
                }
    
            if (RunTime ==1 ){
                run.times = c(run.times,gettime()); run.lbl = c(run.lbl,"Carrying capacity applied")
                }
        
            #Create lists to hold summary output if vdb.data is being used
            SeedBank.lst = Juveniles.lst = Adults.lst = All.lst = Cells.Occupied.lst = rep(NA,spe)
            
            for (sp in 1:spe){
                ## Set the initial population vector for the next run
                n0.lst[[sp]] <- nt1.lst[[sp]]  
                
                ## Compute summary information
                if (vdb.data == 1){
                    
                    #**# Optimization note: could change this so all information is written at once at the end - this would probably be quicker (don't have to keep writing to file)
                    
                    #Get information to output
                    VSeedBank <- nt1.lst[[sp]][ISeedBank]
                    SeedBank.lst[[sp]]<-sum(VSeedBank)
                    
                    VJuveniles <- nt1.lst[[sp]][IJuveniles]
                    Juveniles.lst[[sp]] <- sum(VJuveniles)
                    
                    VAdults   <- nt1.lst[[sp]][IAdults]
                    Adults.lst[[sp]]<- sum(VAdults)
                    
                    VAll = VSeedBank + VJuveniles + VAdults
                    All.lst[[sp]] = sum(VAll)
                         
                    #Calculate number of cells occupied by more than 1 adult. #Formerly was occupied by >2 adults.
                    Cells.Occupied.lst[[sp]] <- length(VAdults[ VAdults > 1])
                  
                    #Output species data to a large aggregate file
                    write.sp.data("Seeds",sp,RunTime,VSeedBank,extent,species.data)
                    write.sp.data("Juveniles",sp,RunTime,VJuveniles,extent,species.data)
                    write.sp.data("Adults",sp,RunTime,VAdults,extent,species.data)
                    write.sp.data("ALL",sp,RunTime,VAll,extent,species.data)
                  
                    # Plots left over from an earlier version of the code. Currently not used
                    # The plots will now be done in the server file, rather than here.
                    #Trellis1<-levelplot(SpatialDist.SeedBank, colorkey=TRUE, xlab="", ylab="")
                    #Trellis2<-levelplot(SpatialDist.Juveniles, colorkey=TRUE, xlab="", ylab="")
                    #Trellis3<-levelplot(SpatialDist.Adults, colorkey=TRUE, xlab="", ylab="")
                    #
                    #Row<-1 + RunTime/3 
                    #
                    #
                    #plot(Trellis1, split=c(1,Row, 3,4), more=TRUE)
                    #plot(Trellis2, split=c(2,Row, 3,4), more=TRUE)
                    #plot(Trellis3, split=c(3,Row, 3,4), more=TRUE)
                    }
                
                } #end of species loop
            
            # Write species summary data to file
            if (vdb.data == 1){
                seedbank.out = listtotext(SeedBank.lst,",")
                juv.out = listtotext(Juveniles.lst,",")
                ad.out = listtotext(Adults.lst,",")
                all.out = listtotext(All.lst,",")
                cells.occ.out = listtotext(Cells.Occupied.lst,',')
                
                cat(sprintf("%s,%s\n",RunTime,seedbank.out), file = seed.stats, append = T)
                cat(sprintf("%s,%s\n",RunTime,juv.out), file = juv.stats, append = T)
                cat(sprintf("%s,%s\n",RunTime,ad.out), file = ad.stats, append = T)
                cat(sprintf("%s,%s\n",RunTime,all.out), file = all.stats, append = T)
                cat(sprintf("%s,%s\n",RunTime,cells.occ.out), file = cells.occupied, append = T)            
                }

            ## ENVIRONMENTAL CHANGE
            #Set an indicator for whether matrices need to be reset or not (i.e. did change occur this time step?)
            time.step.change = 0
    
            #Do not do environmental change on the very last time step: no point: all demography, etc. has already taken place!
            if (RunTime != MaxTime){
    
              #Step 0 (only if in a changing environment)
              if (is.change == 1){
                                  
                  #Loop through change layers
                  for (ev in 1:length(env.c.freq)){
                      env.lyr = env.c.freq[ev]
                      
                      #Check if this environmental layer will ever change.  env.lyr == 0 will crash the next if statement (yields NAN instead of T/F
                      if (env.lyr != 0){
                                  
                          #Check if this environmental layer will change this time step
                          if (RunTime %% env.lyr == 0){
                              
                              #Set an indicator so that matrices and lists will be updated to take into account the new landscape
                              time.step.change = 1
      
                              #Change the landscape as appropriate
                              dec.out = do.env.change(my.env,ev,landscape,landscape.identifiers,p,run.path,env.lbl,s.lbl,landscape.dir, lnd.lbl, change.count)
                              landscape[[ev]] = dec.out[[1]]
                              my.env = dec.out[[2]]
                                                     
                              #NOTE: This timing step will not actually work, since this may not happen on the first run of the code!
                              if (RunTime == 1){
                                  run.times = c(run.times,gettime()); run.lbl = c(run.lbl,"Environment changed")
                                  }
                              }
                          }
                      }               
                  
                  #If there was environmental change this timestep, reset the lists
                  if (time.step.change == 1){
      
                      #Update the change counter to refelct the number of changes
                      change.count = change.count + 1
                            
                      ## Set up matrices based on the neew landscape to govern model behavior                     
                      #Calculate matrices for this species
                      mat.info = setup.matrices(K_g,spe,SpTraits,p,S,landscape,landscape.identifiers,distances,B1.template,B2.template,P,M_sub,DispPath,vdb.data,change.count,outpath.base,num.sim,run.times,run.lbl,multi.species.K,edge.type)
                        #Unpack mat.info for each species.
                        if (multi.species.K == 0){
                            K.lst = mat.info[[1]]
                            }
                        #else Doesn't matter - multispecies K will be applied later on.
                        B1.lst = mat.info[[2]]
                        B2.lst = mat.info[[3]]
                        DispersalProbabilities.lst = mat.info[[4]]
                        SeedDispersalRatio.lst = mat.info[[5]]
                        M.lst = mat.info[[6]]
                        run.times = mat.info[[7]]
                        run.lbl = mat.info[[8]]                                           
                      
                      #Recompute diagnostics for new landscape
                      if (do.diagnostics == 1){
                          cd.out = compute.diagnostics(spe,B1.lst,B2.lst,M.lst,P,S,p,outpath.base,vdb.data,change.count,run.times,run.lbl, Gave.Warn = Gave.Warn)
                            run.times = cd.out[[1]]
                            run.lbl = cd.out[[2]]
                            Gave.Warn = cd.out[[3]]
                          }
                      
                      #If visual output is desired, save files
                      if (vdb.data == 1){
                          for (b in 1:length(env.lbl)){
                              
                              #Check if a file was already used, if so, do not reproduce it
                              if (is.na(my.env$env.change.type[b]) | my.env$env.change.type[b] != "from.file"){ #Note: the second criteria crashes with an NA value, but because the first criteria evaluates first, if a value is NA, it will evaluate to True and skip the second step.
                                  lbl = env.lbl[b]                                  
                                  outfile = sprintf("%s%s_%s_%s.csv",landscape.dir,lbl,s.lbl,Model.Name)                                  
                                  write.env.lyr(landscape[[b]],change.count,extent,outfile)              
                                  }
                              }
                          }
                      }
                  }
              }
        
            #Write which change step corresponds to which runtimestep
            cat(sprintf("%s,%s\n",RunTime,change.count), file = change.count.lookup,append = T)
                    
            if (RunTime ==1 ){
                run.times = c(run.times,gettime()); run.lbl = c(run.lbl,"First timepoint completed")
                }
                
            }#end of loop over time series                    
    
        #Output the last timestep run by the model so that one can check that the model ran to completion
        Results$LastTime[1:nrow(Results)] = RunTime
    
        run.times = c(run.times,gettime()); run.lbl = c(run.lbl,"Remaining simulation runs completed")
       
        #Calculate log lambda & dispersion of log lambda for all life stages
        #If only run for one time step, this makes no sense, because you cannot calculate loglambda. Although running the model for a single time step should be relatively rare.
        if (MaxTime > 1 & vdb.data == 1){
            logL.all = get.log.lambda(all.stats,"all")
        
            #Calculate log lambda & disperson of ll for only adults
            logL.ad = get.log.lambda(ad.stats,"adults")
          
            #Calculate log lambda & dispersion of ll for each cell
            #LogL.all.cell = get.log.lambda(species.data, lifestage = "ALL") # NEEDS SCRIPTING
        
            #Calculate log lambda & dispersion of ll for each cell for only adults
            #LogL.ad.cell = get.log.lambda(species.data, lifestage = "Adults") # NEEDS SCRIPTING
            logLs = list(logL.all,logL.ad)

            ll.out = sprintf("%sLogLambda.csv",outpath.base)
            compile.log.lambdas(logLs,ll.out)
            }
   
        for (sv in 1:length(scale.vec)){
        
            #Recover the associated ini.abund.vec
            ini.abund.vec = ini.abund.lst[[sv]]

            #Set up for subsetting (if any)
            scale.cells = extract.scale.cells(scale.cells.lst, sv, S) #The label in scale.vec is not needed, as the indices are all positional.
        
            #Calculate species richness, beta-diversity, and biomass at end of simulation
            timepoint = "Final"                  
            metric.out = calc.metrics.v2(Results,timepoint,n0.lst,IAdults,spe,p,SpTraits, scale.cells, sv, num.tp) #n0.lst is still appropriate here, because nt1.lst is reassigned to n0.lst at each model step.
            Results = metric.out[[1]]
            fin.abund.vec = metric.out[[2]]

            run.times = c(run.times,gettime()); run.lbl = c(run.lbl,"Calculate final metrics")

            
            #Calculate functional diversity at end of simulation
            abund.mat = matrix(c(ini.abund.vec,fin.abund.vec), nrow = 2,byrow = T,dimnames = list(c("Initial","Final"),colnames(ini.abund.vec)))
            Results = calc.fd(Results,SpTraits,abund.mat,spe,resolution,log.trans,sv, num.tp)

            run.times = c(run.times,gettime()); run.lbl = c(run.lbl,"Calculate functional diversity")

            
            #Calculate change in functional diversity over the course of the simulation
            Results = calc.delta.metrics.v2(Results,out.metrics,sv,num.tp)

            run.times = c(run.times,gettime()); run.lbl = c(run.lbl,"Calculate change in metrics")            
            }

        # Output results from this replicate
        write.table(Results,file = ResultsFile,col.names = F,row.names = F, sep = ",",append = T)
        
        }

    run.times = c(run.times, gettime()); run.lbl = c(run.lbl, sprintf("%s completed", Model.Name))
    GetTimes(start.time,run.times,run.lbl,timefile,write.timefile,Model.Name)
        
    return(Results) #These have been cleared, and returning it saves me from re-creating the data frame (although maybe that would be faster?)
    }
