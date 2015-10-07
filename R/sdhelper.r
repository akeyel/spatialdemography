#Helper functions for SpatialDemography.  See spatialdemography package license & details
#Originally by J. Gerstenlauer
#Updated by Sasha Keyel, skeyel@gmail.com

    
# Functions from myr.r version 1.0.1
    #as.num = function to shorten as.numeric(as.character()) command
    #Digit - add leading zeros to allow proper sorting
    #force.hibernate = hibernate the computer (force means it will still work if the machine is locked)
    #gettime = function to get the time from the code in a more intuitive manner.
    #listtotext - convert a list to text NOTE: THIS FUNCTION ALSO APPEARS IN MULTIRICH
    #sample.fixed = a function to fix R's sample function to remove inconsistent behavior

# Functions based on I. Stott's code
    #is.irreducible
    #is.ergodic

# Index to broken functions
    #LocalTransitionMatrix = function for computing the local transition matrices, for eigenvector analysis
    #CalcA = calculate the overall matrix properties - may be useful as a diagnostic tool.  Not currently in the model
    #CalcEigen = function to compute Eigen analyses - needs updating!
    #CalcPerturbation = calculate perturbation analysis for the matrix model
    #EigenVis = function to visualize the eigenvalues on the landscape
    #DispersalVis = function to visualize dispersal on the landscape

#' MetaComeSE (package)
#' 
#' Package to run a spatially explicit, matrix-based metacommunity model
#'
#' @details \tabular{ll}{ %% Note tabular{ll} is tabular{lowercase(LL)} not tabular{11} 
#' Package: \tab spatialdemography\cr
#' Type: \tab Package\cr
#' Version: \tab 0.10.5\cr
#' Date: \tab 2015-09-29\cr
#' License: \tab GPL-2 (or later)\cr
#' }
#' @author Alexander "Sasha" Keyel & Jakob L.K. Gerstenlauer\cr
#' Maintainer: Sasha Keyel <skeyel@@gmail.com>
#'
#' @template mycitation
#' @keywords package
#' @name spatialdemography_package
#' @docType package
#' @seealso \link[spatialdemography]{SpatialDemography}
#' @import Matrix
NULL

################ FUNCTIONS FROM MYR.R Version 1.0.1 ######################
#DD# Think about whether to export these functions - they seem generally useful to me, but are not the focus of the package.

#' As number
#'
#' A shortcut to avoid having to always write out as.numeric(as.character(x))
#'
#' @param x the thing to be converted to numeric
#'
as.num = function(x){

    #check if length of x is > 1
    if (length(x) == 1){
        #if so, and x is NA, mark NA
        if (is.na(x)){
            out = NA
        #otherwise
        }else{
            #if x is "NA", mark as NA
            if (x == "NA"){
                out = NA
            #otherwise proceed as normal
            }else{
                out = as.numeric(as.character(x))
                }
            }
    #if length x > 1, proceed as normal
    }else{
        out = as.numeric(as.character(x))
        }

    return(out)
    }

#' Digit
#'
#' Digit takes a number and places leading zeros before it if needed to bring
#' the length of the number up to places. Inputs can be either numeric or
#' character but must be integers (e.g., "1" or 1 are okay, 1.5 is not).
#' @param number The input number (potentially) in need of leading zeros
#' @param places The number of places the final number should have.  Needs to be
#' greater than or equal to one.
#' @return A number with leading zeros in character format
#' @note I have written a similar function for Python, but with a different
#' implementation.
#' @author Alexander "Sasha" Keyel
#' @examples
#' Digit(10,3)  #010; one leading zero added
#' Digit(5,3)   #005; two leading zeros added
#' Digit(200,3) #200; long enough, no leading zeros added
#' Digit(2000,3) #2000; no leading zeros, no truncation (places 4 would be better)
#' @export Digit
Digit = function(number,places){
    
    #Check that Digit is a single number, not a vector
    if (length(number) > 1){
        stop("Inputs to Digit function must be single numbers, not vectors!")
        }
    
    #Check that input is an integer, if not, return NA
    test = as.integer(number)
    if (test != number){
        stop("Inputs to Digit function must be integers")
        }
        
    if (places < 1){
        stop("Number of places to round to is less than 1. This is a non-sensical input for this function")
        }
    
    number = as.character(number) #Convert number to string
    nlen = nchar(number)
    #If number is longer than the number of places needed, leave the number unchanged (other than converted to string)
    #otherwise, continue until number = places
    while (nlen < places){
        zro = "0"
        number = sprintf("%s%s",zro,number)
        nlen = nchar(number)}
        
    return(number)
    }

#Tell the computer to hibernate in a way that will still work if the machine is locked.
#' Hibernate
#'
#' Hibernate the computer.
#' 
#' Can be used following simulation runs to shut-down the computer.
#' Hibernate is chosen over shutdown, as this allows direct access to the
#' current R session (rather than requiring reloading a saved session, where you
#' can no longer see the commands & have to deal with the changed workspace.
#' Note that the force option is used, which will cause the computer to
#' hibernate even if other programs are running, or if the computer is locked.
#'
force.hibernate = function(){
    shell("shutdown -h -f")
    }

#' Get Time
#'
#' A short function so I don't have to keep writing proc.time()[3]
#'
gettime = function(){
    outtime = proc.time()[3]
    return(outtime)
    }

#' List to text
#' 
#' Take a list (or vector) and convert it to text separated by separator.
#' Does not work for nested lists
#'
#' @param inlist a list to convert to text
#' @param separator The separator to separate the text
#'
listtotext = function(inlist,separator){

    #Check that there is no nesting in the list structure
    for (thing in inlist){
        if (length(thing) > 1){
            print("One or more list elements containied multiple elements.")
            print("This is not allowed, as it will lead to strange outputs.")
            return(NA)
            }
        }
        
    #Check that separator is input (otherwise it can let a typo through where just a one element list or a separator is given)
    sep.check = separator #sep.check is not used later.  It is here to make it so R will crash if it is not input.

    #Perform list to text function
    textout = sprintf("%s",inlist[1])
    listlen = length(inlist)
    if (listlen > 1){
        for (item in inlist[2:listlen]){
            textout = sprintf("%s%s%s",textout,separator,item)
            }
        }
    return(textout)
    }

#Default behavior of this function was unacceptable, so I fixed it
#' Sample (fixed)
#'
#' Goal is to correct the default behavior of the "sample" function in R.
#' The default behavior changes if only one sample is needed, but some of my
#' code depends on consistent behavior.  Consequently, this function avoids
#' the change in behavior.
#'
#' @param x Vector to be sampled
#' @param size Number of samples to take.  Must be an integer less than x but
#' greater than zero.
#' @return A vector of length size sampled from x.  If the length of x is 1,
#' this returns x.
#'
sample.fixed = function(x,size){

    #Require that size be >= 0 (should it just be > 0?)
    if (size < 0){
        stop("number of samples taken must be zero or greater")
        }

    if (length(x) < size){ 
        stop("There was an error - more samples were required than values to sample from!")
        }

    #Correct the default behavior so that asking for one sample from a vector length 1 gives the element of the vector, instead of drawing from 1:the value of the element.  See documentation
    if (length(x) == 1){
        output = x
        }

    #Otherwise, proceed as normal.
    if (length(x) != 1){
        output = sample(x,size)
        }
    return(output)
    
    }

############## FUNCTIONS FROM CODE SUPPLIED BY IAIN STOTT TO JAKOB GERSTENLAUER.############
#Permission to use them in SpatialDemography granted on Oct 23, 2014, until popdemo is patched to accept sparseMatrix format

#'1. TEST REDUCIBILITY OF THE MATRIX
#'
#' This function tests whether a matrix is reducible or irreducible.
#' IT WILL ONLY RELIABLY WORK FOR NONNEGATIVE MATRICES (e.g. PPMs).
#' It works on the basis that for any matrix A, (I+A)^(s-1) where I
#' is the identity matrix and s is equal to the order of the matrix
#' is positive (Caswell 2001).  The function returns the condition
#' "TRUE: Irreducible" or "FALSE: Reducible".
#' @param x a square nonnegative matrix
#' @author Iain Stott, 9.1.2009
is.irreducible<-function(x){

    #create an object 'order' that is equal to the dimension of x.
    order=sqrt(length(x))
    
    #create an identity matrix of the same size as x
    identity=diag(order)
    
    #create an object 'identityplusx'=I+A
    identityplusx=identity+x
    
    #create a character string for multiplication of I+A and repeat it s-2 times
    string=rep("identityplusx%*%",order-2)
    
    #end the character string so that it now repeats multiplication of I+A s-1 times
    string=c(string,"identityplusx")

    #collapse the character and evaluate it, omitting quotation marks
    string=paste(string,collapse="")
    powermatrix=eval(parse(text=noquote(string)))

    #create an object 'minval' that is equal to the smallest entry of the matrix
    minval=min(powermatrix)

    #if this minimum is greater than 0 (i.e. the matrix is positive), then return
    #"TRUE: Irreducible", if it is not greater than 0 (i.e. the matrix is not positive)
    #then return "FALSE: Reducible", omitting quotation marks.
    if(minval>0)return(noquote("TRUE: Irreducible"))else(noquote("FALSE: Reducible"))
    }

#' 2. TEST ERGODICITY OF A MATRIX
#'
#' This function will test the ergodicity of a matrix based upon whether its dominant
#' left eigenvector is positive or not.  A reducible-ergodic matrix has a positive
#' dominant left eigenvector, where a reducible-nonergodic matrix will contain zeroes
#' in its dominant left eigenvector.  An irreducible matrix is always ergodic.
#' NOTE: this function can prove problematic for imprimitive, irreducible matrices or
#' reducible matrices with imprimitive, irreducible blocks on the diagonal.  The
#' 'dominant' left eigenvector chosen by R is that with the largest absolute value
#' (including both real and imaginary components), so that sometimes it chooses a dominant
#' with imaginary components.  In addition, where a reducible matrix in block-permuted form
#' contains one or more irreducible, imprimitive blocks on the diagonal, values of zero in
#' the dominant left eigenvector may actually be calculated by R as small numbers, approximate
#' to zero.  Hence, for such matrices, R may return a spurious result of the matrix being ergodic
#' when in fact it is nonergodic.  For this reason, the 'dominant' left eigenvector is also returned
#' by the function so that it may be checked by eye: if it contains non-zero imaginary components,
#' or if it contains small numbers that may approximate to zero then further diagnosis may be
#' required.  The 'blockmatrix' and 'is.primitive' functions can help to
#' diagnose whether diagonal blocks are imprimitive or not.
#' @param x a square nonnegative matrix
#' @author Iain Stott, 29.1.2010
is.ergodic=function(x){
    
    #create an object 'order' that is equal to the dimension of the matrix
    order=sqrt(length(x))
    
    #create an object 'xt' that is the transpose of x
    xt=Matrix::t(x)
    
    #create an object 'lefteigvec' that is the dominant right eigenvector of xt, hence the dominant
    #left eigenvector of x
    lefteigvec=as.matrix(eigen(xt)$vectors[,1])
    
    #create an object 'abslefteigvec' that is the modulus of the dominant left eigenvector
    abslefteigvec=as.matrix(abs(lefteigvec))
    
    #create an empty object 'ergodic'
    ergodic=numeric(1)
    
    #if the minimum absolute value of the dominant left eigenvector is >0 then the matrix is
    #ergodic, otherwise it is nonergodic
    if(min(abslefteigvec)>0) ergodic= 1 else(ergodic= 0 )
    
    #return 'ergodic' and 'lefteigvec'.
    return(list(ergodic=ergodic,lefteigvec=lefteigvec))
    }


################# SPATIALDEMOGRAPHY FUNCTIONS ###############################

#' Check for required packages
#'
#' This function checks that all required packages needed to run SpatialDemography
#' are present and usable. Packages to check are:
#' Matrix, multirich, FD, and copula.
#' spatialdemography may also use lhs and raster, but these are used in exported
#' functions that contain their own checks.
#' Hmisc, quantreg & gbm were formerly listed under suggests, but are currently
#' not part of the code workflow (i.e. they would require an external call to an internal function)
#' lattice was formerly listed under suggests, but I don't think it was actually used
#' so it is no longer listed nor is it checked for.are not integrated into the code.
#'
#' @param out.metrics A vector of potential metrics to calculate. Those ending in UTC
#' will require multirich to calculate, and those ending in FRIC will require
#' FD to calculate. At some point, I need to add in Petchey & Gaston's FD metric.
#' @param include.copula An entry from the settings file. If null or 0, the copula
#' package is not required.
#'
check.packages = function(out.metrics, include.copula){

    missing.pkg = 0

    #Check outmetrics if multirich or FD will be required
    for (item in out.metrics){
        test = substring(item,nchar(item) - 2,nchar(item))
        if (test == "UTC"){
            if (!require(multirich)){
                warning("Package multirich must be installed to calculate the specified or default output metrics. Please install package multirich (install.packages('multirich') ")
                missing.pkg = 1
                }
            }
        if (test == "FRIC"){
            if (!require(FD)){
                warning("Package FD is required to calculate the specified output metrics. Please install package FD (install.packages('FD'))")
                missing.pkg = 1
                }
            }
        }
        
    if (length(include.copula) > 0){
        if (include.copula > 0){
            if (!require(copula)) {
                warning("Please install copula: install.packages('copula')")
                missing.pkg = 1
                }
            }
        }
        
    if (missing.pkg == 1){ stop("One or more required packages is missing. See warnings for details.") }
    }


#' do.input.rename = Helper function to help with renaming
#' 
#' Rename input templates with a specified file ending
#' @param input.path Path for input files
#' @param to.rename File names to be renamed
#' @param file.ending The file ending to add/replace the previous file ending
#'
#' @export do.input.rename
do.input.rename = function(input.path,to.rename,file.ending){
    
    inputs = list.files(input.path)
    
    for (item in to.rename){
        old.file = grep(item,inputs,value = T)
        old.file = sprintf("%s%s",input.path,old.file) #Add path to old.file for proper renaming
        new.file = sprintf("%s%s%s.csv",input.path,item,file.ending)
        file.rename(old.file,new.file)
        }

    }

#' Save Seed
#'
#' Simple function to create a seed token, that can later be assigned to .Random.seed
#' this will allow reproducibility of results, even when pieces of the code are moved around
#' The downside is that the code will slowly get cluttered with seed tokens, which at some point
#' will need to be replaced & the test code updated.
#' But this will forstall that day, and at least ensure that code that passed the test
#' is being used to replace the test code. And that reassures me greatly.
#' Thanks to Ben Bolker's answer on Stack Overflow:
#'  http://stackoverflow.com/questions/13997444/print-the-current-random-seed-so-that-i-can-enter-it-with-set-seed-later
#' The seed can be restored by doing load("SeedToken%s.RData"), where %s is the seed.id you want to access.
#'
#' @param run.id Identifies which test run is being run
#' @param seed.id Identifies the location where the seed needs saving
save.seed = function(run.id, seed.id){
    my.seed.token = .Random.seed
    save("my.seed.token", file = sprintf("SeedToken%s_%s.RData", run.id, seed.id) ) #Saves the random seed as a seed token
    }

#' Load Seed
#'
#' Simple function to load a seed token (and ensure that you don't break the normal code while doing so!
#' WARNING: Output from this function needs to be assigned to .Random.seed with the global assignment operator <<-
#' OTHERWISE the assignment will be local to the FUNCTION ONLY and will not properly reset the random seed!
#' @param run.id Identifies which test run is being run
#' @param seed.id Identifies the location where the seed needs saving
#' @param testing TRUE/FALSE Indicator for whether a test is being run
load.seed = function(run.id, seed.id, testing){
    my.seed.token = .Random.seed
    if (testing == T){
        #Set seed to match that obtained after running species locations code to use prior test code for validation
        seed.token = sprintf("SeedToken%s_%s.RData", run.id, seed.id)
        load(seed.token) #Adds an object my.seed.token to the function workspace (but not to the global workspace)
        message(sprintf("Seed token %s applied", seed.token))
        }
    return(my.seed.token) # .Random.seed <<- load.seed(run.id, seed.id, testing) #the global assignment operator is VERY important here, and I couldn't figure out a way around using it.
    }

# Short code used to replace seed tokens so that they would have the my.seed.token object.
#run.names = c("TestRun1","TestRun2")
#bits = c(1,2,3)
#
#for (rn in run.names){
#    for (bit in bits){
#        load(sprintf("SeedToken%s_%s.RData", rn, bit))
#        save.seed(rn,bit)
#        }
#    }

### From Connectance.r
#' Calculate distances between cells
#' 
#' Calculates euclidean distance between all cells in a landscape
#' 
#' @details Designed with a square landscape in mind, but it will work for other configurations
#' Basically, cells are added by rows, and if there are not enough cells to complete a row, the row is missing some cells.
#' @param NumPatches Number of cells in the landscape, equal to the extent squared
#' @param extent Length of one side of the square landscape
#' @author Jakob Gerstenlauer, with minor modification by A.C. Keyel
#'
Connectance<-function(NumPatches,extent){

    #Check that extent is reasonable (i.e., non-negative!)
    if (extent < 1){
        stop("Extent cannot be less than one.")
        }
        
    #Figure out how to do warnings - issue warning if landscape is not square
    if (NumPatches != (extent ^ 2)){
        stop("Landscape must be square")
        }

    #create a new matrix containing pairwise distances between cells
    Distances<-matrix(rep(0.0,NumPatches^2), nrow=NumPatches, ncol=NumPatches)

    #for all patches as origin cells
    for (Origin in seq(0,NumPatches-1)){

        #for all patches as target cells
        for (Target in seq(0,NumPatches-1)){

           #only calculate distance if patches are distinct
           if (! identical (Origin,Target)){

                #calculate the indices of the origin cell
                xpos<-1 + Origin - floor(Origin / extent) * extent;
                ypos<-1 + Origin %/% extent;  #The %/% is a modulus. I do not think the semi-colons do anything here

                #calculate the indices of the target cell
                xposT<-1 + Target - floor(Target / extent) * extent;
                yposT<-1 + Target %/% extent;

                #the absolute value of the distances in x and y direction
              	dx <- xposT-xpos;
              	dy <- yposT-ypos;

                #The dispersal matrix M later is of the form p*p (p=numer of patches), where mij denotes the probability
         	      #of dispersal from patch j (origin) to patch i (target), thus D[Target,Origin]!
               	Distances[Target+1, Origin+1] <- sqrt( dx*dx + dy*dy); #calculate the euclidean distance
                }

            #if target and origin are identical then write zero to that element
            else{
                Distances[Target+1, Origin+1] <-0.0
                }
            }
        }

    return(Distances)
    }

### Sub-functions for running the simulation (called by simulation2.r which is called in turn by Metacommunities.r, modified from Simulation.r

#' Create Permutation Matrix
#'
#' Creates the vec-permutation matrix for use in running the matrix model
#'
#' @param p The number of cells in a square landscape
#' @param S The number of stages in the matrix model
#'
#'@author Jakob Gerstenlauer
#'
CreatePermutationMatrix = function(p,S){

    #The permutation matrix has dimension s*P x s*P
    Dim.PermutMatrix <- S*p
    Elements.PermutMatrix <-  (S*p)^2

    Row<-Column<-rep(0, S*p)

    index<-0
    for( i in 1:S){
        for( j in 1:p){
            index<-index+1
            Column[index]<- (j-1)* S + i
            Row[index]<- (i-1)* p + j
            }
        }

    P<-sparseMatrix(i = Row, j = Column, x= rep(1,length(Row)),dims=rep(S*p,2))

    #Assign the (identical) dispersal submatrices for 2<-2,3<-3,4<-4 submatrix
    #this submatrix is sparse and has ones in the diagonal
    #Here we store it as a sparse vector
    Dim<-p^2
    index<-0:(p-1)
    Index<-1+index*p+index
    M_sub<-sparseMatrix(i = rep(1,p), j = Index, x= rep(1.0,p), dims=c(1,Dim))

    cpm.out = list(M_sub,P)
    return(cpm.out)
    }

#' get positions
#' 
#' Get positions for assigning vital rates to the appropriate matrix locations
#'
#' @param x The vital rate in need of a position
#' @param cell cell
#' @param p The number of cells in a square landscape
#' @param S The number of stages in the matrix model
get.positions = function(x,cell,S,p){
    # Old version as a guide
    #Put diagram illustrating the pattern somewhere.
    #this.pos = S * p * block.index + S * p * row.index + S * block.offset + position.offset 

    block.index = S * p * (cell - 1) * S #Determine which cell a block of vital rates belong in
    block.offset = S * (cell - 1)   #Determine where in the cell the vital rates belong 

    position.offset = as.numeric(substring(x,2,2)) + 1 #Determine where in a row the vital rate belongs
    row.index = S * p * (as.numeric(substring(x,3,3))) #Determine what row the vital rate belongs in.  # Warning: this won't be the most intuitive, because my description assumes a matrix format, but the actual assignment is done to a vector.
    position = block.index + row.index + block.offset + position.offset

    return(position)
    }

#' Get adjusted positions
#'
#' Get positions for adjusting the B1 matrix and for assigning to the B2 matrix
#' @param row.indicator An indicator for the row
#' @param cell The target cell
#' @param p The number of cells in a square landscape
#' @param S The number of stages in the matrix model
#'
get.adj.pos = function(row.indicator,cell,S,p){
    pos.offset = 1 #In this case it is always the first vital rate that needs adjusting
    row.index = S * p * row.indicator
    
    block.index = S * p * (cell - 1) * S #Determine which cell a block of vital rates belong in
    block.offset = S * (cell - 1)   #Determine where in the cell the vital rates belong 

    position = block.index + row.index + block.offset + pos.offset
    return(position)
    }

#DD# Update documentation for functions 104 & 105
#' Get Modifier
#'
#' Purpose is to calculate vital rate modifiers based on input functions.
#'
#' @param env.val The value from a focal environmental layer for a given cell
#' @param opt.val The target value for the focal species
#' @param funct A function describing how to determine the modifier based
#' on env.val and opt.val.  A list of possible functions is given in #LL# LOOK UP
#' @param par1 The first input parameter to the function.
#' @param par2 The second input parameter to the function
#' @return Returns a modifier between zero and one that can be multiplied with
#' the base vital rate.
#' @author Alexander "Sasha" Keyel
#'
get.mod = function(env.val,opt.val,funct,par1,par2){

    if (funct != 104 & funct != 105){
        par1 = as.num(par1)
        par2 = as.num(par2)
        }
    
    #Indicator to check whether modifier values are constrained to 0 & 1
    check.constr = 1
    
    #apply a quadratic decrease 
      #follows form of equation of y = -a(x-h)^2 + k, where (h,k) is the vertex, and k = 1 because that is no reduction
    if (funct == 102){
        modifier = -par1 * (env.val - opt.val)^2 + 1
        }
    
    #Apply a simple match/nomatch function for categorical layer values
      #e.g., if layer is grazed (g)/not grazed (ng), and species are grazed specialists (gs) or ungrazed specialists (us) there are 4 possible combinations (generalist species should not be coded with this function!)
      #Simplest setup would be to have the response for a gs in ng the same as the response for a us in g.  But I don't know that that is realistic.  But it's symmetric
    if (funct == 101){
        if (env.val == opt.val){
            modifier = par1
        }else{
            modifier = par2
            }
        }
    
    #Create a function to read the modifier directly from the environmental layer
    if (funct == 103){
        modifier = env.val
        check.constr = 0
        }
    
    #Do Stochastic plants style modifier for seed number
    if (funct == 104){ 
        #opt.val = value specified as base vr for seed production (allows calculation of the modifier)
        #par1 = biomass.adult;biomass.seed;biomass.clone
        #par2 = relative.allocation.reproduction;ratio.sex.allocation 
        par1.split = strsplit(par1,":")[[1]] #strsplit makes a list of length 1, I want a vector.  Can't use semicolons as delimiters - they're already used in an earlier step!
        biomass.adult = as.num(par1.split[1])
        biomass.seed = as.num(par1.split[2])
        biomass.clone = as.num(par1.split[3])
        
        par2.split = strsplit(par2,":")[[1]]
        a = as.num(par2.split[1])
        g = as.num(par2.split[2])
        
        fertility = env.val
        
        q = biomass.adult * a * fertility   #in grams
        num.seeds = q * (1 - g) / biomass.seed

        #Convert to a modifier to remain consistent with rest of code
        modifier = num.seeds / opt.val #This will later be multiplied by opt.val, allowing that part to cancel out leaving the new seed number.        
        check.constr = 0
        }
    
    #Stochastic plants style modifier for clone number.
    if (funct == 105){
        #opt.val = value specified as base vr for clone production (allows calculation of the modifier)
        #par1 = biomass.adult;biomass.seed;biomass.clone
        #par2 = relative.allocation.reproduction;ratio.sex.allocation 
        par1.split = strsplit(par1,":")[[1]] #strsplit makes a list of length 1, I want a vector.
        biomass.adult = as.num(par1.split[1])
        biomass.seed = as.num(par1.split[2])
        biomass.clone = as.num(par1.split[3])
        
        par2.split = strsplit(par2,":")[[1]]
        a = as.num(par2.split[1])
        g = as.num(par2.split[2])
        
        fertility = env.val
        
        q = biomass.adult * a * fertility   #in grams
        num.clones = q * g / biomass.clone

        #Convert to a modifier to remain consistent with rest of code
        modifier = num.clones / opt.val #This will later be multiplied by opt.val, allowing that part to cancel out leaving the new seed number.        
        check.constr = 0
        }
    
    # Threshold response
    if (funct == 106){
      # Default is full suitability
      modifier = 1
      
      lower.threshold = par1
      upper.threshold = par2
      
      # If lower.threshold is not missing, check if environmental value is lower than that.
      if (!is.na(lower.threshold)){
        if (env.val < lower.threshold){
          modifier = 0
        }
      }
      
      # If upper.threshold is not missing, check if environmental value is greater than upper threshold
      if (!is.na(upper.threshold)){
        if (env.val > upper.threshold){
          modifier = 0
        }
      }
    }
    
    if (check.constr == 1){
        #Check that modifier is plausible
        if (modifier > 1){
            warning("A modifier was greater than 1. Modifier changed to 1. Please check function inputs for accuracy")  
            modifier = 1
            }
        if (modifier < 0){
            #Turned the warning off - too common
            #warning("A modifier was less than 0.  Modifier changed to 0.")
            modifier = 0
            }
        }
    
    return(modifier)    
    }

#DD# Might be good to expand the documentation a little on this one.

#' Create a composite demography matrix
#'
#' Creates the demographic matrices used in the matrix model
#'
#' @param B1.template A template for creating the B1 matrices
#' @param B2.template A template for creating the B2 matrices
#' @param spe The number of species in the species pool
#' @param SpTraits The traits (i.e. vital rates, response modifiers and dispersal parameters)
#' @param p The number of cells in the landscape
#' @param S The number of life stages in the matrices
#' @param landscape A list object containing the environmental layer values
#' @param landscape.identifiers A list of single letter identifiers for each environmental layer
#' @note The original notation used in this function was changed to be consistent & intuitive for ACK.
#' @author Jakob Gerstenlauer and Alexander "Sasha" Keyel
#'
CreateCompositeDemographyMatrix = function(B1.template,B2.template,spe,SpTraits,p,S,landscape,landscape.identifiers){
      
    evt = SpTraits #Patch, because I don't want to replace the old evt with SpeciesTraits each time.

    #Incoming format for species traits needs to be:
    #optimumvalue;function;par1;par2 - have them all in the same cell in the csv file so they can be dealt with together!

    #List of matrix vital rates
    vital.rates.base = c("p01","p02","p11","p12","p22","p23","p33","p30","p32")    #no p31, Adults only produce mobile seeds (not all of which must disperse).
    hdr = names(evt)

    #create a variable to contain vital rates.
      #It will be sorted by species, then by cell, then by vital.rates.base
      #Should correspond to the vital.rates variable from the previous version of this code
      #This could all be put into a function to make the code more readable, by the way.
    vr.vals = c()
    for (sp in 1:spe){

        for (cell in 1:p){
            env.values = c()
            #get a vector of environmental layers in the same order as landscape
            for (e.lyr in 1:length(landscape)){
                this.e.lyr = landscape[[e.lyr]]
                e.lyr.val = this.e.lyr[cell]
                env.values = c(env.values,e.lyr.val)
                }
                

            for (v in 1:length(vital.rates.base)){
                vr = vital.rates.base[v]
                #vr = "p23"
    
                this.sp = sprintf("%s",sp) #convert species to character for lookup purposes
                base.vr = as.num(evt[this.sp, vr])
            
                #Extract vital rate modifiers
                vr.info = grep(vr,hdr, value = T)
    
                modifiers = c()
                #loop through each potential modifier
                for (thing in vr.info){
                    #If thing == vr, do nothing, this has already been extracted
                    if (thing != vr){
                        #Find out which environmental layer it corresponds to:
                        ident = substr(thing,1,1)
                        env.mod.index = which(ident == landscape.identifiers) #Find out which environmetnal layer ident corresponds to
                        env.value = env.values[env.mod.index] #Get the environmental value for this cell
                        stuff = as.character(evt[this.sp,thing]) #**# May speed the code up to do this once after reading in SpTraits!
                        stuff = strsplit(stuff, ";", fixed = T)[[1]] #the [[1]] is because R turns it into a list for some reason (beyond just what holds the values)
                        opt.val = as.num(stuff[1])
                        funct = as.num(stuff[2])
                        par1 = stuff[3]
                        par2 = stuff[4]

                        modifier = get.mod(env.value,opt.val,funct,par1,par2)
                        modifiers = c(modifiers,modifier)        
                        }
                    }

                this.vr = base.vr * prod(modifiers) #Multiply the base rate by all modifiers affecting the base rate.  If modifiers is c(), this will yield 1 and have no effect
                vr.vals = c(vr.vals, this.vr)
                }    
            }
        }
    

    # NEED THE VITAL RATES TO BE BLOCKED BY SPECIES, THEN BY CELL

    #Set up so that each vital rate is present for each cell on the landscape
    vital.rates = rep(vital.rates.base,p * spe)     #**# This is 180,000 elements for a 20 x 20 landscape & 50 species 
 
#    # Need to get a vital rates vector with all species, labeled properly
#    sp.vec = seq(1,spe,1)
#    sp.vec = sapply(sp.vec,Digit,2) #Convert to text with leading 0's for proper sorting
#    sp.lbl = sprintf("sp%s",sp.vec)        #create a label for each species (used in create composite demography matrix)
#    sp.lbl = rep(sp.lbl,(length(vital.rates)/spe)) #Extend it so that there is a species label for each vital rate
#    sp.lbl = sort(sp.lbl) #Sort it so they are in the correct order
#    vital.rates = sprintf("%s%s",vital.rates,sp.lbl) #Add the species label to look up species specific vital rates
#    

    #**# Is there a fast way to get a vector from 1 to p * vr * sp that would correspond to a cell index?
    cell.index = seq(1,p,1)
    cell.index = rep(cell.index,length(vital.rates.base)) #Create a cell index for each vital rate
    cell.index = sort(cell.index) #Sort so that the cell indices are in the proper order #**# Seems like if there was a way to avoid this step, it would be faster (i.e. create the index in the proper order to begin with.
    cell.index = rep(cell.index,spe) #Create a separate index for each species

    positions = mapply(get.positions,vital.rates,cell.index,MoreArgs = list(S = S,p = p))
    
    #Assign matrices for each species #**# Not the most efficient approach, but will let me see what speed gains I've already achieved.
    B1.lst = list()
    B2.lst = list()

    #This is the length of the vital rates for one species for all cells
    sp.offset = length(vital.rates.base) * p

    #Set up adjustment positions for B1 matrix
    cell.indices = sort(rep(seq(1,p,1),3)) #Create 3 sets of cell indices, one for each position that needs replacing, sorted.
    row.indicators = rep(seq(0,2,1),p) #Create p sets of row indices, one for each row that needs replacing.  Starts with 0 to be consistent with approach taken in initial assignment of positions    
    adj.pos = mapply(get.adj.pos,row.indicators,cell.indices,MoreArgs = list(S = S,p = p)) # Include the postions for stage 0 to 0 - will be 0 already in B1, but needs to be 0 in B2.

    #This could just be done once outside this code.  Although now that this code doesn't get looped, it might not need it.

    #**# Wouldn't it be better & more intuitive to assign B1 & B2 vital rate values separately?  Probably slower (but not by much), but more intuitive
    #This could probably be replaced by a lapply, with B as an extra input that is then returned.  That would work & get rid of another for loop.  But might not speed things up much.
    for (sp in 1:spe){
        #subset positions to be appropriate to this matrix
        sp.start = 1 + sp.offset * (sp - 1) #1 is the starting place when you are on the first species, then you add the offset.  It is sp - 1 because there is no offset for the first species
        sp.end = sp.offset * sp #Go through to last record for the species
        sp.pos = positions[sp.start:sp.end]  #Get the positions that correspond to each species
        
        #Create a new matrix based on the template
        B1 = B1.template #B1.template serves as a template (duh)
        B2 = B2.template
        
        #Assign values to the B1 matrix, then move the appropriate values to B2, then remove them from B1.
        B1[sp.pos] = vr.vals[sp.start:sp.end]        
        B2.vals = B1[adj.pos] #Get a vector of positions from the B1 matrix that actually belong in the B2 matrix
        B1[adj.pos] = rep(0,length(adj.pos)) #Change the vital rates that belong in the B2 matrix to 0's in the B1 matrix
        B2[adj.pos] = B2.vals #Assign the vital rates that belong in B2 to B2.

        #Put the matrices into appropriate matrix format for the vital rate code
        dim(B1)<-rep(S*p,2)
        dim(B2)<-rep(S*p,2)
    
        #Transpose matrices so that the correct vital rate is in the correct location        
        #message(B1)
        #message(typeof(B1))
        
        B1<-Matrix::t(B1)
        B2<-Matrix::t(B2)

        #Write the matrices to a list keyed to species number
        B1.lst = append(B1.lst,B1)
        B2.lst = append(B2.lst,B2)
        }
       
    ccdm.out = list(B1.lst,B2.lst)
    return(ccdm.out)
    }
  
#' Look up Dispersal Probabilities
#'
#' Function for looking up dispersal probabilities from a table
#'
#' @details The base probabilities will be adjusted for each cell based on the
#' edge type. In an absorbing edge type, probabilities beyond the landscape will
#' simply be dropped, and dispersers will be removed (e.g. dispersal off an island)
#' With a torus edge, dispersers wrap around to the other side of the landscape,
#' and the dispersal probabilities are adjusted to account for this wrap-around
#' effect.
#' @param p The number of cells in a square landscape
#' @param BaseProbabilities A set of base dispersal probabilities.
#' @param edge.type Either ABSORBING or TORUS. See details.
#'
disp.lookup.v2 = function(p, BaseProbabilities,edge.type){
    
    #So, we know cells positions relative to one another because the landscape is square.
    
    DispersalProbabilities = c()
    
    # Go through cells
    for (i in 1:p){
        
        #For each cell, figure out where its seeds will disperse to
        #This allows seeds to fall off the edge of the landscape
        if (edge.type == "ABSORBING"){
            out.probs = absorbing.edge(i,p,BaseProbabilities)
            }
        #This wraps seeds around.
          #Note: because of the way dispersal probabilities are calculated in an earlier step,
            #some really long dispersers can still disperse out of the landscape entirely (if they disperse > 50 cells)
            #This is to maintain computational efficiency, and is off-set by potential invasion from outside)
        if (edge.type == "TORUS"){
            out.probs = torus.edge(i,p,BaseProbabilities)
            }

        #Add the calculated dispersal probabilities to the overall vector        
        DispersalProbabilities = c(DispersalProbabilities,out.probs)    
        }
    
    DispersalProbabilities = as.numeric(as.character(DispersalProbabilities))
     
    return(DispersalProbabilities)
    }

#DD# see if you want to merge the documentation with this with Torus edge.

#' Absorbing edge
#'
#' Implement the dispersal probabilities, with species able to fall off the edge
#' of the world
#' @param i The location of the specific cell
#' @param p The number of cells in a square landscape
#' @param BaseProbabilities Base dispersal probabilities 
absorbing.edge = function(i,p, BaseProbabilities){

    n = sqrt(p) #Get an index for the horizontal landscape
    m = sqrt(p) #Get an index for the vertical size of landscape (equals n, but done separately to keep the concepts clearer in my head)

    #Get x,y for each cell
    #start with i - 1
    #Divide by m to get row - 1 (+1 gives row position)
    #remainder + 1 gives column position

    #calculate location of i
    i.cpos = ( (i - 1) %% m) + 1
    i.rpos = ( (i - 1) %/% m) + 1

    # Tested by  p = 25, i = 1, i = 25, i = 6, results visually checked for accuracy

    c.disp = c() #Get displacement in columns
    r.disp = c() #Get displacement in rows

    #**# Do you need a shortcut for truncating, if all options are 0 after a certain point?
      #Just do it complete for now, and optimize later

    #Switching to row/column notation, because x,y was confusing me (the notation is opposite that for row/columns - x would correspond to columns, and would be listed second)
    for (j in 0:(p-1)){
        #0:(p-1) makes it start with 0, and that makes the math easier.
        cpos = (j %% m) + 1
        rpos = (j %/% n) + 1

        #cat(sprintf("(%s,%s)",rpos,cpos)) # Checked with a print statement for accuracy

        #Calculate displacement from cell of interest   
        c.disp.j = cpos - i.cpos
        r.disp.j = rpos - i.rpos #NOTE: This would make down a positive direction.  This may matter if adding directional dispersal.

        #Add to vector in character format, so it can be used as a key for looking something up in the table
        c.disp = c(c.disp,as.character(c.disp.j))
        r.disp = c(r.disp,as.character(r.disp.j))
        
        }

    #Look up dispersal probability    
    d.prob = mapply(absorbing.lookup,r.disp,c.disp,MoreArgs = list(BaseProbabilities))

    return(d.prob)
    }

#DD# See if you want to merge the documentation for this with the Torus edge lookup

#' Lookup function, absorbing edge
#'
#' Look up the base probabilities when seeds can fall off the edge of the world
#' @param r.val Row value
#' @param c.val Column value
#' @param BaseProbabilities Base dispersal probabilities
absorbing.lookup = function(r.val,c.val,BaseProbabilities){
    
    #BaseProbabilities needs to be reformated to come in as a nested list
    if ( !is.null( BaseProbabilities[[r.val]][[c.val]] ) ){
        prob.out = BaseProbabilities[[r.val]][[c.val]]
    }else {
        prob.out = 0
        }
    
    return(prob.out)
    }


#DD# consider merging documentation with absorbing edge

#' Torus Edge
#'
#' Implement dispersal probabilities,
#' where individuals wrap around to the other edge of the world
#' @param i The location of the specific cell
#' @param p The number of cells in a square landscape
#' @param BaseProbabilities Base dispersal probabilities 
torus.edge = function(i,p,BaseProbabilities){

    d.prob = rep(0,p) #Set each cell to 0 by default

    #Go through list of displacements
    r.disps = names(BaseProbabilities) #Names gets list of keys for accessing the list

    #Iterate through main keys
    for (k in 1:length(r.disps)){
        r.disp = r.disps[[k]] #Set the row dispersal
        
        sublist = BaseProbabilities[[r.disp]]
        c.disps = names(sublist) #Get keys in each sublist

        r.disp = as.numeric(as.character(r.disp)) #Convert to numeric format

        #Iterate through sublist
        for (kk in 1:length(c.disps)){
            c.disp = c.disps[kk]
            this.val = sublist[[c.disp]]
        
            c.disp = as.numeric(as.character(c.disp))
            this.val = as.numeric(as.character(this.val))
            
            #need to know relationship between the target cell and the other cells (do you?  Yes, and relationship to the edge)
            n = sqrt(p)  #get length of landscape
            m = sqrt(p)  #get height of landscape
    
            #calculate location of i
            i.cpos = ( (i - 1) %% n) + 1
            i.rpos = ( (i - 1) %/% m) + 1
            
            d.re = n - i.cpos  #get distance to right edge
            d.le = 1 - i.cpos  #get distance to left edge (this should be negative)
            d.te = 1 - i.rpos  #get distance to top edge (this should be negative)
            d.be = m - i.rpos  #get distance to bottom edge
    
            #Get displacement with wrap-around
            column.target = find.disp(i.cpos,c.disp,d.re,d.le,n)
            row.target = find.disp(i.rpos,r.disp,d.be,d.te,n)
            
            # This code spot tested with row/column comobinations from a 3 x 3 landscape and with a 5 x 5 landscape
            #convert row/column to the appropriate location in the vector
            c.part = column.target  #This adjusts where in the row the value is
            r.part = (row.target - 1) * n    #This adjusts the number according to the row.
            target.cell = c.part + r.part #Does this work?
            
            #print(d.prob)
            #print(d.prob[target.cell])
            #print(this.val)
            
            #Add dispersal to appropriate cell.
            d.prob[target.cell] = d.prob[target.cell] + this.val
            }
        }

    return(d.prob)
    }

#' Find Displacement
#' 
#' Function to find the displacement for a torus landscape, using wrap around in the landscape
#'
#' @param i.pos Position of the cell in the landscape
#' @param disp Total displacement to be applied
#' @param d.pe Distance to the positive edge (top or right)
#' @param d.ne Distance to the negative edge (bottom or left)
#' @param n Dimension of the landscape
#'
find.disp = function(i.pos,disp,d.pe,d.ne,n){

    #Test whether column displacement fits in landscape
    #if the displacement is to the right (or 0):
    if (disp >= 0){
        #check if it fits on the landscape
        if (disp <= d.pe){
            #Assign displacement column
            target = i.pos + disp

        }else{
            #Apply first wrap around & decrement c.disp
            disp = disp - d.pe #Reduce by distance needed for wrap around

            #While column displacement is larger than the landscape, continue to wrap around and decrement c.disp
            while(disp > n){
                #Decrement disp in proportion to one wrap around
                disp = disp - n
                }

            #Assign appropriate column displacement
            target = disp
            }
        }

    #if the displacement is to the left:
    if (disp < 0){
        #Check if it fits on the landscape
        if (disp >= d.ne){
            target = i.pos + disp #This will subtract from i.cpos, because c.disp is negative
        }else{
            #Apply first wrap around & decrement c.disp (which is actually an increment, because it's negative, and we want it to be more positive)
            disp = disp - d.ne #This is addition, because d.le is negative
            
            #While the displacement column is too large for the landscape, wrap around until it fits
            while(disp < -n){
                disp = disp + n
                }
        
            #Assign appropriate column displacement
            target = disp + n + 1 # the + n + 1 puts it on the right side of the landscape, and the 1 makes it so that a -1 assigns it to the edge of the actual landscape            
            }        
        }
    return(target)
    }
    
#' Look up Seed Dispersal Ratio
#'
#' Finds the seed dispersal ratio
#' @param value a value
#' @param vec1 a vector
#' @param vec2 a vector
#' @note This is the proportion of seeds dispersing to other cells
#' and is not actually a ratio (appears to be misnamed)
sdr.lookup = function(value,vec1,vec2){
    for (item in 1:length(vec1)){
        if (vec1[item] == value){
            seed.dispersal.ratio = 1 - vec2[item] #vec2[item] gives the probability of seeds remaining in the cell.  The seed dispersal ratio is the proportion of seeds dispersing to other cells.
            }
        }
    
    return(seed.dispersal.ratio)
    }

       
#' Get dispersal probabilities
#'
#' @param sp The species of interest (numeric)
#' @param SpTraits A dataframe holding information about the species
#' @param p the number of cells in the landscape
#' @param distances The intercell distances for every cell in the landscape
#' @param outpath The path where a dispersal probability table is, or should be written to.
#' @param num.sim The number of draws to use in simulating dispersal
#' @param edge.type The type of landscape edge (i.e. ABSORBING or TORUS)
#'
get.disp.prob = function(sp,SpTraits,p,distances,outpath,num.sim,edge.type){

    #Define a dispersal kernel for the species.
    #Here I need two parameters: mean dispersal distance and dispersal ratio which are stored in the data set
    sp.txt = sprintf("%s",sp) #convert species to text
    dispersal.function = as.num(SpTraits[sp.txt,"dispersalfunction"]) # Get the dispersal function to be used. #Currently this does not change by cell type

    par1   <- SpTraits[sp.txt, "disppar1"]  #For log-normal this is mean dispersal distance, in normal space cell units
    par2 = SpTraits[sp.txt,"disppar2"]  #For log-normal this is dispersal standard deviation, in log-cell units
    
    if (dispersal.function == 1 | dispersal.function == 2){            
        #Take the distances vector, and look up dispersal probabilities
        lookup.target = sprintf("%sfunct%s_mean_%s_sd_%s_Nsim_%.0f_Dispersal_Probabilities.csv",outpath,dispersal.function,par1,par2,num.sim)
    }else{
        lookup.target = sprintf("%sfunct%s_par1_%s_par2_%s_Nsim_%.0f_Dispersal_Probabilities.csv",outpath,dispersal.function,par1,par2,num.sim)
        }
    
    #If a dispersal probabilities table does not exist, create one
    if (!file.exists(lookup.target)){
        out.message = sprintf('Creating dispersal probabilities table with %s simulations.\nPlease note that this may take some time (e.g., hours for the default of 1,000,000 simulations)\nIf you believe the dispersal tables already exist, please abort and check the path to the dispersal tables.\nTarget path is %s',num.sim,outpath)
        message(out.message)
        flush.console()
        make.disp.prob.v2(dispersal.function,par1,par2,outpath, num.recs = num.sim)
        }
    
    #Read in base probabilities from table
    bprobs = read.table(lookup.target, sep = ",", header = T) 

    #Convert probabilities table to a nested list for easier lookup
      #**# this approach may be non-optimal for speed
    BaseProbabilities = list()
    for (a.row in 1:nrow(bprobs)){
        
        test.x = as.character(bprobs[a.row , 1]) #I think bprobs should be a matrix so this should pull the appropriate row
        test.y = as.character(bprobs[a.row , 2]) #Will pull the vertical displacement
        d.val = as.character(bprobs[a.row, 3]) #pulls up the value
                    
        #check if the row index has already been added
        if (!is.null(BaseProbabilities[[test.x]])){
            #Add a new y value to the list
            this.val = BaseProbabilities[[test.x]]
            #Check that a y value does not already exist (it never should!)
            if(!is.null(this.val[[test.y]])){
                print("Something went wrong in get.disp.prob, somehow there are multiple y values for the same x value in the base probabilities table")
                }
            this.val[[test.y]] = d.val
            BaseProbabilities[[test.x]] = this.val
            
            
        }else{
            new.list = list()  #Create a blank list to contain the information
            new.list[[test.y]] = d.val #Assign the value (this needs to be done separately, so that the list element is not called "test.y" and instead uses the value from test.y!
            BaseProbabilities[[test.x]] = new.list #Assign the value to a sub-list under x. #Do not say list here, or you get too many lists!
            
            }            
        }
            
    #Match the dispersal probabilities to the correct base probability
    DispersalProbabilities = disp.lookup.v2(p,BaseProbabilities,edge.type)

    #Find where distance = 0, use that for calculating seed dispersal ratio
    SeedDispersalRatio = sdr.lookup(0,distances,DispersalProbabilities)

    #NOTE: Is this step necessary? or simply undone by the next line of code?
    DispersalProbabilities<-Matrix(DispersalProbabilities, p, p)  #byrow = T
    
    dim(DispersalProbabilities)<-c(1, p^2)
    DispersalProbabilities<-as(DispersalProbabilities, "sparseMatrix")

    cdispmat.out = list(DispersalProbabilities,SeedDispersalRatio)
    return(cdispmat.out)    
    }

#' Generate dispersal probabilities
#'
#' Calculates dispersal probabilities (point to area) via simulation based on
#' It creates a table with dispersal probabilities to different cells compatible with the SpatialDemography model.
#' an input mean dispersal distance (not log transformed) and dispersal standard deviation (log-transformed).
#'
#' @details Point to area dispersal assumes that dispersers originate in the center of
#' the cell, and determines dispersal probabilities using the entire area of
#' potential destination cells. (see Chipperfield et al. 2011 for validation of
#' this approach). Unlike Chipperfield et al. 2011, this function is not based
#' on integrals, but uses a simulation approach to approximate the dispersal
#' probabilities.(not as mathematically sophisticated, but much simpler to
#' program, and with large enough sample sizes, the differences are minimal.)
#'
#' @references Chipperfield, J.D., E.P. Holland, C. Dytham, C.D. Thomas,
#' T. Hovestadt. 2011.  On the approximation of continuous dispersal kernels in
#' discrete-space models.  Methods in Ecology and Evolution 2: 668-681.
#' @author Alexander "Sasha" Keyel
#' @param funct Chose a function for modeling dispersal (e.g., log-normal).
#' Function codes are 1: log-normal, 2: normal, 3: constant, 4: uniform,
#' 5: uniform, rounded to nearest integer. 
#' @param par1 First function parameter.  For log-normal, this should be the
#' mean dispersal distance in cell units.  For example, if mean dispersal for a
#' species was 25 m, and you intend to use a 50 m cell size, you would enter
#' 25 / 50 = 0.5 here.  Note that this is NOT log-transformed
#' @param par2 Second function parameter.  For log-normal, this is the dispersal
#' standard deviation in log-transformed  cell units.
#' @param outpath outpath specifies where to put the newly generated dispersal
#' probability table
#' @param num.recs The number of draws used in the simulation.  Ideally this
#' will be a large number (e.g., 1,000,000), but the function will run slowly
#' for numbers >10,000.
#' @param max.disp.extent The maximum dispersal distance allowed for calculation
#' of probabilities.  With the log-normal function and a high enough standard
#' deviation, you can get VERY long distance dispersal.  This option sets a
#' cut-point to improve computational efficiency.
#' @param cut.off The minimum allowed dispersal probability.  Probabilities
#' less than this value will be rounded to 0.  This is intended to improve
#' computational efficiency.
#' @return No value is returned, the function is run for its side-effects
#' (creation of a dispersal probability table.)
#' @note WARNING: If dispersers disperse out of the landscape extent (set by
#' max.disp.extent), they will still disperse out of the landscape, regardless
#' of whether a torus or absorbing edge is used.
#' Only tested for log-normal distribution so far.  Also, it may be faster
#' to use a GIS implementation for this process.
#' @export make.disp.prob.v2
make.disp.prob.v2 = function(funct,par1,par2,outpath,num.recs = 100000,max.disp.extent = 101,cut.off = 0.00001){
    
    #for log-normal, par 1 should be mean.disp.cell and par2 should be ln.sd
    
   #As of 2014-07-08 dispersal needs to be input in terms of cell distances, and not m. 
   ##Convert distances to number of cells
   # mean.disp.cell = mean.disp / cell.size

    dist.lst = list()

    #Add a label, because par1 may be log-transformed later, but the un-logtransformed name is used for the title
    par1.lbl = par1
    
    ##Convert par1 to logarithmic scale for use in log-normal distribution
    if (funct == 1){
        par1 = log(par1)
        }
    
    #Start by drawing distances and angles    
    dist.vals = compute.funct(num.recs,funct,par1,par2,to.round = NA)
    angle.vals = runif(num.recs,0,2*pi)
    
    #Convert the distance and angle to x,y coordinates
    x.vals = mapply(get.x,dist.vals,angle.vals) #mapply uses multiple vectors as an input to the function.  
    y.vals = mapply(get.y,dist.vals,angle.vals)
    
    #index cells relative to cell of origin
      #Cell coords are x,y from top right to top left going clockwise
    origin.cell = c(0.5,0.5,0.5,-0.5,-0.5,-0.5,-0.5,0.5)

    #Loop through each cell in the extent
    l.start = -max.disp.extent/2
    l.end = max.disp.extent/2
    
    #Set up vectors to hold output values
    xmp.vec = c()
    ymp.vec = c()
    p.vec = c()
            
    counter = 0
    for (x.right.coord in (l.start + 1):l.end){
        for (y.top.coord in (l.start + 1):l.end){
            counter = counter + 1
        
            x.left.coord = x.right.coord - 1
            y.bottom.coord = y.top.coord - 1
                                    
            #Get shorter variable names & repeat so that mapply function works properly
            xrc = rep(x.right.coord,length(x.vals))
            xlc = rep(x.left.coord,length(x.vals))
            ytc = rep(y.top.coord,length(y.vals))
            ybc = rep(y.bottom.coord,length(y.vals))
            
            #Count the number of points falling in this cell
            point.vec = mapply(check.coords,x.vals,y.vals,xlc,xrc,ybc,ytc)
            num.points = sum(point.vec)

            #Divide by the total number generated to get dispersal probabilities
            disp.prob = num.points / num.recs
            
            #Convert dispersal probabilities < 10^-4 (or whatever the cut-off is set to) to 0 to simplify matrix calculations (sensu Jakob.  This only makes sense if surviving seed number is much less than 10^4)
            if (disp.prob < cut.off){
                disp.prob = 0
                }
                            
            #Get cell midpoint for distance calculation
            xmp = mean(c(x.right.coord,x.left.coord))
            ymp = mean(c(y.top.coord,y.bottom.coord))
            
            #To save on computation, do not output 0 values
            if (disp.prob != 0){
            
                #Add values to vectors
                xmp.vec = c(xmp.vec,xmp)
                ymp.vec = c(ymp.vec,ymp)
                p.vec = c(p.vec, disp.prob)
                }                
            }
        }

    #Set up file & header (label with mean and standard deviation for log-normal & normal, otherwise with par1 and par2
    if (funct == 1 | funct == 2){
        this.csv = sprintf("%sfunct%s_mean_%s_sd_%s_Nsim_%.0f_Dispersal_Probabilities.csv",outpath,funct,par1.lbl,par2,num.recs)
        fileheader = sprintf("X_displacement,Y_displacement,Dispersal_Probability_mean_%s_sd_%s\n",par1.lbl,par2)

    }else{
        this.csv = sprintf("%sfunct%s_par1_%s_par2_%s_Nsim_%.0f_Dispersal_Probabilities.csv",outpath,funct,par1,par2,num.recs)        
        fileheader = sprintf("X_displacement,Y_displacement,Dispersal_Probability_par1_%s_par2_%s\n",par1,par2)
        }

    #Write header with mean and sd to a table  
    cat(fileheader, file = this.csv)

    probs = matrix(c(xmp.vec,ymp.vec,p.vec),ncol = 3)
    
    write.table(probs, file = this.csv,append = T, row.names = F, col.names = F, sep = ",")

    }

#Function for getting an x coordinate from angle & distance #Used by make.disp.prob, originally from d2dhelper.r
#' Get x/y coordinate
#' 
#' Functions for getting an x (or y) coordinate from angle & distance
#'
#' @param a.distance The input distance
#' @param an.angle The input angle
#' @param angle.units The units for the angle. Must be set to "RADIANS".
#' 
get.x = function(a.distance,an.angle,angle.units = "RADIANS"){
    
    if (angle.units == "RADIANS"){
        if (an.angle >= 0 & an.angle < (pi/2)){
            out.quad = 1
            x = a.distance * cos(pi/2 - an.angle)
            }
            
        if (an.angle >= (pi/2) & an.angle < (pi)){
            out.quad = 2
            x = a.distance *  cos(an.angle - pi/2)
            }
            
        if (an.angle >= pi & an.angle < ((3*pi)/2)){
            out.quad = 3
            x = a.distance * -cos(3*pi/2 - an.angle)
            }
            
        if (an.angle >= ((3*pi)/2) & an.angle < 2*pi){
            out.quad = 4
            x = a.distance * - cos(an.angle - 3*pi/2)
            }
        }
    
    
    return(x)
    }

#function for getting a y coordinate from angle and distance #Used by make.disp.prob, originally from d2dhelper.r
#DD# Figure out how to dispaly with the get.x documentation!
get.y = function(a.distance,an.angle,angle.units = "RADIANS"){
    
    if (angle.units == "RADIANS"){
        if (an.angle >= 0 & an.angle < (pi/2)){
            out.quad = 1
            y = a.distance * sin(pi/2 - an.angle)
            }
            
        if (an.angle >= (pi/2) & an.angle < (pi)){
            out.quad = 2
            y = a.distance * -sin(an.angle - pi/2)
            }
            
        if (an.angle >= pi & an.angle < ((3*pi)/2)){
            out.quad = 3
            y = a.distance * -sin(3*pi/2 - an.angle)
            }
            
        if (an.angle >= ((3*pi)/2) & an.angle < 2*pi){
            out.quad = 4
            y = a.distance * sin(an.angle - 3*pi/2)
            }
        }
    
    
    return(y)
    }

#' Check coordinates
#'
#' Checks if coordinates are within a cell.
#'
#' @param x.val The X coordinatea
#' @param y.val The Y coordinate
#' @param x.lower.bound The lower x coordinate of the cell
#' @param x.upper.bound The upper x coordinate of the cell
#' @param y.lower.bound The lower y coordinate of the cell
#' @param y.upper.bound The upper y coordiante of the cell
#' @note Used by make.disp.prob.v2
#' @note WARNING Points that fall exactly on a cell's border are dropped.
#' While this is bad, this is expected to be relatively minor,
#' as the odds of getting a number that lands exactly on a cell's border is
#' pretty low, given the number of decimal places.
#'
check.coords = function(x.val,y.val,x.lower.bound,x.upper.bound,y.lower.bound,y.upper.bound){

    in.cell = 0
    if (x.val > x.lower.bound & x.val < x.upper.bound){
        if (y.val > y.lower.bound & y.val < y.upper.bound){
            in.cell = 1
            }
        }
    
    return(in.cell)
    }

#' Create the composite dispersal matrix
#'
#' Create a composite dispersal matrix for the dispersal portion of the model
#'
#' This matrix consists of S*S submatrices of dimension p x p describing the
#' dispersal probability between patches:
#' Where p is the number of patches, S is the number of stages (here 4).
#' Only diagonal submatrixes, which describe spatial movement of individuals of
#' a given stage between patches, can contain non-zero matrix entries.
#' We assume that only individuals of stage 1 (mobile seeds) move between
#' patches, therefore only the submatrix M1<-1 contains non-zero off-diagonal
#' matrix entries!
#' The off-diagonal entries of M1<-1 describe the probability of seed movement
#' between patches of the landscape.
#' The diagonal entries describe the probability that mobile seeds stay in the
#' local patch, which is 1-u (seed dispersal ratio).
#' All other diagonal submatrices are identity matrices (ones in the diagonal)
#' as all individuals of these stages stay put.
#' 0<-0   O     O     O
#' O     1<-1   O     O
#' O     O    2<-2    O
#' O 	   O      O   3<-3
#'
#' @param DispersalProbabilities The dispersal probabilities from each cell to
#' every other cell in the landscape
#' @param p Number of cells in the landscape
#' @param S Number of life-stages in the model (currently needs to be 4)
#' @param M_sub A template matrix of the appropriate dimensions to fill in.
#' @return returns a dispersal matrix for controlling seed dispersal.
#' @author Jakob Gerstenlauer and Alexander "Sasha" Keyel
#' @references Hunter, C.M. & Caswell, H. (2005). The use of the vec-permutation
#' matrix in spatial matrix population models.
#' Ecological modelling, 188, 15-21.
#' @note WARNING While S is an input, this function will ONLY work for S = 4.
#'
CreateCompositeDispersalMatrix = function(DispersalProbabilities,p,S,M_sub){
    #create a sparse matrix with dimensions 1 and dim (a vector!) for the dispersal matrix M
    Dim<-S*S*p^2
    M <- sparseMatrix(dims = c(1,Dim), i={}, j={},x=rep(0.0,Dim))
        
    #Calculate the indices which have to be added for all p^2 indices of M_ii
    index1 <- seq(1,p * p);
    xpos <- (index1 - 1) - ((index1 -1) %/% p) * p   
    ypos  <- (index1 - 1) %/% p;
    index2 <- xpos + ypos * S * p
    
    #assign 1<-1 submatrix (i=1)
    #z<-1+(1-1)*(S*p^2+p)
    z <- 1
    M[z+index2] <- DispersalProbabilities[index1]
    
    #SS# This would break if you changed the number of stages in the code right now.
    
    #assign 2<-2 submatrix (i=2)
    z <- 1 + S*p^2 + p
    M[z+index2] <- M_sub[index1]
    
    #assign 3<-3 submatrix (i=3)
    z <- 1 + 2 * (S*p^2+p)
    M[z+index2] <- M_sub[index1]
    
    #assign 4<-4 submatrix (i=4)
    z <- 1 + 3 * (S*p^2+p)
    M[z+index2] <- M_sub[index1]

    dim(M)<-rep(S*p,2)
        
    #check the resulting matrix
    #image(M)
    
    return(M)
    }

#' Write table for timing function
#' 
#' Simple version to get write.table to behave properly
#' @param in.vec the vector to be written to table
#' @param timefile the file to write to.
#' @param do.append Whether or not to append to an existing file
#'
wrt = function(in.vec, timefile, do.append = TRUE){
    in.vec = matrix(in.vec, nrow = 1)
    write.table(in.vec, file = timefile, sep = ",", row.names = FALSE, col.names = F, append = do.append)
    }

#' Get model timing
#'
#' This function takes a start time, and different run times,
#' and outputs the timing to a file.  The purpose is to allow one to see 
#' how long different model processes take, and prioritize sections of code for
#' optimization or for implementation in C++.
#'
#' @param start.time The starting time for the model run
#' @param run.times A list of intermediate times during the model run
#' @param run.lbl A list of labels for each of the intermediate times
#' @param timefile The file (including path) to be created
#' @param write.timefile An indicator for whether or not to write to file. If 0, no file will be output, if 1 only the total model time will be output, if 2 all timing will be output.
#' @param Model.Name The model being timed
#' @return Nothing is returned, function is run to create a file with run times.
#' @note run.times and run.lbl need to be the same length and in the same order,
#' or you will get corrupted/strange results!
#'
GetTimes = function(start.time,run.times,run.lbl,timefile, write.timefile, Model.Name){

    #Note: this function changed on 2015-02-05.
    
    #if (write.timefile == 0){ # Do nothing }
        
    if (write.timefile == 1){
        if (!file.exists(timefile)){
            wrt(c('Model','ElapsedTime'),timefile, FALSE)
            }
        elapsed.time = run.times[length(run.times)] - start.time #last item minus first item
        wrt(c(Model.Name,elapsed.time),timefile)
        } 
     
    if (write.timefile == 2){         
        total.times = run.times - start.time
        elapsed.times = c()
        prev.item = start.time
        for (i in 1:length(run.times)){
            item = run.times[i]
            item.time = item - prev.item
            elapsed.times = c(elapsed.times, item.time)
            prev.item = item
            }
  
        #Set up model run header
        out.lbl = c("Model.Name","Model.Time",run.lbl)
        wrt(out.lbl,timefile)

        #Set up elapsed time output
        et.hdr = sprintf("%s_elapsed", Model.Name)
        elapsed.time = run.times[length(run.times)] - start.time #last item minus first item
        elapsed.times = c(et.hdr, elapsed.time, elapsed.times)
        wrt(elapsed.times,timefile)

        #Set up total time output
        tot.hdr = sprintf("%s_total", Model.Name)
        tot.time = run.times[length(run.times)] - start.time
        total.times = c(tot.hdr, tot.time, total.times)
        wrt(total.times,timefile)        
        }
    }

    
#' Set up initial population sizes
#'
#' Set up initial population sizes in the model
#'
#' @param n0.lst A list of initial population sizes for each species (will be empty on input, will be returned with values)
#' @param spe The total number of species in the species pool
#' @param species.locs A list of which cells are occupied by each species
#' @param S The number of life stages
#' @param n.seed The number of seeds to assign to each occupied cell
#' @param n.juv The number of juveniles to assign to each occupied cell
#' @param n.adult The number of adults to assign to each occupied cell
OccSetup = function(n0.lst,spe,species.locs,S,n.seed,n.juv,n.adult){

    #Loop through each species (#**# Is this the most efficient approach?  Seems reasonable)
    for (sp in 1:spe){

        #Translate species.locs to Occupied patches for each species
        to.find = sprintf("\\b%s\\b",sp) #\b looks for boundaries - so this makes sure only the search term matches, and not larger numbers that include the search term.  The \ needs a \ in order for R to interpret it as a \, and not as something else (i.e., an escape character). Otherwise a search for species 1 will return wrong matches, e.g., 41 or 14!
        OccupiedPatches = grep(to.find,species.locs) #This gives the location in the list, not in the specific vector in the list - which is exactly what I want.
        
        #If length == 0, there are no occupied patches, and no assignment is necessary
        if (length(OccupiedPatches > 0)){
        
            #Assign number of individuals to n0.lst for each species
            #Note - no point in assigning mobile seeds - the first transition matrix will kill them all.
            adult.Indices <- OccupiedPatches * S
            juv.Indices <- OccupiedPatches * S - 1
            seed.Indices <- OccupiedPatches * S - 2
            
            
            n0.lst[[sp]][seed.Indices]<- n.seed[sp]
            n0.lst[[sp]][juv.Indices]<- n.juv[sp]
            n0.lst[[sp]][adult.Indices]<- n.adult[sp]
            }
        }
    
    return(n0.lst)
    }

#' Implement invasion in MetaComeSE model
#'
#' Manage invasion in the model.  A certain number of species (num.invaders) are
#' chosen to invade a certain number of cells in the landscape (cells.to.invade)
#' at a particular propagule pressure (related to the species' reproductive
#' rates).
#'
#' @param nt1a.lst A list giving the locations of each species in each cell
#' @param spe The number of species in the regional species pool
#' @param p The number of cells in the landscape
#' @param S The number of stages in the population model (= 4)
#' @param SpTraits A dataframe containing the species traits
#' @param num.invaders The number of species to have invade the landscape
#' @param cells.to.invade The number of cells in the landscape to be invaded
#' @param repro.proportion The proportion of a species' normal reproduction to
#' assign to the landscape.  E.g., if a species normally makes 1000 seeds, and
#' repro.proportion is set to 0.5, 500 seeds will be assigned to each invaded
#' cell.  Similarly, a repro.proportion of 1.5 would lead to 1500 seeds.
#' @return An updated n0.lst to include the invaders
#'
do.invasion = function(nt1a.lst,spe,p,S,SpTraits,num.invaders = 1,cells.to.invade = 5,repro.proportion = 1){

    # For testing purposes
    #spe = 100
    #num.invaders = 1
    #cells.to.invade = 5
    #repro.proportion  = 1 #proportion of one individual's normal reproduction to assign to the invaded cell 

    #Draw [num.invaders] species to invade
    invaders = sample(spe,num.invaders)
    
    #For each invader...
    for (invader in invaders){
        #Select [cells.to.invade] cells for each species to invade
        invaded.cells = sample(p,cells.to.invade)
        
        #Assign Z seeds to invade each cell (numbers proportional to the species seed production in optimal habitat)
        sp.txt = sprintf("%s",invader)
        n.new.seeds = SpTraits[sp.txt,"p30"] * repro.proportion * SpTraits[sp.txt, "p01"]  #Take normal fecundity * proportion of reproduction * proportion of mobile seeds that survive to get number of invaders
        seed.Indices <- invaded.cells * S - 3 #Goal is to index which values in nt1a.lst (approach copied from OccSetup, could also be applied to juveniles and adults, if invasion at these stages is preferred!)
        n.old.seeds = nt1a.lst[[invader]][seed.Indices]
        nt1a.lst[[invader]][seed.Indices]<- n.new.seeds + n.old.seeds #Some cells may be already colonized, in this case just increase the number of seeds present.
        }
    
    return(nt1a.lst)
    }

#' Implement seed competition
#'
#' Idea is that seeds will compete for microsites. Currently three types of competition possible:
#' 0 = no competition, 1 = within-species K (excess seeds die), 2 = deterministic
#' multi-species K (seeds are assigned to microsites proportional to the number of seeds
#' of each species), and 3 = stochastic multi-species K (seeds are assigned to microsites
#' via a lottery approach, where each seed has a chance to occupy a microsite.
#' NOTE: Seeds already in the seedbank occupy microsites, juveniles and adults do not.
#' WARNING: My implementation of this may be computationally intensive for large numbers of seeds!
#'
#' @param competitiontype The type of competition (see description)
#' @param microsites.vec A vector giving the number of microsites in each cell.
#' @param nt1a.lst A list giving the abundances of each of the four stages for each species
#' @param spe The number of species in the regional species pool
#' @param ISeedBank An index for which entries for each species in nt1a.lst correspond to stage 1 individuals.
#' @param ISeedlings An index for which entries for each species in nt1a.lst correspond to stage 0 individuals.
#' @param p Number of cells in the landscape
#'
do.competition = function(competitiontype, microsites.vec, nt1a.lst, spe, ISeedBank, ISeedlings, p){
        
    #Within-species competition scenario
    if (competitiontype == 1){
        for (sp in 1:spe){
            #Get number of seeds already in the seedbank, subtract from overall carrying capacity (those microsites are already full)
            new.microsites = microsites.vec - nt1a.lst[[sp]][ISeedBank]
            
            #Prevent negative numbers of new microsites!
            new.microsites = ifelse(new.microsites < 0, 0, new.microsites)
    
            #Reduce number of mobile seeds to the carrying capacity for the cell.  Mobile seeds includes seeds dispersing to the same cell (but not already present in the seedbank)
            nt1a.lst[[sp]][ISeedlings]<-ifelse(nt1a.lst[[sp]][ISeedlings] > new.microsites, new.microsites, nt1a.lst[[sp]][ISeedlings])
            }
        }
    
    #Across-species competition scenario
    if (competitiontype == 2){
        
        #Compute number of available microsites
        ms.out = compute.microsites(microsites.vec,nt1a.lst, spe, ISeedBank, ISeedlings,p)
          new.microsites = ms.out[[1]]
          TotalMobileSeeds = ms.out[[2]]
        
        # Reduce the number of mobile seeds for each species to its share of the carrying capacity
        for (sp in 1:spe){
            #Divide the number of seeds of this species by the total number of seeds from all species
            sp.proportion = nt1a.lst[[sp]][ISeedlings] / TotalMobileSeeds
            #Calculate the number of spots assigned to this species.  May want to put restrictions on how small of a decimal it can be.
            spots = sp.proportion * new.microsites
            nt1a.lst[[sp]][ISeedlings] <-ifelse(nt1a.lst[[sp]][ISeedlings] > spots, spots, nt1a.lst[[sp]][ISeedlings])
            }
        }
    
    #Lottery approach to microsites (stochastic version of type 2)
    if (competitiontype == 3){
        #Compute number of available microsites
        ms.out = compute.microsites(microsites.vec,nt1a.lst, spe, ISeedBank, ISeedlings,p)
          new.microsites = ms.out[[1]]
          TotalMobileSeeds = ms.out[[2]] #Note: not actually needed for this competition type!
        
        #Create a list to contain seed vectors
        cells.lst = rep(list(NA),p)
        
        #Create vectors labeled by species of all seeds for each cell
        for (sp in 1:spe){
            sp.cell.vec = nt1a.lst[[sp]][ISeedlings]
            
            #Loop through and extract from cells
            for (cell in 1:p){
                this.sp.val = sp.cell.vec[cell]
                #NOTE: This step drops fractional seeds!
                sp.vals = rep(sp, this.sp.val)
                
                #Need special behavior to avoid adding an NA on the first time.
                if (cell == 1 & sp == 1){
                    old.vals = c() #Note: this will come up as NULL
                }else{
                    old.vals = cells.lst[[cell]] 
                    }
                new.vals = c(old.vals, sp.vals)
                cells.lst[[cell]] = new.vals
                }
            }

        #Create a vector to hold number of mobile seeds per cell
        base.spots = rep(0,p)
        # Use the template vector for each species
        spots = rep(list(base.spots),spe)
        
        #Sample the requisite number of seeds from the vector
        for (i in 1:length(new.microsites)){            
            sites = new.microsites[i]
            pool = cells.lst[[i]]

            #If there are fewer seeds than microsites, all seeds find microsites
            if (length(pool) <= sites){
                winners = pool
            #otherwise, sample from the seed pool
            }else{
                winners = sample.fixed(pool, sites)
                }
            
            #Get counts of # seeds per cell as a vector
            winners.table = table(winners)
            
            #Update the spots list for each species
            for (rec in names(winners.table)){
                sp = as.num(rec)
                spots[[sp]][i] = winners.table[[rec]] #Fill in the appropriate cell value with the entry matching the table.
                # This should be more efficient than looping over species, as only records in the table are used.
                # Note that the default value for each species and cell is 0, so this approach should work well.
                # However, optimization here will likely be necessary for larger models or large numbers of seeds!
                }
            }
        
        #Assign to nt1a.lst
        for (sp in 1:spe){
            nt1a.lst[[sp]][ISeedlings] = spots[[sp]]
            }        
        }
    
    #Unequal across species competition #This could also be very cool!
    #if (competitiontype == X{USE YOUR IMAGINATION!} 
    
    #Might even be able to make a selective species competition scenario.  This could get cool! (e.g., if species A can establish if species B is at a microsite, but species B can't establish if A is present at the microsite #CAN YOU THINK OF ANY REAL EXAMPLES OF THIS?
    return(nt1a.lst)
    }

#' Compute number of available microsites
#'
#' Helper function to calculate number of available microsites
#'
#' @param microsites.vec A vector giving the number of microsites in each cell.
#' @param nt1a.lst A list giving the abundances of each of the four stages for each species
#' @param spe The number of species in the regional species pool
#' @param ISeedBank An index for which entries for each species in nt1a.lst correspond to stage 1 individuals.
#' @param ISeedlings An index for which entries for each species in nt1a.lst correspond to stage 0 individuals.
#' @param p Number of cells in the landscape
#'
compute.microsites = function(microsites.vec,nt1a.lst, spe, ISeedBank, ISeedlings,p){

    #Get total number of seeds already in the seedbank
    SeedsinSeedbank = rep(0,p) #Create a separate entry for each cell
    TotalMobileSeeds = rep(0,p) #Separate entry for each cell
    for (sp in 1:spe){
        SeedsinSeedbank = SeedsinSeedbank + nt1a.lst[[sp]][ISeedBank]
        TotalMobileSeeds = TotalMobileSeeds + nt1a.lst[[sp]][ISeedlings]
        }
    #Subtract occupied microsites from overall carrying capacity
    new.microsites = microsites.vec - SeedsinSeedbank
    
    #Prevent negative numbers of new microsites!
    new.microsites = ifelse(new.microsites < 0, 0, new.microsites)

    return(list(new.microsites, TotalMobileSeeds))
    }

#' Calculate model functional diversity
#'
#' This function calculates functional diversity at the end of the model
#' simulation and updates the Results dataframe.
#'
#' @param Results The results file to be updated
#' @param SpTraits A dataframe containing the Species Trait values
#' @param abund.mat A matrix giving abundances for each species and cell
#' @param spe The total number of species in the regional species pool
#' @param resolution The number of decimal places to round functional traits
#' @param log.trans Whether or not to log-transform functional traits (this may be broken)
#' @param sv An indicator parameter
#' @param num.tp An indicator to assist in updating the correct row of the Results
#' @note The intention is to add an option to allow this to be calculated at
#' intermediate model steps
#'
calc.fd = function(Results,SpTraits,abund.mat,spe,resolution,log.trans, sv, num.tp){ 

    SP.mod = SpTraits
    SP.mod$is.error = NULL #Drop is.error column. NOTE: can drop this code, because the column is already gone

    #Convert character fields (Response traits) to factor fields to avoid crashing the code later
    for (a.field in colnames(SP.mod)){
        if (typeof(SP.mod[[a.field]]) == "character"){
            SP.mod[[a.field]] = as.factor(SP.mod[[a.field]])
            }
        }
        
    #NOTE: I should probably should drop the na columns regardless.
    if (log.trans != 0){ 
        na.zero = apply(SP.mod,2,function(.col){all(is.na(.col))  || all(.col == 0)})
        SP.mod = SP.mod[ ,!na.zero]
        }
    
    #Do the actual calculations
    Results = getfd(Results,SP.mod,abund.mat,resolution,log.trans,sv, num.tp)
        
    return(Results)
    }
                  
#' Calculate functional diversity (helper function to calc.fd)
#' 
#' Calculate functional diversity for calc.fd & place values in Results dataframe
#' Choose what metrics to calculate based on what is set up in Results.
#' Calculate appropriate functional trait diversity
#' Calculate for both functional traits & just for response traits
#'
#' @param Results The results file to be updated
#' @param SP.mod Species Traits, but with transformations and changes in format
#' @param abund.mat A matrix giving abundances for each species and cell
#' @param resolution The number of decimal places to round functional traits
#' @param log.trans Whether or not to log-transform functional traits (this may be broken)
#' @param sv An indicator parameter
#' @param num.tp An indicator to assist in updating the correct row of the Results
#'
getfd = function(Results,SP.mod,abund.mat, resolution,log.trans, sv, num.tp){

    #Get names from results
    res.nam = names(Results)

    #Calculate overall functional diversity & response trait diversity
    div.types = c("FTD","RTD")
    for (div.type in div.types){

        #Create a species matrix that can be modified without changing the original
        sp.mat = SP.mod
                
        #Drop non-response traits (i.e., in this example, those labeled "Max"
        if (div.type == "RTD"){
            drop.lst = c("p01","p02","p11","p12","p22","p23","p30","p32","p33","biomass.adult","biomass.juv","biomass.seed")
            #**# Could be optimized or done better.
            for (item in drop.lst){
                sp.mat[[item]] = NULL
                }
           
            }

        #Convert from dataframe to matrix (Note: as.matrix does not work here, because it makes everything character, due to fields that should be interpreted as factors

        sp.mat = data.matrix(sp.mat) #Note that neither matrix or as.matrix give appropriate results!

        td.to.calc = grep(div.type,res.nam,value = T) #Get any things in results identified as "FTD" functional trait diversity
        #s.term = sprintf("landscape.%s.", div.type)
        s.term = sprintf("%s.", div.type)
        td.to.calc = sub(s.term,"",td.to.calc) #Strip off prefixes
        td.to.calc = unique(td.to.calc) #Remove duplicates.
        
        #**# Consider improving this code: idea is to go through ftd.to.calc, and then calculate the appropriate metrics.  But need to avoid redundancy of calculations & do things sensibly
        did.mvfd = 0 #indicator variable for my multivariate measures
        did.dbfd = 0 #indicator variable for Laliberte package
        did.pgfd = 0 #indicator variable for Petchey & Gaston's 2002,2006 measure    
    
        for (item in td.to.calc){
            #**# This approach could be improved / optimized
            if (item == "UTC"){
                if (did.mvfd != 1){
                    if(!require(multirich)){ stop("Please install multirich: install.packages('multirich')") }
                    utc.out = multirich::mvfd(sp.mat,abund.mat,resolution = resolution, log.trans = log.trans, calc.ovr = 0) #**# Need to deal with resolution problem!
                    did.mvfd = 1
                    }
                    
                i.val = utc.out$utc[1] #initial UTC
                f.val = utc.out$utc[2] #final UTC
                }
    
            if (item == "PGFD"){
                if (did.pgfd != 1){
                    stop("PGFD has not yet been added as an option")
                    #**# script out petchey & Gaston measure
                    did.pgfd = 1
                    }
                }
    
            if (item == "FRIC"){
                if (did.dbfd != 1){
                    if(!require(FD)){ stop("Please install FD: install.package('FD')") }
                    fun.div = FD::dbFD(sp.mat,abund.mat,calc.FRic = T)
                    did.dbfd = 1
                    }
                }
            
            #Write to appropriate output in Results
            ii = (sv - 1) * num.tp + 1
            fi = (sv - 1) * num.tp + 2
            
            field = sprintf("%s.%s", div.type,item)
            Results[[field]][ii] = i.val
            Results[[field]][fi] = f.val
            
            #Write to appropriate output in Results.
            #i.term = sprintf("landscape.%s.%s.Initial",div.type,item) #Could make it so landscape and initial could be changed too!
            #f.term = sprintf("landscape.%s.%s.Final",div.type,item)
            #Results[[i.term]] = i.val 
            #Results[[f.term]] = f.val
            }
        }
    
    return(Results)
    }

#' Convert abundance to occupancy
#'
#' Abundances > 0 are coded as present (1),
#' abundances == 0 are coded as absent (0).
#'
#' @param x Input a number >= 0.
#' @return Outputs a recoded number: 0 (absent) or 1 (present).
#'
recode = function(x){
    if (x > 0){
        y = 1
        }
    if (x == 0){
        y = 0
        }
    if (x < 0){
        print("x < 0.  You have a problem, as there is no such thing as negative abundance!")
        y = NA
        }
        
    return(y)
    }

#' Target recode
#'
#' Recode such that all target values are 1 and all other values are 0
#' @param x An input number
#' @param target the target to be matched
#'
target.recode = function(x, target){
    y = 0
    if (x == target){ y = 1 }
    
    return(y)
    }

#' Drop fractional individuals
#'
#' Abundances >= 1 remain as they were
#' Abundances < 1 are recoded as 0
#' Negative inputs are not allowed.
#'
#' @param x A number >= 0 (corresponds to abundance)
#' @return Either the original number (if x >= 1) or 0 (x < 1).
#'
drop.partial = function(x){
    if (x >= 1){
        y = x
        }
    if (x < 1){
        y = 0
        }
    if (x < 0){
        print("x < 0.  You have a problem, as there is no such thing as negative abundance!")
        y = NA
        }
    return(y)
    }

#' Calculate output metrics
#' 
#' Calculates output metrics for the simulation.
#' E.g., species richness, beta diversity, biomass, and functional diversity
#'
#' @param Results A dataframe to contain the results
#' @param timepoint The timepoint (initial, final, etc.) for which to calculate the metrics
#' @param n0.lst A list of the abundances of each stage for each species
#' @param IAdults An index of which stages correspond to adults
#' @param spe The total number of species in the species pool
#' @param p The number of cells in the landscape
#' @param SpTraits A dataframe containing species traits
#'
calc.metrics = function(Results,timepoint,n0.lst,IAdults,spe,p,SpTraits){

    # Get adult abundances for the scenario, calculate species richness, alpha diversity, and biomass
    tot.occ = 0
    sp.richness = 0
    abund.vec = c()
    nam.vec = c()
    biomass = 0
    sp.extinct = 0
    for (sp in 1:spe){
        sp.abund = n0.lst[[sp]][IAdults] #n0.lst is the list of abundances.  sp indexes which species is considered, and [IAdults] is an index that gets only values for adults.
        sp.abund = sapply(sp.abund,drop.partial)

        #Set up species richness and alpha diversity
        sp.occ = sapply(sp.abund,recode)
        #If at least one adult of a species is present somewhere, increase species richness by 1.
        if (sum(sp.occ) > 0){
            sp.richness = sp.richness + 1
            }
        if (sum(sp.occ) == 0){
            sp.extinct = 1
            }
        tot.occ = tot.occ + sp.occ

        abund.vec = c(abund.vec,sum(sp.abund)) #sum of sp.abund because it is the abundance for every cell in the landscape.  Right now we're ignoring that level of diversity, because I'm feeling overwhelmed.
        nam.vec = c(nam.vec,sprintf("%s",Digit(sp,2)))
        sp.txt = sprintf("%s",sp)
        sp.biomass = sum(sp.abund) * SpTraits[sp.txt,"biomass.adult"] #Biomass = number of individuals * biomass of an adult individual
        biomass = biomass + sp.biomass
        }

    #Write species richness to results
    sp.r.lbl = sprintf("landscape.Sp.Rich.%s",timepoint)
    Results[[sp.r.lbl]][1] = sp.richness # = gamma diversity  #Could also use nbsp from the nbFD output.
    
    mean.richness = sum(tot.occ) / p  #alpha diversity = sum of species richness of each cell divided by the number of cells
      #p is the extent ^ 2, which is equal to the number of cells.  But you should know that by this point in the code!+
    
    # Calculate beta-diversity of species richness at start of simulation
    #B = Y/A  Y = total species diversity, A = average diversity across landscape
      #B is the number of sub-units, if units shared no species in common.
    beta.div = sp.richness/mean.richness  # This should be 1 for my initial scenario!
    beta.div.lbl = sprintf("landscape.Beta.Div.%s",timepoint)
    Results[[beta.div.lbl]][1] = beta.div
            
    #Write biomass to results
    biomass.lbl = sprintf("landscape.Biomass.%s",timepoint)
    Results[[biomass.lbl]][1] = biomass
    
    abund.vec = matrix(abund.vec,nrow = 1, dimnames = list("community",nam.vec))
        
    return(list(Results,abund.vec))
    }

#' Extract Scale Cells
#'
#' Extract the indices of cells to keep (indicated by T) and those to exclude (indicated by F).
#' Basically, the vectors that will be extracted from will have S elements for
#' each cell. Therefore the initial scale.cells entry needs to be expanded to accomodate this.
#'
#' @param scale.cells.lst A list of cells to be kept
#' @param sv An indicator
#' @param S The number of life stages in the model
extract.scale.cells = function(scale.cells.lst, sv, S){
    scale.cells.vec = scale.cells.lst[[sv]]
    scale.cells = c()
    for (sc in scale.cells.vec){
        scale.cells = c(scale.cells, rep(sc,S))
        }    
    return(scale.cells)
    }

#' Create a subset for scale cells
#'
#' Select just a particular value of an environmental layer for evaluating
#' biodiversity metrics
#'
#' @param scale.cells.lst In this case, a text field with three parts. The first
#' part is the word subset, the second part is the landscape.identifier to select
#' the landscape layer, and the third part is the target value from that landscape layer
#' @param landscape.identifiers A vector of landscape.identifiers
#' @param landscape The landscape consisting of environmental layers
#'
scl.subset = function(scale.cells.lst, landscape.identifiers, landscape){
    parts = strsplit(scale.cells.lst,";")[[1]]
      this.lbl = parts[2]
      this.val = parts[3]
    
    #Go to environmental layer
    for (y in 1:length(landscape.identifiers)){
        lbl = landscape.identifiers[y]
        if (lbl == this.lbl){
            this.lyr = landscape[[y]]
            #Create a vector where values that match the target value are 1 and the rest are 0.
            this.lyr = sapply(this.lyr, target.recode, this.val)
            scale.cells.lst = list(this.lyr)
            }
        }
        
    return(scale.cells.lst)
    }


#' Convert double T's to T, otherwise set to F
#'
#' I'm sure this exists as a function!
#'
#' @param x the input to be recoded
IA.recode = function(x){
    y = F
    if (x == 2){
        y = T
        }
    return(y)
    }

#' Calculate output metrics
#' 
#' Calculates output metrics for the simulation.
#' E.g., species richness, beta diversity, biomass, and functional diversity
#' 
#' @param Results A dataframe to contain the results
#' @param timepoint The timpoint being calculated
#' @param n0.lst A list of abundances of each stage for each species
#' @param IAdults An index indicating which entries correspond to adults
#' @param spe The total number of species in the species pool
#' @param p The number of cells in a square landscape
#' @param SpTraits A dataframe containing species traits
#' @param scale.cells A list of cells to restrict the calculation to.
#' @param sv An indicator
#' @param num.tp An indicator for the timepoint
calc.metrics.v2 = function(Results,timepoint,n0.lst,IAdults,spe,p,SpTraits, scale.cells, sv, num.tp ){

    #Subset the IAdults vector to only apply to the cells listed in scale.cells
    IAdults = IAdults + scale.cells #0 = F, 1 & 2 = T, use recode & change back to T/F
    IAdults = sapply(IAdults, IA.recode)

    #Set index to be correct for timepoint and sv
      # NOTE: This will need to change if you actually use numbered timepoints (i.e. a system other than Initial, Final, Change.
    if (timepoint == "Initial"){ timepoint = 1 }
    if (timepoint == "Final"){ timepoint = 2 }
    if (timepoint == "Change"){ stop("Somehow a timepoint associated with Change entered calc.metrics.v2. This should not happen.") }
    
    #ri for short!
    ri = result.index = (sv - 1) * num.tp + timepoint  #sv - 1 makes it a multiplier of 0 for the first sv, 1 for the second, etc. Multiply by number of timepoints to get the correct offset. then add the specific timepoint information.

    # Get adult abundances for the scenario, calculate species richness, alpha diversity, and biomass
    tot.occ = 0
    sp.richness = 0
    abund.vec = c()
    nam.vec = c()
    biomass = 0
    sp.extinct = 0
    for (sp in 1:spe){
        sp.abund = n0.lst[[sp]][IAdults] #n0.lst is the list of abundances.  sp indexes which species is considered, and [IAdults] is an index that gets only values for adults.
        sp.abund = sapply(sp.abund,drop.partial)

        #Set up species richness and alpha diversity
        sp.occ = sapply(sp.abund,recode)
        #If at least one adult of a species is present somewhere, increase species richness by 1.
        if (sum(sp.occ) > 0){
            sp.richness = sp.richness + 1
            }
        if (sum(sp.occ) == 0){
            sp.extinct = 1
            }
        tot.occ = tot.occ + sp.occ

        abund.vec = c(abund.vec,sum(sp.abund)) #sum of sp.abund because it is the abundance for every cell in the landscape.  Right now we're ignoring that level of diversity, because I'm feeling overwhelmed.
        nam.vec = c(nam.vec,sprintf("%s",Digit(sp,2)))
        sp.txt = sprintf("%s",sp)
        sp.biomass = sum(sp.abund) * SpTraits[sp.txt,"biomass.adult"] #Biomass = number of individuals * biomass of an adult individual
        biomass = biomass + sp.biomass
        }

    #Write species richness to results
    sp.r.lbl = "Sp.Rich"
    Results[[sp.r.lbl]][ri] = sp.richness # = gamma diversity  #Could also use nbsp from the nbFD output.
    
    mean.richness = sum(tot.occ) / length(sp.abund)  #alpha diversity = sum of species richness of each cell divided by the number of cells (p is replaced by length(sp.abund) because some cells may be dropped from the calculations. The length of sp.abund vector should remain the same, only the values should differ between species
      #p is the extent ^ 2, which is equal to the number of cells.  But you should know that by this point in the code!+
    
    # Calculate beta-diversity of species richness at start of simulation
    #B = Y/A  Y = total species diversity, A = average diversity across landscape
      #B is the number of sub-units, if units shared no species in common.
    beta.div = sp.richness/mean.richness  # This should be 1 for my initial scenario!
    beta.div.lbl = "Beta.Div"
    Results[[beta.div.lbl]][ri] = beta.div
            
    #Write biomass to results
    biomass.lbl = "Biomass"
    Results[[biomass.lbl]][ri] = biomass
    
    abund.vec = matrix(abund.vec,nrow = 1, dimnames = list("community",nam.vec))
        
    return(list(Results,abund.vec))
    }


#Function to systematically calculate change in metrics.
#' Calculate metric change
#'
#' Calculate changes in metrics from beginning of simulation
#' E.g., change in species richness, or change in functional diversity
#'
#' @param Results The results dataframe to store the computed changes
#' @param out.metrics The fields for which change needs to be calculated
#' @param scale.vec A vector of indicators
calc.delta.metrics = function(Results,out.metrics,scale.vec){

    #Simple example to get concept, then need to make it more automatic
    #Results$Change.Num.Species = Results$Final.Num.Species - Results$Initial.Num.Species 

    #Calculate change for metrics in out.metrics
    for (m in out.metrics){
        for (sv in scale.vec){
            im = sprintf("%s.%s.Initial",sv,m)
            fm = sprintf("%s.%s.Final",sv,m)
            dm = sprintf("%s.%s.Change",sv,m)
        
            Results[[dm]][1] = Results[[fm]][1] - Results[[im]][1]
            }
        }
    
    # Update when re-enabling community weighted means
    #Calculate change for metrics in lbl.lst & TraitNames
    #for (tr in TraitNames){
    #    for (i in lbl.lst){
    #        im = sprintf("%s.I.%s",i,tr)
    #        fm = sprintf("%s.F.%s",i,tr)
    #        dm = sprintf("%s.C.%s",i,tr)
    #        
    #        Results[[dm]][1] = Results[[fm]][1] - Results[[im]][1]
    #        }
    #    }

    return(Results)
    }

#Function to systematically calculate change in metrics.
#' Calculate metric change
#'
#' Calculate changes in metrics from beginning of simulation
#' E.g., change in species richness, or change in functional diversity
#'
#' @param Results A dataframe containing the results to be output
#' @param out.metrics A list of the fields for which change should be calculated
#' @param sv an indicator
#' @param num.tp An indicator for the timepoints
calc.delta.metrics.v2 = function(Results,out.metrics,sv, num.tp){

    ii = initial.index = (sv - 1) * num.tp + 1  #sv - 1 makes it a multiplier of 0 for the first sv, 1 for the second, etc. Multiply by number of timepoints to get the correct offset. then add the specific timepoint information.
    fi = final.index = (sv - 1) * num.tp + 2
    ci = change.index = (sv -1) * num.tp + 3

    #Calculate change for metrics in out.metrics
    for (m in out.metrics){
        Results[[m]][ci] = Results[[m]][fi] - Results[[m]][ii]
        }

    return(Results)
    }


#' Create base environmental layer
#' 
#' Create a base environmental layer that can then be arranged into different
#' configurations.
#' %%Add details to explain different options & link to where functions are defined.
#' 
#' @param env.info A list with three parts: one function and two parameters
#' @param p The number of cells in the landscape
#' @param to.round Number of decimal places in the resulting environmental layer
#' @return returns a created environmental layer (a vector)
#' @note Note that the created environmetnal layer is converted to a matrix by
#' rows, rather than by columns.
#'
get.base.env.lyr = function(env.info,p, to.round = 1){
    funct = env.info[[1]]
    par1 = env.info[[2]]
    par2 = env.info[[3]]

    if (funct != "cover"){
        out.base.lyr = compute.funct(p,funct,par1,par2, to.round = to.round)
        }

    #Assign based on specified cover thresholds.
      #In this case, par1 is a list giving the cover levels for integer cover codes
        #for example, if grassland, forest and row crops were 50%, 25% and 25% of landscape cover,
          #this would need to be c(.5,.25,.25), and grassland would be coded 1, forest = 2, row crop = 3.
        #par2 gives the starting number, and will ordinarily be 1.  but sometimes 0 is desired.
    
    if (length(par2) != 1){
        stop("Environmental_layers param1 must be a single number (start of sequence)")
        }
    
    s.p = as.num(par2) - 1 #pre-cursor to the cover value - takes the starting number (par2) and subtracts 1 (because you will add at least 1 to it later)
    #NOTE: consider moving this to compute.funct, if you think it will be used by another function (I don't think it needs an additional input, but am not positive.)
    if (funct == "cover"){
        out.base.lyr = c()
        
        for (cc in 1:length(par1)){ 
            
            prop.cov = par1[cc]
            #cov.val = par2[cc]

            #check that prop.cov <= 1
            if (prop.cov > 1){
                stop("Cover proportion cannot exceed 1")
                }

            n.cells = p * prop.cov #Get number of cells that should be of this cover type.
    
            #Check that this.cov is not fractional!
            if (n.cells != round(n.cells,0)){
                stop("Error: Cover levels cannot be assigned to the landscape in those proportions!")
                }

            #Get the value to assign to this cell based on cov.val & cc
            #s.p = cov.val - 1
            this.cov.val = s.p + cc

            add.cover = rep(this.cov.val,n.cells)
            out.base.lyr = c(out.base.lyr,add.cover)
            }        
        }

    return(out.base.lyr)
    }

#' Create environmental layer configuration
#'
#' Takes the layer generated by get.base.env.lyr and draws a sample from it
#' to create a new environmental layer configuration
#'
#' @param base.lyr An environmental layer (as a vector)
#' @return An environmental layer (as a vector)
#' @note Note that the vectors are converted to a matrix by rows, rather than by
#' columns
#'
get.env.lyr = function(base.lyr){
    p = length(base.lyr) #This is the same as the extent^2, but redefined here, to keep the function self-contained
    #If the landscape is 1, then sample will fail and give wrong results, so just assign the landscape
    if (p == 1){
        out.lyr = base.lyr
    }else{
        out.lyr = sample(base.lyr,p)
        }

    return(out.lyr)
    }

#Function to draw an environmental layer that requires particular conditions of another environmental layer
#' Create conditional environmental layer
#'
#' Generates an environmental layer, taking into account the values of another
#' environmental layer.  The new layer will be assigned only for particular
#' values of the other layer, for all other values, the new layer will have the
#' value of zero.
#'
#' @param env.info A list with three parts: one function code and two parameters
#' @param p The number of cells in the landscape
#' @param other.lyrs The other layer (or layers) to take into account
#' @param other.lyrs.info The values in the other layer that represent suitable
#' habitat
#' @return An environmental layer with a value for each cell in the landscape
#' (as a vector)
#' @note WARNING: Not tested for conditioning on more than one layer.  It should work, but needs testing
#'
get.cond.env.lyr =function(env.info,p,other.lyrs,other.lyrs.info){

    #Unpack env. info
    funct = env.info[[1]]
    par1 = env.info[[2]]
    par2 = env.info[[3]]
    
    this.lyr = rep(NA,p) #Create a layer, use NA's as placeholders
    
    #Variable to hold suitable cells from all layers
    suit.cells = rep(1,p) #1 indicates all are suitable
    for (o in 1:length(other.lyrs)){
        cur.lyr = other.lyrs[[o]]
        cur.info = other.lyrs.info[[o]] #contains acceptable values to find
        
        #Create a vector to hold suitable cells from this layer (because the below will need a different join
          #I.e. here if one value is 0 and one is 1, I want it to be 1.  But if one layer is 0 and one layer is 1, I want it to be 0.
        layer.cells = rep(0,p)
        
        #Need to loop through values in cur.info, this allows more than one value to be useful
        for (c.val in cur.info){
            #Return index of positions in cur.lyr that match values in c.val = #Suitable cells from this 
            cell.index = which(cur.lyr == c.val)
            layer.cells[cell.index] = 1 #Assign 1's to suitable cells
            }
        
        #Change suitable cells to unsuitable #Could also do the above index approach!
        suit.cells = suit.cells * layer.cells #This does pairwise multiplication, so a 0 anywhere will convert a suit.cell to a zero)
        
        }
    
    #Get index of positions of suitable cells
    suit.index = which(suit.cells == 1)
    not.suit.index = which(suit.cells == 0)

    #get number of suitable cells
    num.suit = sum(suit.cells)

    #Get base values for suitable cells only
    base.lyr = get.base.env.lyr(env.info,num.suit)
    
    #Draw random order for suitable cells only
    val.lyr = get.env.lyr(base.lyr)
    
    #Where suit.cells == 1, assign appropriate values (these can include 0)
    this.lyr[suit.index] = val.lyr

    # otherwise, assign 0
    this.lyr[not.suit.index] = 0 #I tried this, thinking it would be simpler, but it did not work: this.lyr[!suit.index] = ...

    return(this.lyr)
    }

#' Read landscape layer
#'
#' Read in a landscape layer from a .csv.  Used when initially reading in a layer
#' and if c.type == "from.file".
#'
#' @param landscape.dir The path for this model run
#' @param lbl The name of the landscape layer being read in
#' @param lnd.lbl A scenario specific label identifying the landscape layers to be used.
#' @param change.count An indicator for the current environmental change step.
#' @param p Number of cells in the landscape
#'
read.landscape.lyr = function(landscape.dir,lbl,lnd.lbl,change.count,p){

    #Set up and read in the file
    infile = sprintf("%s%s_%s.csv",landscape.dir,lbl,lnd.lbl)
    prep.lyr = read.table(infile, header = T, sep = ",", row.names = 1) 

    #extract the layer pertaining to the current change.count
    act.lyr = as.num(prep.lyr[as.character(change.count), ]) #change.count should indicate the appropriate row.  as.character ensures that the rowname is used, instead of the index (in case the file is reordered)
    
    #Check that the current layer was read in from file
    if (is.na(act.lyr[1])){
        stop(sprintf("Landscape layer read in as NA for %s change count %s.  Perhaps a record corresponding to the current ChangeStep in the landscape file is missing?",lnd.lbl, change.count))
        }
            
    #Check that the current layer has the required extent
    if (length(act.lyr) != p){
        stop(sprintf("Specified number of cells in landscape (extent^2) does not match the length of the input landscape layer %s",lbl))
        }
    
    return(act.lyr)
    }

#DD# add in ' again
# # Read landscape layer (old)
# #
# # Read in a landscape layer from a .csv.  Used when initially reading in a layer
# # and if c.type == "from.file".
# #
# # @param inpath The path for this model run
# # @param lbl The name of the landscape layer being read in
# # @param s.lbl A label for the scenario being considered
# # @param change.count An indicator for which environmental change is being used.
# #
# read.landscape.lyr = function(inpath,lbl,s.lbl,change.count,p){
# 
#     #Set up and read in the file
#     infile = sprintf("%s%s_%s_%s.csv",inpath,lbl,s.lbl,change.count)
#     prep.lyr = read.csv(infile, header = F) #When written, byrows = T, so this needs to be taken into account when unpacking
#     
#     # Re-order the file to be in proper order and of proper type
#     # There has to be a better way to script this
#     act.lyr = c()
#     for (a.row in 1:nrow(prep.lyr)){
#         this.row = as.num(prep.lyr[a.row, ]) #As num converts it to numeric and takes it out of the weird list format - otherwise setup.matrices will crash!
#         act.lyr = c(act.lyr,this.row)
#         }
#        
#    #Check that the current layer has the required extent
#    if (length(act.lyr) != p){
#        stop(sprintf("Specified number of cells in landscape (extent^2) does not match the length of the input landscape layer %s",env.lbl[y]))
#        }
#    
#    return(act.lyr)
#    }

#' Function to set up copula
#'
#' This function sets up a copula for the model. A copula is a set of variables
#' generated with a certain level of correlation.
#'
#' @param my.env Information about the environmental layers
#' @param MaxTime The number of timesteps the model will be run for
#' @param p The number of cells in a square landscape
#' @param include.copula The number of copulas to create
#' @param run.path The path for the analysis
#' @param env.lbl A label
#' @param s.lbl A label
#'
setup.copula = function(my.env,MaxTime,p,include.copula,run.path,env.lbl,s.lbl){

    cop.element = list(c(),c(),c(),c())
    cop.lst = rep(list(cop.element),include.copula) #Create a list object for each copula setup.
    #Loop through environmental layers & see which ones require the copula
    for (y in 1:nrow(my.env)){
        #Extract info needed for the copula & put in a list.
        #c.mag will have:
          # copula number (which copula this variable is part of)
          # copula correlation (this will need to be the same & redundant between all variables with a shared copula)
          # variable's function
          # variable's mean
          # variable's variance
          
          #e.g. 1;0.3;lnorm;2;1
        #Extract from c.mag
        c.mag = as.character(my.env[y, "env.change.mag"])
        c.mag = strsplit(c.mag,";")[[1]]
        
        cop.num = as.num(c.mag[1])
        this.cor = as.num(c.mag[2])
        this.fx = c.mag[3]
        this.mean = as.num(c.mag[4])
        this.var = as.num(c.mag[5])
        
        #update copula list values
        this.cop = cop.lst[[cop.num]]
        cop.cor = this.cop[[1]]         #extract list
        cop.cor = c(cop.cor, this.cor)  #append to list
        this.cop[[1]] = cop.cor         #update list value
        
        cop.fxs = this.cop[[2]]         #extract list
        cop.fxs = c(cop.fxs,this.fx)    #append to list
        this.cop[[2]] = cop.fxs         #update list value
        
        cop.means = this.cop[[3]]       #extract list
        cop.means = c(cop.means,this.mean) #append to list
        this.cop[[3]] = cop.means       #update list
        
        cop.vars = this.cop[[4]]        #extract list
        cop.vars = c(cop.vars, this.var) #append to list
        this.cop[[4]] = cop.vars        #update list
        
        cop.lst[[cop.num]] = this.cop   #update copula list
        }

    #loop through each desired copula
    for (a.cop in cop.lst){
        #Extract values to use for the copula
        n = MaxTime
        correlations = a.cop[[1]]

        #check that all correlation values were assigned properly (and redundantly)
        correlation = unique(correlations)
        if (length(correlation) != 1){
            stop("Error: More than one correlation value given for a single copula")
            }

        fxs = a.cop[[2]]              #e.g., c("lnorm","beta")
        fxs.means = a.cop[[3]]
        fxs.vars = a.cop[[4]]

        # Create landscape files for stochastic variables according to a copula
        create.stochastic.landscape(run.path,env.lbl,s.lbl,correlation,n,p,fxs,fxs.means,fxs.vars)                
        }
    }


#' Create stochastic landscape
#'
#' Create a stochastic landscape for the stochastic plants model using a copula function
#' In this case, it creates correlated distributions between a log-normal function
#' (fertility) and a beta distribution (stochastic survival)
#'
#' @param run.path Path for the model run
#' @param my.lyrs Layers to include in the landscape
#' @param s.lbl A label
#' @param n Number of samples to generate (1 per time step)
#' @param p Number of cells in the landscape
#' @param correlation Desired correlation level (will not be exact)
#' @param fxs Vector of functions for the copula
#' @param fxs.means Vector of means for the copula, in same order as functions
#' @param fxs.vars = Vector of variances/sd for the copula, in same order as above
#' @note Requires copula package.  Cells in the landscape will be generated independently of one another.
#' @references Sklar 1959, others? #DD#
create.stochastic.landscape = function(run.path,my.lyrs,s.lbl,correlation,n,p,fxs,fxs.means,fxs.vars){

    #Get number of points needed 
    np = (n + 1) * p #need one point for every cell at every timestep (plus one for starting conditions)

    #Get copula values
    out.vals = do.copula(correlation,np,fxs,fxs.means,fxs.vars)
    
    #Loop through output columns - these will be the output values for each input variable
    for (a.col in 1:ncol(out.vals)){
        
        this.col = out.vals[ ,a.col]
    
        #Loop through and output values
        is.first = 1  #Indicator for whether this is the first time through the loop (for file creation purposes)
        timestep = 1
        c.start = 1
        c.end = c.start + p - 1
        while(c.end <= np){
            
            #Write to files in blocks of p
            out.data = this.col[c.start:c.end] #Select only the values corresponding to this block
            outdir = sprintf("%slandscape/",run.path)
            dir.create(outdir, recursive = T, showWarnings = F)
            outfile = sprintf("%s%s_%s.csv",outdir,my.lyrs[a.col],s.lbl)

            write.env.lyr(out.data,timestep,sqrt(p),outfile,is.first)
            #write.table(out.data, file = outfile, col.names = F, row.names = F, sep = ",")

            #Update variables
            c.start = c.start + p
            c.end = c.end + p
            timestep = timestep + 1
            is.first = 0
            }
        }
    }


#' Function to implement copula
#' 
#'
#' @param np Number of samples to generate
#' @param correlation Desired correlation level (will not be exact)
#' @param fxs Vector of functions for the copula
#' @param fxs.means Vector of means for the copula, in same order as functions
#' @param fxs.vars = Vector of variances/sd for the copula, in same order as above
#' @note Currently only works for log-normal and beta distributions.
#' @author Jakob Gerstenlauer & Sasha Keyel
#' @references Sklar 1959 %DD%
#'
do.copula = function(correlation,np,fxs,fxs.means,fxs.vars){
    if (!require(copula)) { stop("Please install copula: install.packages('copula')") }

    marg.list = list()
    #Set up list objects for mvdc function of the copula
    for (f in 1:length(fxs)){
        fx = fxs[f]
        
        if (fx == "lnorm"){
            new.item = list( meanlog = log(fxs.means[f]), sdlog = log(fxs.vars[f]))
            }

        if (fx == "beta"){
            fxs.mean = fxs.means[f]
            fxs.var = fxs.vars[f]
                        
            #convert mean/var to alpha/beta of beta distribution
            Alpha<- fxs.mean * ( (1 - fxs.mean)* fxs.mean * (1/fxs.var) - 1)
            Beta<- (Alpha / fxs.mean) - Alpha
            
            new.item = list(  shape1=Alpha,    shape2=Beta)
            }

        if (fx != "lnorm" & fx != "beta"){
            stop("SpatialDemography is not yet setup to incorporate copula options other than lnorm and beta")
            }

        #WARNING:  Watch for errors in list structure if modifying!
        #Add new list object to the margins list
        marg.list = append(marg.list, list(new.item))
        }

    #do copula
    #Create a new archimedean copula of family frank for two dimensions (whatever that means!)
    n.dim = length(fxs)
    Copula.normal<-copula::ellipCopula(family="normal", dim= n.dim, dispstr="un", param=correlation)
    
    #Create a new multivariate distribution based on the copula and two marginals
    MultivarDist<-copula::mvdc(copula=Copula.normal, margins= fxs, paramMargins = marg.list)   
                      
    #Create and plot random numbers from the multivariate distribution
    x <- copula::rMvdc(np,MultivarDist) #rmvdc in Jakob's original example is deprecated.  The new version switches the order

    return(x)
    }

#DD# Do I want to add documetation for each of the inputs & outputs?  Might not be a bad idea.

#' Set up model matrices
#'
#' This function sets up the core matrices for the matrix diagnostics and the
#' simulation model.
#'
#' @param K_g Carrying capacity in grams
#' @param spe Total number of species in the species pool
#' @param SpTraits A dataframe containing species traits
#' @param p The number of cells in a square landscape
#' @param S The number of stages in the matrix model
#' @param landscape a list of environmental layers, each with a value for each cell in the landscape
#' @param landscape.identifiers A list of single-letter identifiers for each environmental layer
#' @param distances Distances between each cell in the landscape
#' @param B1.template A template for the B1 demography matrix
#' @param B2.template A template for the B2 demography matrix
#' @param P The vec-permutation matrix
#' @param M_sub The M matrix template
#' @param DispPath The path to the dispersal tables
#' @param vdb.data An indicator for whether or not to create visual debugger data
#' @param change.count The change step
#' @param outpath.base The base output path.
#' @param num.sim The number of simulations to use in creating dispersal probabilities
#' @param run.times (optional, defaults to NA) For timing the function
#' @param run.lbl = (optional, defaults to NA) For labeling the timing of the function
#' @param multi.species.K (default is 0) An indicator for whether a multispecies carrying capacity is desired
#' @param edge.type (default is "TORUS") An indicator for what type of landscape edge is desired.
#'
setup.matrices = function(K_g,spe,SpTraits,p,S,landscape,landscape.identifiers,distances,B1.template,B2.template,P,M_sub,DispPath,vdb.data,change.count,outpath.base,num.sim,run.times = NA,run.lbl = NA,multi.species.K = 0, edge.type = "TORUS"){

    #**# Modify to incorporate visual debugger system
    
    #Set up some model inputs that depend on species
    
    #Set up carrying capacity.  Multi-species carrying capacity is dealt with in another location
    if (multi.species.K == 0){
        K.lst = list()
        for (sp in 1:spe){
            sp.txt = sprintf("%s",sp)
            sp.biomass = SpTraits[sp.txt, "biomass.adult"] #Look up species adult biomass
            K<- floor( K_g / sp.biomass )                  # Get species carrying capacity for adults in number of individuals
            K.lst = append(K.lst,K)
            }
    } else {
        K.lst = NA
        }
              
    #Create the composite demography matrices B1 and B2.  Call composite.demography.matrix.description() for more information 
    ccdm.out = CreateCompositeDemographyMatrix(B1.template,B2.template,spe,SpTraits,p,S,landscape,landscape.identifiers)        
      B1.lst = ccdm.out[[1]]
      B2.lst = ccdm.out[[2]]

      run.times = c(run.times,gettime()); run.lbl = c(run.lbl,"composite demography matrix completed")              

    # Add eigenvalue debugger here
    if (vdb.data == 1){
        # NOT YET SCRIPTED
        #EigenVis(B1.lst)
        }

    #**# Consider changing this approach to remove the species loop.
    DispersalProbabilities.lst = list()
    SeedDispersalRatio.lst = list()
    M.lst = list()
    
    for (sp in 1:spe){
        
        #Get dispersal probabilities and seed dispersal ratio
        dir.create(DispPath,showWarnings = F)
        cdispmat.out = get.disp.prob(sp,SpTraits,p,distances,DispPath,num.sim,edge.type)
          #DispersalProbabilities= cdispmat.out[[1]]
          DispersalProbabilities.lst = append(DispersalProbabilities.lst,cdispmat.out[[1]])
          #SeedDispersalRatio = cdispmat.out[[2]]           
          SeedDispersalRatio.lst = append(SeedDispersalRatio.lst,cdispmat.out[[2]])
          #run.times = c(run.times,gettime()); run.lbl = c(run.lbl,sprintf("DispersalProbabilitiesGenerated_%s",sp))
    
          if (sp == 1){
              run.times = c(run.times,gettime()); run.lbl = c(run.lbl,"Dispersal probabilities generated")              
              }
        
        #Create the composite dispersal matrix M:
        this.M = CreateCompositeDispersalMatrix(DispersalProbabilities.lst[[sp]],p,S,M_sub)
        M.lst = append(M.lst,this.M) 
          #run.times = c(run.times,gettime()); run.lbl = c(run.lbl,sprintf("Composite.Dispersal.Matrix.made_%s",sp))
    
          if (sp == 1){
              run.times = c(run.times,gettime()); run.lbl = c(run.lbl,"composite dispersal matrix completed")              
              }
        }        
    
    #Output data for visualizing dispersal and reporting overall eigenvalues for the matrix
    if (vdb.data == 1){
        # NOTE: Add dispersal visual debugger here.
        #DispersalVis(STUFF) #NOT YET SCRIPTED
        
        # Calculate eigenvalue for overall landscape
          # BROKEN, need to do checks to make sure it can be calculated (or figure out how to deal with the failed calculations
        #CalcA(B1.lst,B2.lst,P,M.lst,change.count,outpath.base)
        
        #NOTE Perturbation analysis could be added here.
        
        }
    
    
    mat.info = list(K.lst,B1.lst,B2.lst,DispersalProbabilities.lst,SeedDispersalRatio.lst,M.lst,run.times,run.lbl)
    
    return(mat.info)
    }

#' Create results file
#'
#' Set up the output results file and results dataframe
#'
#' @param ResultsFile The file that results will be written to.
#' @param out.metrics The fields to be calculated
#' @param scale.vec An indicator vector
#' @param do.cwm An indicator for whether community weighted means should be calculated. (Needs testing, may be broken)
setup.results = function(ResultsFile,out.metrics,scale.vec, do.cwm = 0){
    
    ## Set up header for results file
    r.hdr = c("Scenario.Number","LastTime")
    tp.vec = c("Initial","Final","Change")
    for (k in out.metrics){
        for (sv in scale.vec){
            for (m in tp.vec){
                new.item = sprintf("%s.%s.%s",sv,k,m)
                r.hdr = c(r.hdr,new.item)
                }
            }
        }

    if (do.cwm == 1){
        print("This function is broken & cannot be implemented without script changes")
        }
    
    ## Results will be stored in a dataframe that can be updated as the script goes.
    Results = data.frame(Scenario = NA)
    
    for (item in r.hdr){
        Results[[item]] = NA
        }
        
    if (!file.exists(ResultsFile)){
        #Only output results file if one does not already exist
        cat(file = ResultsFile,sprintf("Scenario,%s\n",(listtotext(r.hdr,separator = ','))))
        }
    
    return(Results)
    }

#' Create results file in a new format
#'
#' Set up the output results file and results dataframe, with a row for each content type, and a row for initial, final and change
#'
#' @param ResultsFile The file that results will be written to.
#' @param out.metrics The fields to be calculated
#' @param nrow.results Number of rows in the final results file
setup.results.v2 = function(ResultsFile,out.metrics,nrow.results){
    
    #Set up header for results file
    r.hdr = c("Scenario.Number","Scale","Timepoint","LastTime", out.metrics)
      #Scale = what scale the metrics are calculated over
      #Timepoint - initial, final, change, etc. (could add options for more timepoints here)    

    #Set up rows for results file (& set up columns for which you already have sufficient information
    ## Results will be stored in a dataframe that can be updated as the script goes.
    Results = data.frame(Scenario = rep(NA, nrow.results)) 
    
    for (item in r.hdr){
        Results[[item]] = rep(NA, nrow.results)
        }
        
    if (!file.exists(ResultsFile)){
        #Only output results file if one does not already exist
        cat(file = ResultsFile,sprintf("Scenario,%s\n",(listtotext(r.hdr,separator = ','))))
        }
    
    return(Results)
    }


#' Generate species
#'
#' This function generates species based on the instructions given in two input
#' files.
#'
#' sp.base.file contains the base vital rates and how they should be distributed
#' in the regional species pool, while resp.traits contains information on 
#' species response traits.  This creates a regional species pool that contains
#' a realization of a defined distribution of base vital rates and response
#' traits.
#' 
#' @param sp.base.file An input file containing base vital rates.
#' See Appendix S1 (ODD protocol) for file setup & details
#' @param sp.resp.instr.file A file containing details on the species response
#' traits, and how they should be distributed.  Currently these are independent
#' of the underlying base vital rates.
#' @param sp.out.file The species file to be created
#' @param tot.sp.num The total number of species to be generated
#' @note Currently does not support creation of constrained species traits,
#' although the intention is to add that functionality in future versions.
#' WARNING There appears to be a bug in R (3.1.0) that is affecting the way
#' some column headers are being displayed during troubleshooting. (this will
#' not affect function users and only matters for those modifying the functions)
#' %% Notes on function development in 2014_05_MC.docx file,\
#' %% under 2014-05-06 (the second one)
#'
gen.spp = function(sp.base.file,sp.resp.instr.file,sp.out.file,tot.sp.num){

    #Set up base vital rates (may or may not be correlated/constrained
      #External input should provide mean, standard deviation, functional form of variation
   #Read in species base file (or not, if it is already assumed to be an R object)
    if (typeof(sp.base.file) == "character"){
        sp.base = read.csv(sp.base.file)
    }else{
        sp.base = sp.base.file
        }
    
    #Convert first column (vital rates) to row names
    row.names(sp.base) = sp.base$VitalRate
    sp.base$VitalRate = NULL
    vrs = row.names(sp.base) #Create a variable of the row names
    
    #Need to put the created vital rates in some sensible format.
    vrs.out = data.frame(sp = seq(1,tot.sp.num))
    
    vrs.out = get.vrs(vrs.out,vrs, sp.base,tot.sp.num)
    
    #Check that created species are valid
    vrs.out = check.spp(vrs.out)
    
    loop.counter = 0
        
    #Remake invalid species
    while(max(vrs.out$is.error) > 0){

        #Add emergency break if code cannot create valid species
        loop.counter = loop.counter + 1
        if (loop.counter == 100000){
            stop("Valid species of the requisite number could not be generated.\n Please check input species generation parameters for accuracy and whether they produce possible results") 
            }
            
        #Keep valid species
        vrs.good = vrs.out[vrs.out$is.error == 0, ]
        
        #Reassign values to problem species
        vrs.bad = vrs.out[vrs.out$is.error == 1, ]
        to.redo = nrow(vrs.bad)
        redo.lbls = vrs.bad$sp #use species as labels
        
        vrs.redo = data.frame(sp = redo.lbls)
        rownames(vrs.redo) = redo.lbls #make row names match species
        vrs.redo = get.vrs(vrs.redo,vrs,sp.base,to.redo)

        #Check that created species are valid
        vrs.redo = check.spp(vrs.redo)
        
        # merge vrs.good & vrs. redo to get a single new dataframe
        vrs.out = merge(vrs.good,vrs.redo,all = T)
        
        }

    #Read in response traits to generate (unless it is assumed to already be an R object
    if (typeof(sp.resp.instr.file) == "character"){
        resp.traits = read.csv(sp.resp.instr.file)
    }else{
        resp.traits = sp.resp.instr.file
        }

    for (e.row in 1:nrow(resp.traits)){
        resp.trait = resp.traits[e.row, ]
    
        #Unpack environmental layer
        lbl = as.character(resp.trait["Label"][[1]]) #apparently this is a list object, and when you do as.character, you get numbers instead of the letters!
        opt.fun = as.num(resp.trait["DistributionFunction"][[1]])
        par1 = as.num(resp.trait["distfunpar1"][[1]])
        par2 = as.num(resp.trait["distfunpar2"][[1]])

        #Currently resp.fun and parameters are fixed for all species
        resp.fun = as.num(resp.trait["ResponseFunction"][[1]])
        resp.par1 = as.num(resp.trait["respfunpar1"][[1]])
        resp.par2 = as.num(resp.trait["respfunpar2"][[1]])
                
        #Generate optima for species
            #No checks are included here to ensure that they are valid.  Note that all functions will be effectively truncated at 0 & 1 by a later step which will drop any species outside this range # These are truncated distributions in that sense.
        sp.vals = compute.funct(tot.sp.num,opt.fun,par1,par2, to.round = 3)
        vrs.out[lbl] = sprintf("%s;%s;%s;%s",sp.vals,resp.fun,resp.par1,resp.par2)
                
        }

    #Apply constraints, if any
    #Not Scripted

    #Remove internal is.error column from species file.
    vrs.out$is.error = NULL 

    #Write species information to file
    write.table(vrs.out, file = sp.out.file, sep = ",",row.names = F)

    #Convert species column to row names to use in lookups
    rownames(vrs.out) = vrs.out$sp
    vrs.out$sp = NULL

    
    #Return species information in R usable form
    return(vrs.out)
    }

#' Get vital rates
#'
#' Function to loop through vital rates and get values
#' @param vrs.out A vector to contain the final vital rates
#' @param vrs A vector of vital rates
#' @param sp.base Information about the vital rates and their distribution for species generation
#' @param tot.sp.num Total number of species (== spe)
#'
get.vrs = function(vrs.out,vrs, sp.base,tot.sp.num){

    #Loop through vital rates
    for (vr in vrs){

        #Assign values for each species
        funct = sp.base[vr,1]
        par1 = sp.base[vr,2]
        par2 = sp.base[vr,3]
        
        #assign vital rates
          #No checks are included here to ensure that they are valid.  Note that all functions will be effectively truncated at 0 & 1 by a later step which will drop any species outside this range # These are truncated distributions in that sense.
        sp.vals = compute.funct(tot.sp.num,funct,par1,par2,to.round = 3)
        vrs.out[vr] = sp.vals #Assign to the dataframe containing species  
        }

    return(vrs.out)
    }

#Function implement standard functions.  Used by get.base.env.lyr, gen.spp and get.vrs
#' Implement standard functions
#'
#' This function implements one of several functions, based on an input code.
#'
#' Function codes are as follows:
#' 1 = log-normal distribution (par1 = mean, par2 = sd)
#' 2 = normal distribution (par1 = mean, par2 = sd)
#' 3 = constant (par1 = value, par2 = NA)
#' 4 = uniform distribution, continuous values
#' (par1 = lower bound, par2 = upper bound)
#' 5 = uniform distribution, only integer values
#' (par1 = lower bound, par2 = upper bound, par1 & 2 must be integers!)
#' SEE Appendix S1 ODD description for more information
#'
#' @param num.reps The number of values to be created
#' @param funct The function to use to create the values
#' @param par1 See details for chosen function
#' @param par2 See details for chosen function
#' @param to.round Number of decimal places to round output values
#' @return A vector of values with length equal to num.reps
#'
compute.funct = function(num.reps,funct,par1,par2 = NA,to.round = 3){
    #If funct == 3, make the variable constant, using only par1
    if (funct == 3){
        out.vals = rep(par1,num.reps)
        }

    #If funct == 4, apply a uniform sampling, with par1 as the lower bound and par2 as the upper bound
    if (funct == 4){
        out.vals = runif(num.reps,par1,par2)
        }
    
    #If funct == 5, apply uniform sampling but round to nearest integer
    if (funct == 5){
        
        #check that par1 and par2 are integers. as.num ensures that character inputs are treated appropriately.
        if (as.num(par1) != round(as.num(par1),0) | as.num(par2) != round(as.num(par2),0)){
            stop("non-integer input to compute.funct is not allowed for funct == 5 (round to nearest integer)")
            }
        
        #Subtracting and adding 0.5 ensures that there is an even range around the integers.
        out.vals = runif(num.reps,(par1 - 0.5),(par2 + 0.5))
        out.vals = round(out.vals,0)
        }

    #If funct == 6, apply a beta distribution based on mean and variance (Not alpha and beta as is normal)
    if (funct == 6){
        out.vals = calc.beta(num.reps,par1,par2)
        }

    #If funct == 1, apply a log-normal distribution        
    if (funct == 1){
        out.vals = rlnorm(num.reps,par1,par2)
        }
    
    #If funct == 2, apply a normal distribution
    if (funct == 2){
        out.vals = rnorm(num.reps,par1,par2)
        }
  
    #Round so that you do not have a ridiculous number of digits.  to.round can be specified as NA to prevent this step.
    if (!is.na(to.round)){
        out.vals = round(out.vals,to.round)
        }

    #0 = none, but this currently is not supported by the code.
    #if (funct == 0){
        #If you wish to turn off seed dispersal, you must choose a dispersal function where all seeds remain in the cell.
        #}


    return(out.vals)
    }

#' Check species
#'
#' Check that the species that were generated are valid and realistic
#' (e.g. no spontaneous generation)
#'
#' @param vrs.out The vital rates produced during species generation
#'
check.spp = function(vrs.out){
    #Add a column to indicate whether a species is valid or not.
    vrs.out$is.error = rep(0,nrow(vrs.out))

    #Check that p01,p02,p11,p12,p22,p23,p33 each are constrained by 0 <= p <= 1
    vr.to.check = c("p01","p02","p11",'p12','p22','p23','p33')
    
    #**# Consider replacing with an apply function for speed
    for (tc in vr.to.check){
        #**# THIS IS RENAMING MY IS.ERROR column (when I use head(vrs.out), except that names still return is.error, and vrs.out$is.error still gives me the correct column!
          #Very bizarre, and appears to be a bug in R?
          #For now, just know that the second p33 is actually is.error and will respond as such!
        vrs.out$is.error = ifelse(vrs.out[tc] < 0 | vrs.out[tc] > 1 ,1,vrs.out$is.error)
        }

    #Check that p01 + p02 <= 1 AND p11 + p12 <= 1 AND p22 + p23 <= 1
    vr.check.p1 = c("p01",'p11','p22')
    vr.check.p2 = c("p02",'p12','p23')
    for (i in 1:length(vr.check.p1)){
        if (length(vr.check.p1) != length(vr.check.p2)){
            stop("There is an error in the error checking function check.spp")
        }else{
            tc1 = vr.check.p1[i]
            tc2 = vr.check.p2[i]
            vrs.out$is.error = ifelse(vrs.out[tc1] + vrs.out[tc2] > 1,1,vrs.out$is.error)
            }        
        }

    return(vrs.out)
    }

#' Set up species abundances in the landscape
#'
#' Set up species abundances in the landscape
#'
#' @param n0.lst A list of abundances for each stage for each species
#' @param spe The total number of species in the species pool
#' @param species.locs The locations of each species in the landscape
#' @param S The number of life stages in the model
#' @param initial.n A setting for initial number of species desired
#' @param ic The initial conditions read in from the Initial Conditions File
#' @param SpTraits The species traits, read in from the species file
setup.abund = function(n0.lst, spe, species.locs, S, initial.n, ic, SpTraits){

    #Assign initial numbers for each species
    #if the initial.n column is missing, set it to run the default do default assignment
    if (length(initial.n) == 0){
        initial.n = 1 # 1 is the default setting
        }

    #do default assignment if initial.n is 1
    if (initial.n == 1){
        num.adult = as.num(ic[["num.adults"]]) 
        n.adult = rep(num.adult,spe) #this is the number of adults, assuming the species is initially assigned to a cell
        n.juv = rep( (num.adult * 10) ,spe) # x10 is arbitrary
        n.seed = rep( (num.adult * 100) ,spe) #x100 is arbitrary
        }
        
    #do even biomass assignment if initial.n is 2
    if (initial.n == 2){
        starting.biomass = as.num(ic[["starting.biomass"]])
        n.adult = c()
        n.juv = c()
        n.seed = c()
        for (sp in 1:spe){
            stage.biomass = starting.biomass / (S - 1) #Divide biomass evenly among non-dispersing stages
            this.ad.n = stage.biomass %/% SpTraits[sp, "biomass.adult"] # %/% does floor division
            this.juv.n = stage.biomass %/% SpTraits[sp, "biomass.juv"]
            this.seed.n = stage.biomass %/% SpTraits[sp, "biomass.seed"]

            n.adult = c(n.adult, this.ad.n)
            n.juv = c(n.juv, this.juv.n)
            n.seed = c(n.seed, this.seed.n)
            }
        }
        
    #do approximate stable stage distribution if initial.n is 3
    if (initial.n == 3){
        stop("Option for initial.n = 3 has not yet been scripted.")
        }
    
    n0.lst = OccSetup(n0.lst,spe,species.locs,S,n.seed,n.juv,n.adult)
    
    return(n0.lst)
    }


#' Set up species locations in the landscape
#'
#' Function to implement one of several options for species assignment
#' @param n0.lst An initial abundance vector containing abundances of every species at every stage for every cell
#' @param spe The total number of species in the landscape
#' @param p The number of cells in a square landscape
#' @param S The number of stages in the matrix model
#' @param ic The initial conditions read in from the initial conditions file
#' @param settings The initial settings, read in from the initial settings file
#' @param locations.file A file giving the species locations. If no such file exists, this parameter should be set to "none".
#' @param SpTraits The Species traits, read in from the species file
#' @param rtd.c An indicator identifiying response traits. Currently broken
#' @param run.path The path for the analysis.
setup.locations = function(n0.lst, spe, S, p, ic, settings, locations.file, SpTraits, rtd.c, run.path){
    #Test code
    #locations = "C:/docs/beplants/Scripts/BEO_proof_of_concept/Analysis1/Analysis1/SpeciesData.csv"

    #Old format:
      #Locations gives the file name. The file needs to be in the inputs folder
    #New format:
      #locations should give a path to the file with the locations in it.
      #This may include complicated paths, using .. for root directory options (or is it just ".").

    #locations = as.character(settings[["locations"]])
    initial.n = settings[["initial.n"]] # An indicator for how the initial number of each life stage should be assigned.

    is.predef = 0
    #Check if locations field exists & is not NA
    if (locations.file[1] != "none"){
        is.predef = 1
        }

    #If SpatialDemography is assigning locations
    if (is.predef == 0){
        ignore.rtd = as.num(settings[["ignore.rtd"]])
        
        land.sp.num = as.num(ic[["Land.sp.rich"]])
        patch.sp.num = as.num(ic[["Loc.sp.rich"]])
        target.tot.rtd = as.num(ic[["Tot.rtd"]]) #NOTE: the actual total will be determined by species generation, but this can be used as a criteria to reject inappropriate species draws
        land.rtd = as.num(ic[["Land.rtd"]])
        loc.rtd = as.num(ic[["Loc.rtd"]])        
        
        #Assign species with specified levels of response trait diversity
        if (ignore.rtd == 0){
            rtd.setup.out = setup.rtd(SpTraits,rtd.c)
              SpTraits = rtd.setup.out[[1]]
              tot.rtd = rtd.setup.out[[2]]
              unq.rtds = rtd.setup.out[[3]]
    
            #Check that the desired level of total.rtd was acheived
            if (tot.rtd != target.tot.rtd){
                warning("Total Response Trait Diversity does not match the level specified!")
                }
    
            #Check inputs for validity
            check.inputs(spe,land.sp.num,patch.sp.num,tot.rtd,land.rtd,loc.rtd,p)
    
            #Assign species to landscape
            species.locs = assign.spp.v3(p,loc.rtd,land.rtd,land.sp.num,patch.sp.num,unq.rtds,SpTraits)
            }

        #Assign species without respect to response trait diversity            
        if (ignore.rtd == 1){
            species.locs = assign.spp.v2(p,SpTraits,land.sp.num,patch.sp.num)
            }
        
        #Combine species.locs with abundances to get n0.lst values for each species
        n0.lst = setup.abund(n0.lst, spe, species.locs, S, initial.n, ic, SpTraits)        
        }

    #If using predefined locations:
    if (is.predef == 1){
        ex.type = as.num(settings[["loc.extraction"]])

        ## 2015-02-05 Dropping support for the old locations file format.
        #If using the old locations file format
        #if (ex.type == 0){
        #    species.locs = old.style.locs(settings, run.path)
        #    n0.lst = setup.abund(n0.lst, spe, species.locs, S, initial.n, ic, SpTraits)

        ##Otherwise, use one of the more recent settings (structured this way because the other settings use some common inputs/code
        #}else{
        #Have new format of file be: LifeStage, Species, Timestep, Cells...
          #Then a row for each (present) species
                      
        #p gives number of cells
        #TimeStep = 1
        #locations = C:/docs/beplants/Scripts/BEO_proof_of_concept/Analysis1/Analysis1/SpeciesData.csv"
        TimeStep = as.num(settings[["timestep.extraction"]])

        if (typeof(locations.file) == "character"){
            in.dat = read.table(locations.file, header = T, sep = ",")        #Read in file
        }else{
            #Assume the file is in R object format.
            in.dat = locations.file
            }

        #Create a lifestage/species/timestep ID
        in.dat$ID = sprintf("%s_%s", in.dat$LifeStage, in.dat$TimeStep)
        in.dat$LifeStage = NULL
        in.dat$TimeStep = NULL
        
        Missing.Values.Seeds = 0 #Create an indicator if any year is missing.
        Missing.Values.Juvs = 0
        Missing.Values.Adults = 0
        
        #If assigning abundance via SpatialDemography, then set up species.locs list
        #It will be length p (one entry for each cell)
        if (ex.type == 2){ species.locs = make.species.locs(p) }
                            
        #Loop through lifestages & species and extract information
        for (sp in 1:spe){
            #Subet in.dat to only correspond to this species
            sub.dat = in.dat[in.dat$Species == sp, ]
            sub.dat$Species = NULL #Drop the now unnecessary column\

            #Extract information for this species
            seed.abund = id.extract("Seeds", TimeStep, sub.dat)
            juv.abund = id.extract("Juveniles", TimeStep, sub.dat)
            adult.abund = id.extract("Adults", TimeStep, sub.dat)

            #Check that all extracted information extracted correctly
            if (ex.type == 1 | ex.type == 2){
                #If a record is missing, it will come in as NULL, with length 0.
                  #Only give the message once
                if (length(seed.abund) == 0){
                    #reset vector
                    seed.abund = rep(0,p) #Set all cell entries to 0
                    #Warn user (once)
                    Missing.Values.Seeds = extract.message(seed.abund, Missing.Values.Seeds, "Seeds", TimeStep)
                    }
                if (length(juv.abund) == 0){
                    #Set up vector as 0's
                    juv.abund = rep(0,p)
                    #Warn user (once)
                    Missing.Values.Juvs = extract.message(juv.abund, Missing.Values.Juvs, "Juveniles", TimeStep)
                    }
                }
            
            #Check that an adult abundance was read in, otherwise set adult abundance to 0.
            if (length(adult.abund) == 0){
                #Set up vector as 0's
                adult.abund = rep(0,p)
                #Warn user (once)
                Missing.Values.Adults = extract.message(adult.abund, Missing.Values.Adults, "Adults", TimeStep)
                }   
                                            
            #Create indices - first place is blank - mobile seeds, second is seeds, third is juveniles, fourth is adults
              #n0.lst is a list by species (so loop through by species first.
                #within that list, is a p & S vector
                # elements correspond to:
                # S = adults
                # S - 1 = juveniles
                # S - 2 = seeds
                # S - 3 = mobile seeds, needs to be initialized at 0.
            len = p * S
            s.indices = seq((S-2),(len-2),S)
            j.indices = seq((S-1),(len-1),S)
            a.indices = seq(S,len,S)

            #If using extracted abundances, assign to cell
            if (ex.type == 1){
                #n0.lst = list(rep(0, p *S), rep(0, p*S)) #For testing purposes
                n0.lst[[sp]][s.indices] = seed.abund
                n0.lst[[sp]][j.indices] = juv.abund
                n0.lst[[sp]][a.indices] = adult.abund
                }
            
            #If using presences & predefined abundances, calculate presences, and then handle as above.
            if (ex.type == 2){
                pres = seed.abund + juv.abund + adult.abund
                proto.locs = sapply(pres,recode)
                
                #Loop through cells and set up species.locs based on presences
                for (cell in 1:length(proto.locs)){
                    this.val = species.locs[[cell]] #Get previous list of species

                    #Add species if present
                    if (proto.locs[cell] == 1){
                        this.val = c(this.val, sp) 
                        }
                    species.locs[[cell]] = this.val
                    }
                }     

            #If using only adult numbers in a hybrid approach:
              #At some point expand, and see if you can use common functions with setup.abund to streamline functionality.
            if (ex.type == 3){
                n0.lst[[sp]][a.indices] = adult.abund

                if (length(initial.n) == 0){ initial.n = 1 } #Set default behavior to be 1

                #Set up juvenile and seed abundances in a simplified manner                    
                if (initial.n == 1){
                    n0.lst[[sp]][j.indices] = adult.abund * 10
                    n0.lst[[sp]][s.indices] = adult.abund * 100
                    }
                
                if (initial.n == 2){
                    a.bio = SpTraits[sp, "biomass.adult"]
                    j.bio = SpTraits[sp, "biomass.juv"]
                    s.bio = SpTraits[sp, "biomass.seed"]
                    tot.biomass = a.bio + j.bio + s.bio
                    #Get starting overall biomass with equal biomass in each stage
                      #adult.abund = (a.bio / tot.bio) * start.bio
                    start.bio = (tot.biomass / a.bio ) * adult.abund
                    
                    n0.lst[[sp]][j.indices] = start.bio / j.bio
                    n0.lst[[sp]][s.indices] = start.bio / s.bio
                    }
                
                if (initial.n != 1 & initial.n != 2){
                    stop("That option has not yet been scripted for extraction type 3!")
                    }

                }

            }

        #Finish processing outside species loop for extraction type 2.
        if (ex.type == 2){
            #Set up as for old.style.locs
            n0.lst = setup.abund(n0.lst, spe, species.locs, S, initial.n, ic, SpTraits)
            }

        }
    return(n0.lst)
    }

#' ID Extract
#'
#' Simple extraction function to streamline code
#' @param Stage The lifestage to be extracted
#' @param TimeStep The timestep to be extracted
#' @param sub.dat The dataset containing the data to extract from.
id.extract = function(Stage, TimeStep, sub.dat){

    ex.id = sprintf("%s_%s",Stage, TimeStep)
    this.abund = sub.dat[sub.dat$ID == ex.id, ]
    this.abund = this.abund[1:(length(this.abund) - 1)] #Drop ID field
    this.abund = as.num(as.matrix(this.abund)) #Convert from list (?!?) to matrix format.
    return(this.abund)
    }


#' Give extraction message
#'
#' Give message if extraction failed for a lifestage and timestep (but only if a similar warning has not already been given.
#'
#' @param abund.vec A vector of abundances
#' @param Missing.Values An indicator for whether or not there were missing values (prevents repeat messages)
#' @param Stage The stage where the first missing value was detected
#' @param TimeStep The timestep where the first missing value was detected.
extract.message = function(abund.vec, Missing.Values, Stage, TimeStep){

    if (Missing.Values == 0){
        message(sprintf("Missing values detected for %s for TimeStep %s", Stage, TimeStep))
        Missing.Values = 1
        flush.console()
        }
    return(Missing.Values)
    }

#' Make species locations element
#'
#' Helper function for extraction type 2
#' The initial entry will be labeled "none", so as not to confuse the regex experession.
#' 
#' @param p The number of cells in a square landscape
make.species.locs = function(p){
    none = "none"
    species.locs  = list() #Initialize a variable needed for extraction type 2
    for (i in 1:p){
        species.locs = append(species.locs,list(none))
        }
    
    return(species.locs)
    }

#' Original format for locations file
#'
#' Preserves backwards compatibility, will probably be deprecated at some point.
#'
#' @param settings The settings from the read-in settings file
#' @param run.path The path for the analysis run
old.style.locs = function(settings, run.path){
    #Pull up species info using locations setting
    locations = as.character(settings[["locations"]])
    loc.file = sprintf("%sinputs/%s",run.path,locations) #run.path gives the path for the run folder and locations should give info pointing to the specific file to use
    species.locs.raw = read.table(loc.file, header = F, sep = ",")        #Read in file

    #Format of species.locs is a list entry for each cell, and the list gives the species present in the cell.
    #So, the locations file needs to have a row for each cell, and in each row, should give species values separated by commas.       
    #Reformat input to match species.locs format
    species.locs = list()
    for (a.row in 1:nrow(species.locs.raw)){
        this.row = species.locs.raw[a.row, ]
        this.row = this.row[!is.na(this.row)]
        species.locs = append(species.locs,list(this.row))
        }
    
    return(species.locs)
    }



#' Assign species to the landscape (v2)
#'
#' Function to assign species to the landscape randomly (without respect to
#' response trait diversity.
#' @param p The number of cells in a square landscape
#' @param in.spp SpTraits object
#' @param land.sp.num Number of species in the landscape
#' @param patch.sp.num Number of species in each patch
#'
assign.spp.v2 = function(p,in.spp,land.sp.num,patch.sp.num){
    #Subsample from the species pool
    sp.vec = as.num(rownames(in.spp))
    land.sp = sample.fixed(sp.vec,land.sp.num)
        
    #Loop through cells, assign the requisite number of species (?why not use apply here?    
    a.out = assign.loop(land.sp,patch.sp.num,p)
      spp.lst = a.out[[1]]
      spp.check = a.out[[2]]
    
    #Add check that appropriate number of species ended up in landscape,
    loop.count = 0
    while (spp.check != land.sp.num){

        #increment loop counter and break if you reach 1000 iterationc
        loop.count = loop.count + 1
        if (loop.count >= 1000){
            stop("Error: Could not assign species to landscape")
            }
            
        #if not, redo        
        a.out = assign.loop(land.sp,patch.sp.num,p)
          spp.lst = a.out[[1]]
          spp.check = a.out[[2]]
        
        }
        
    return(spp.lst)
    }

#' Assign loop
#'
#' Helper function for randomly assigning species to the landscape
#'
#' @param land.sp Species drawn for the landscape
#' @param patch.sp.num Number of species to draw for each patch
#' @param p The number of cells in a square landscape
assign.loop = function(land.sp,patch.sp.num,p){

    spp.lst = list()
    spp.check = c()

    #Deal with special case that each cell requires unique species
      #(the randomization approach below is slow and often fails in this circumstance)
    if (patch.sp.num * p == length(land.sp)){

        index1 = 1
        index2 = patch.sp.num
        for (cell in 1:p){
            patch.sp = land.sp[index1:index2] #Take the first n species, where n is the number of species in each patch
            spp.lst = append(spp.lst,list(patch.sp))
            spp.check = c(spp.check,patch.sp)

            #Increment the indices for the next run
            index1 = index1 + patch.sp.num
            index2 = index2 + patch.sp.num
            }    

    # Otherwise, do species assignment through a randomized approach
    }else{
        for (cell in 1:p){
            patch.sp = sample.fixed(land.sp,patch.sp.num)
            spp.lst = append(spp.lst,list(patch.sp))
            spp.check = c(spp.check,patch.sp)
            }
        }
    
    #Find out how many unique species are in the landscape
    spp.check = length(unique(spp.check))
    
    return(list(spp.lst,spp.check))
    }

#Function to check that inputs are valid and logical
#' Check inputs
#'
#' Function to check that inputs for random species assignment are valid and
#' logical.  For example, local patch diversity cannot exceed landscape diversity
#' @param tot.sp.num total number of species in the species pool
#' @param land.sp.num Number of species in the landscape
#' @param patch.sp.num Total number of species in each patch
#' @param tot.rtd total response trait diversity in the species pool
#' @param land.rtd total response trait diversity in the landscape
#' @param loc.rtd Response trait diversity in each patch
#' @param p The number of cells in a square landscape
#'
check.inputs = function(tot.sp.num,land.sp.num,patch.sp.num,tot.rtd,land.rtd,loc.rtd,p){

    #Check that species numbers are legitimate
    if (tot.sp.num < land.sp.num){
        stop("Total species number cannot be less than landscape species number!")}
    if (land.sp.num < patch.sp.num){
        stop("Landscape species number cannot be less than patch species number!")}
    if (tot.rtd < land.rtd){
        stop("Total response trait diversity cannot be less than landscape response trait diversity")}
    if (land.rtd < loc.rtd){
        stop("Landscape response trait diversity cannot be less than patch response trait diversity")}
    if (land.sp.num > (p * patch.sp.num)){
        stop("Landscape species number cannot exceed number of cells times patch species number")}
    if (land.rtd > (p * loc.rtd)){
        stop("Landscape response trait diversity cannot exceed number of cells times patch response trait diversity")}
    if (land.rtd > land.sp.num){
        stop("Landscape response trait richness cannot exceed landscape species number!")}
    if (loc.rtd > patch.sp.num){
        stop("Patch response trait richness cannot exceed patch species number!")}

    #NOTE: Are there other criteria that need to be included?
    }

#' Estimate Response Trait Diversity
#'
#' This function takes a dataframe with species information, and calculates
#' the response trait diversity present (giving each species a response trait
#' diversity number that corresponds to a unique combination.  Some formatting
#' caveats apply. Dispersal traits need to be in columns 11 - 13, and any
#' remaining response traits in columns 14+
#'
#' @param SpTraits A dataframe with species information
#' @param rtd.c Currently non-functional input
#' @return The SpTraits dataframe with a unique code assigned to each response
#' type
#'
setup.rtd = function(SpTraits,rtd.c = 0){

      #Identify response traits
        #**# Sub-optimal approach to start with for selecting response traits (because it is hardwired to column position
      #If there are other response traits, include them
      if (ncol(SpTraits) > 13){
          rtd.index = c(11,12,13,14:ncol(SpTraits))
      #If not, only include mandatory dispersal traits
      }else{
          rtd.index = c(11,12,13)
          }
      
      rtd.mat = SpTraits[ , rtd.index]

      #NOTE: Consider adding code to classify response traits into categories

      #Create a funcitonal unit matrix
      rtd.fu.mat = rtd.mat[!duplicated(rtd.mat), ]

      #Get unique response trait values
      tot.rtd = nrow(rtd.fu.mat)
      unq.rtds = seq(1,nrow(rtd.fu.mat))

      #Assign numbers to it
      rtd.fu.mat["RTD_ID"] =  unq.rtds
      
      #Assign those numbers to species matching the functional unit
      #**# This could be optimized
      #Join back to rtd.mat
      rtd.id = c()
      for (a.row in 1:nrow(rtd.mat)){
          this.row = listtotext(rtd.mat[a.row, ],",")
          
          for (b.row in 1:nrow(rtd.fu.mat)){
              e.p = ncol(rtd.fu.mat) - 1 #Set the endpoint to exclude the added column
              test.row = listtotext(rtd.fu.mat[b.row,1:e.p],",")
              
              if (this.row == test.row){
                  rtd.val = rtd.fu.mat[b.row,ncol(rtd.fu.mat)] #Take value from last column (added - has RTD_ID)
                  rtd.id = c(rtd.id,rtd.val)
                  break #End this loop and go to the next row in rtd.mat
                  }
              }
          }
                
      #Join from rtd.mat to SpTraits
      #NOTE: This requires that SpTraits is in the same order as rtd.id. This is suboptimal, and could be broken by upstream changes.
      SpTraits["RTD_ID"] = rtd.id 

    return(list(SpTraits,tot.rtd,unq.rtds))
    }

#DD# Figure out how to have the documentation for this and v2 appear together
#' Assign species to the landscape (v3)
#'
#' Assign species to the landscape, meeting certain response trait diveristy
#' constraints.
#'
#' @param p The number of cells in a square landscape
#' @param loc.rtd The response trait diversity in each patch
#' @param land.rtd The response trait diversity in the landscape
#' @param land.sp.num The number of species in the landscape
#' @param patch.sp.num The number of species in each patch
#' @param unq.rtds unq.rtds
#' @param SpTraits Species trait information from species file
#' @note This is not an optimal implementation, and could benefit from some
#' optimization.  This is especially a problem when there is only one or a few
#' valid configurations of species.  This is because species are chosen and
#' removed randomly, so it takes a long time (if ever) to find the valid
#' configuration(s).
#' @return Returns a list of species locations in the landscape
#'
assign.spp.v3 = function(p,loc.rtd,land.rtd,land.sp.num,patch.sp.num,unq.rtds,SpTraits){

    #Set up two lists to contain warning messages.  List 1 is whether the warning occurred at all, List 2 is whether the warning occured on the last iteration
    warn.lst1 = list(0,0,0,0,0,0)

    #Function setup
    do.over = 1         #Indicator variable to determine whether a new draw is needed
    d.o.count = 0
    while (do.over == 1){

        #Reset warning list 2
        warn.lst2 = list(0,0,0,0,0,0)

    
        d.o.count = d.o.count + 1
        if (d.o.count > 10){
            #Issue warnings
            do.warn(warn.lst1,warn.lst2)
            stop("Assign.spp.v3 failed for one or more reasons.  See warnings for more details.")
            }
    
        #Get a list of the cells in the landscape
        species.locs = rep(list("NI"),p) #NI for not initialized
        #Create a vector of which response traits were used and in which cells they were used
        # note: this will increase by increments of loc.rtd, so when random cells are replaced, the appropriate part of this vector can be replaced simultaneously
        used.rt.vec = c()    
        #Create a vector of used species to assess whether all species have been assigned
        used.spp.vec = c()
        
        #Select a subset of response traits based on specificed landscape response trait diversity levels
        rtds.to.use = sample.fixed(unq.rtds, land.rtd)
    
        #Create a subset of species that correspond to the subset of response traits present
        sp.subset = SpTraits[SpTraits$RTD_ID %in% rtds.to.use, ] #Gets species where the response type matches the ones selected for use
    
        s.a.out = s.a.check(SpTraits,sp.subset,land.sp.num,land.rtd,unq.rtds,rtds.to.use,"landscape",warn.lst1,warn.lst2)
          rtds.to.use = s.a.out[[1]]
          sp.subset = s.a.out[[2]]
          do.over = s.a.out[[3]]
          warn.lst1 = s.a.out[[4]]
          warn.lst2 = s.a.out[[5]]
    
        if (do.over != 1){
            #Go through each cell in the landscape
            for (cell in 1:p){
        
                p.a.out1 = patch.assign.v2(rtds.to.use,loc.rtd,sp.subset,patch.sp.num,warn.lst1,warn.lst2)
                  loc.spp = p.a.out1[[1]]
                  rtds.pres = p.a.out1[[2]]
                  do.over = p.a.out1[[3]]
                  warn.lst1 = p.a.out1[[4]]
                  warn.lst2 = p.a.out1[[5]]

                #If the patch.assign.v2 fails, try a new version of the loop - break out of for loop and reset
                if (do.over == 1){
                    break
                    }
                
                species.locs[cell] = list(loc.spp)
                used.rt.vec = c(used.rt.vec, rtds.pres)
                used.spp.vec = c(used.spp.vec, loc.spp)               
                }
            }

        if (do.over != 1){
            #Check to make sure all response traits selected at the landscape level have been assigned to the landscape
            rt.test = unique(used.rt.vec)
            us.test = unique(used.spp.vec)
            wcount = 0
            while (length(rt.test) != land.rtd | length(us.test) != land.sp.num){
                warn.message5 = "Some cells needed to be resampled to meet landscape requirements.  Data are no longer truly random"
                warn.lst1[[5]] = warn.lst2[[5]] = warn.message5
                #warning("Some cells needed to be resampled to meet landscape requirements.  Data are no longer truly random")
                #Select a random cell
                ctr = cell.to.replace = sample(p,1) #note: this use of sample pulls a number from 1:p
                #Replace its values
                pa.out2 = patch.assign.v2(rtds.to.use,loc.rtd,sp.subset,patch.sp.num,warn.lst1,warn.lst2) #These are from the old version: (loc.rtd,patch.sp.num,rt.lst,rt.types)
                species.locs[[ctr]] = pa.out2[[1]]
                used.rt = pa.out2[[2]] #Get used response traits
                used.spp = pa.out2[[1]] #Get used species
                do.over = pa.out2[[3]]
                warn.lst1 = pa.out2[[4]]
                warn.lst2 = pa.out2[[5]]
                
                #If patch assign fails, break out of loop, and try again
                if (do.over == 1){
                    break
                    }
                
                #update rt.test & us.test
                #The logic for the start and end points is as follows:
                  # 1 will make it so the starting cell is 1
                  # (ctr - 1) * loc.rtd will offset the starting cell based on the cell number multiplied by the number of local response traits (in the vector being used).  it is ctr - 1 so that when ctr is 1, this quantity is zero, and no offset is employed.
                  # The end point is the same as the start point, plus (loc.trd - 1).  Add local response trait diversity, but subtract one because the range includes the first number of the index
                  # Same logic applies to patch.sp.num        
                aa = used.rt.index.start = 1 + (ctr - 1) * loc.rtd
                bb = used.rt.index.end = 1 + (ctr - 1) * loc.rtd + (loc.rtd - 1)
                cc = used.spp.index.start = 1 + (ctr - 1) * patch.sp.num
                dd = used.spp.index.end = 1 + (ctr - 1) * patch.sp.num + (patch.sp.num - 1)
                used.rt.vec[aa:bb] = used.rt #Replace the old cell's values with the new values
                used.spp.vec[cc:dd] = used.spp #Replace the old cell's values with the new values
                #Update criteria used to test the while loop
                rt.test = unique(used.rt.vec)
                us.test = unique(used.spp.vec)
                        
                #repeat loop until conditions are satisfied or loop fails.
                
                # Add a break to avoid getting infinitely stuck for impossible conditions
                wcount = wcount + 1
                if (wcount >= 1000){
                    warn.message6 = "Something went wrong when trying to assign landscape-wide response trait diversity or total species numbers"
                    warn.lst1[[6]] = warn.lst2[[6]] = warn.message6
                    #warning(warn.message6)
                    do.over = 1
                    break
                    }
                }
            }
        }
    
    #Issue warnings
    do.warn(warn.lst1,warn.lst2)
        
    return(species.locs)
    }

#DD# Figure out how this is different from check.inputs & decide if the two should be documented together
#' Check species availability
#'
#' Check that the number of species available is greater than the number of
#' species needed based on response trait diversity
#' If not, re-draw until the criteria is reached or until the code gives up.
#'
#' @param my.set my.set
#' @param my.subset my.subset
#' @param target.sp.num target.sp.num
#' @param target.rtd target.rtd
#' @param draw.pool draw.pool
#' @param my.draw my.draw
#' @param chk.type chk.type
#' @param warn.lst1 A list of warnings
#' @param warn.lst2 A second list of warnings
#'
s.a.check = function(my.set,my.subset,target.sp.num,target.rtd,draw.pool,my.draw,chk.type,warn.lst1,warn.lst2){
  #(SpTraits,sp.subset,land.sp.num,land.rtd,unq.rtds,rtds.to.use,"landscape") #For landscape check
  #sp.subset,loc.sp.subset,patch.sp.num,loc.rtd,rtds.to.use,loc.rtds.to.use,"patch" #For species check
    do.over = 0 #If there is an error, this will change to 1, and a new draw will be tried

    #If entered, hopefully only restriction will occur.  If a new draw occurs, issue a warning about non-randomness.
    w.count = 0
    d.count = 0 #I want a counter 
    while (nrow(my.subset) != target.sp.num){
        w.count = w.count + 1
        
        if (w.count > 10){
            warn.message1 = sprintf("s.a.check could not assign species. \n Not enough species per response type to get required %s species numbers and meet landscape response trait diversity target\n based on %s draws. \n Will try again with a different main draw",chk.type,w.count)
            warn.lst1[[1]] = warn.lst2[[1]] = warn.message1            
            #warning(warn.message1)
            
            #break out of the loop and try a new overall draw
            do.over = 1
            break
            }
        if (nrow(my.subset) < target.sp.num){
            #Do a new draw
            d.count = d.count + 1
            my.draw = sample.fixed(draw.pool, target.rtd)
            my.subset = my.set[my.set$RTD_ID %in% my.draw, ] #Gets species where the response type matches the ones selected for use
            }
        
        #Restrict down to appropriate species number & check that RTD is still appropriate
        if (nrow(my.subset) > target.sp.num){
            spp.subset.out = spp.subset(my.subset,target.rtd,target.sp.num,warn.lst1,warn.lst2)
             my.subset = spp.subset.out[[1]]
             do.over = spp.subset.out[[2]]
             warn.lst1 = spp.subset.out[[3]]
             warn.lst2 = spp.subset.out[[4]]           
            }        
        }

    #Issue warning that a new draw(s) was taken (i.e., the draws are no longer completely random)
    if (d.count >= 1){
        warn.message2 = "Species assignment not completely random as initial response trait type draws did not contain sufficient species to meet required landscape level species diversity"
        warn.lst1[[2]] = warn.lst2[[2]] = warn.message2 
        #warning(sprintf("%s. %s new draws taken",warn.message2,d.count))
        }

    return(list(my.draw,my.subset,do.over,warn.lst1,warn.lst2))
    }

#' Species subset
#'
#' Function to reduce species number from a regional species pool to the target
#' species number.
#'
#' @param my.subset A subset
#' @param target.rtd a response trait diversity target
#' @param target.sp.num The target species number
#' @param warn.lst1 A list of warnings
#' @param warn.lst2 A second list of warnings
#'
spp.subset = function(my.subset,target.rtd,target.sp.num,warn.lst1,warn.lst2){

    do.over = 0

    #If subset has too many species, restrict this subset to the desired number of species
    spp.draw = sample.fixed(rownames(my.subset),target.sp.num)
    my.subset2 = my.subset[rownames(my.subset) %in% spp.draw, ]

    ms2.rtd = unique(my.subset2$RTD_ID)
    ww.count = 0
    while (length(ms2.rtd) != target.rtd){
        ww.count = ww.count + 1
        if (ww.count > 100){
            warn.message3 = sprintf("spp.subset function could not successfully subset species \n and maintain response trait diversity after %s iterations.\n so a new draw was evaluated",ww.count)
            warn.lst1[[3]] = warn.lst2[[3]] = warn.message3
            #warning(warn.message3)
            do.over = 1
            my.subset2 = data.frame() #Set it as a blank dataframe, this will give nrow = 0, so will fail the while loop criteria in s.a.check and lead to a new draw
            break
            }
            
        spp.draw = sample.fixed(rownames(my.subset),target.sp.num)
        my.subset2 = my.subset[rownames(my.subset) %in% spp.draw, ]

        #Check that desired response trait diversity is still present in sub-setted species, if not, try again.
        ms2.rtd = unique(my.subset2$RTD_ID)
        }

    return (list(my.subset2,do.over,warn.lst1,warn.lst2))
    }    

#ll# Left off here in updating documentation. Just couldn't take it anymore!

#' Patch level species assignment
#'
#' Function to do local assignment
#' (function so that it can be invoked at multiple points in the code)
#' @param rtds.to.use rtds.to.use
#' @param loc.rtd Local response trait diversity level
#' @param sp.subset sp.subset
#' @param patch.sp.num Number of species in a patch
#' @param warn.lst1 First warning list
#' @param warn.lst2 Second warning list
#'
patch.assign.v2 = function(rtds.to.use,loc.rtd,sp.subset,patch.sp.num,warn.lst1,warn.lst2){

    do.over = 0

    #Draw a subset of response traits to indicate which should be present at this patch
    loc.rtds.to.use = sample.fixed(rtds.to.use,loc.rtd)
    
    #For each patch, select a subset of response traits to match specified patch levels of response trait diversity levels
    loc.sp.subset = sp.subset[sp.subset$RTD_ID %in% loc.rtds.to.use, ]        

    #check that loc.sp.subset has enough species
    s.a.out2 = s.a.check(sp.subset,loc.sp.subset,patch.sp.num,loc.rtd,rtds.to.use,loc.rtds.to.use,"patch",warn.lst1,warn.lst2)
      loc.rtds.to.use = s.a.out2[[1]]
      loc.sp.subset = s.a.out2[[2]]
      do.over = s.a.out2[[3]]
      warn.lst1 = s.a.out2[[4]]
      warn.lst2 = s.a.out2[[5]]

    if (do.over != 1){

        #Then for each patch/cell, draw species from the pool restricted by chosen response traits until the required species number is reached
        species.pool = row.names(loc.sp.subset) #Get list of potential species as a vector
        #Set initial conditions to be changed by while loop
        obs.rtd = 0
        w.count = 0
        
        #Run loop until desired response trait diveristy is acheived
          #because obs.rtd is initially defined as 0, the loop will be entered the first time & executed at least once
        while (obs.rtd != loc.rtd){
            w.count = w.count + 1
            if (w.count >= 1000){
                warn.message4 = sprintf("patch.assign.v2: Unable to assign response trait diversity to local patch in %s attempts.\n Possible error in code (e.g., patch species number lower than required response trait diversity),\n or code requires optimization or an increase in the number of allowed iterations.\n Will try a new rtd combination",w.count)
                warn.lst1[[4]] = warn.lst2[[4]] = warn.message4
                #warning(warn.message4)
                do.over = 1
                break
                }
                           
            loc.spp = sample.fixed(species.pool,patch.sp.num) #Get the desired number of species from the species pool
            patch.subset = loc.sp.subset[rownames(loc.sp.subset) %in% loc.spp, ]  #Subset the dataframe to the selected species
            rtds.pres = unique(patch.subset$RTD_ID)  #Get a vector of their RTD's
            obs.rtd = length(rtds.pres) #Find out how many of the rtd values are unique to check that the patch has the right amount of rtd
            }
        }

    #Code needs something to return for loc.spp, otherwise it will complain that it was undefined.
    if (do.over == 1){
        rtds.pres = NA
        loc.spp = NA
        }

    return(list(loc.spp,rtds.pres,do.over,warn.lst1,warn.lst2))
    }

#' Issue warnings
#'
#' Function to issue warnings
#'
#' @param warn.lst1 First warning list
#' @param warn.lst2 Second warning list
do.warn = function(warn.lst1,warn.lst2){

    #Convert warning lists to text
    warn.lst1.out = listtotext(warn.lst1,"\n")
    warn.lst2.out = listtotext(warn.lst2,"\n")

    no.warn = listtotext(rep(0,6),"\n")

    #Set up warning output message
    warn.out1 = sprintf("The following overall warning messages were issued:\n%s", warn.lst1.out)    
    warn.out2 = sprintf("The following warnings apply to the last model run:\n%s", warn.lst2.out)

    #Actually issue warnings here (but only if there actually are warnings!)
    if (warn.lst1.out != no.warn){
        warning(warn.out1)
        }
    if (warn.lst2.out != no.warn){
        warning(warn.out2)
        }
    }

#' Check for environmental change
#'
#' Test if there will ever be environmental changes in a model run
#' @param env.c.freq Frequency of environmental change
#'
change.check = function(env.c.freq){
    is.change = 0
    if (max(env.c.freq) > 0){
        is.change = 1
        }
    return(is.change)
    }

#' Get conditioning layer
#'
#' Function to get the values from the conditioning layer
#' @param target.cond.lyr The layer to use for creating conditional values
#' @param landscape A list object containing the environmental layer values for each cell in the landscape
#' @param landscape.identifiers A list of single letter identifiers for each environmental layer
#'
cv.setup = function(target.cond.lyr, landscape,landscape.identifiers){
    to.find = sprintf("\\b%s\\b",target.cond.lyr) #\b looks for boundaries - so this makes sure only the search term matches, and not larger numbers that include the search term.  The \ needs a \ in order for R to interpret it as a \, and not as something else (i.e., an escape character). Otherwise a search for species 1 will return wrong matches, e.g., 41 or 14!
    cond.lyr.id = grep(to.find,landscape.identifiers)
    cond.lyr = list(landscape[[cond.lyr.id]]) #Get the actual layer from the landscape.  Obviously, this requires it to have already been created.
    return(cond.lyr)
    }

#' Set up env.info variable
#'
#' Function to set up env.info and reduce code redundancy and increase
#' streamlining
#' @param env.type An indicator for what type of environmental values are present (e.g., cover, normal random) 
#' @param my.env Information about the environmental layers
#' @param index An index
#'
setup.env.info = function(env.type,my.env,index){

    param1 = my.env[index,"param1"]
    param2 = my.env[index,"param2"]

    if (env.type == "cover"){
        cover.levels = as.character(my.env[index,"cover.levels"])
        cover.levels = as.num(strsplit(cover.levels,";",fixed = T)[[1]]) #The [[1]] is because strsplit puts the vector I want in a list.
        
        param1 = as.num(param1)
        env.info = list(env.type,cover.levels,param1)
        }
    # NOTE: This is an artifact, as it could be set up using the function codes
    if (env.type == "rnorm"){
        env.info = list(2,as.num(param1),as.num(param2))
        }
        
    #Catch-all to handle numeric codes
    if (env.type != "cover" & env.type != "rnorm"){
        env.info = list(as.num(env.type),as.num(param1),as.num(param2))
        }

    return(env.info)
    }

#DD# Think about how to best document the below suite of functions
#' Update my.env settings
#'
#' Function to change my.env due to environmental change
#'
#' @param my.env Information about the environmental layers
#' @param ev an index for which environmental layer to change
#' 
change.cover.my.env = function(my.env,ev){
    #Get previous cover levels
    old.cover.levels = as.character(my.env[ev,"cover.levels"])
      old.cover.levels = as.num(strsplit(old.cover.levels,";")[[1]])
    #Get magnitude of change
    c.mag = as.character(my.env[ev, "env.change.mag"])
      c.mag = as.num(strsplit(c.mag,";")[[1]])
    #Get new cover levels
    new.cover.levels = old.cover.levels + c.mag
    
    #check that new cover levels == 1
    if (sum(new.cover.levels) != 1){
        new.cover.levels = old.cover.levels
        warning("Change in cover levels resulted in impossible values, no change was made")
        }
    
    #Update my.env
    my.env[ev,"cover.levels"] = listtotext(new.cover.levels,";")
    
    return(my.env)
    }

#DD# See note on change.cover.my.env
#' Markov change
#'
#' Function to do environmental change via a markov chain (transition probabilities)
#'
#' @param l.lyr The current landscape layer values
#' @param my.env Information about the environmental layers
#' @param ev An indicator for which environmenal layer to use
#' @param p The number of cells in the landscape
#' @param cond.lyr Another layer to use for conditional setup
#' @param cond.vals Values in the other layer to use as suitable
markov.change = function(l.lyr,my.env,ev,p,cond.lyr = NA,cond.vals = NA){
    #test vals
    # l.lyr = c(0,1,0,0,1,0,1,0,1)
    # c.mag = "0:1;0.681:1:0.275:1"
    # cond.lyr = c(1,1,2,1,2,1,1,2,1)
    # cond.vals = list(1)

    new.l.lyr = l.lyr #Create a new layer to store the results
    
    c.mag = as.character(my.env[ev, "env.change.mag"])
      c.mag = strsplit(c.mag,";")[[1]]
    l.values = c.mag[1]
      l.values = as.num(strsplit(l.values,":")[[1]])
    t.probs = c.mag[2]
      t.probs = as.num(strsplit(t.probs,":")[[1]])
      t.probs = matrix(t.probs,nrow = length(l.values),byrow = T) #Makes a matrix, where each row corresponds to transition probabilities for a value
      
    #Go through values in the landscape, and do transitions
    for (cell in 1:p){
        cell.val = l.lyr[cell]

        #Indicator variable for whether or not to update this cell
        do.trans = 1        
        #If conditioning on another layer, check that the other layer is of a suitable type

        if (!is.na(cond.lyr[1])){
            do.trans = 0
            cond.lyr.cell.val = cond.lyr[cell]
            #Go through the list of acceptable values
            for (test.val in cond.vals){
                #If the conditioning layer's cell value is in the list of acceptable values, do the transition
                if (cond.lyr.cell.val == test.val){
                    do.trans = 1
                    }
                }
            }
        
        #If the cell is not of a suitable type, set it's value to 0 (regardless of what it was before - if the conditioning layer has changed, this prevents unsuitable values present here
        if (do.trans == 0){
            new.l.lyr[cell] = 0
        }else{
        
            l.v.count = 0
            #Go through values indexing transition rates
            for (l.v in l.values){
                l.v.count = l.v.count + 1
                #If the cell value matches the transition value index, do the transition
                if (cell.val == l.v){
                    #Draw a random number from a uniform distribution
                    rand.val = runif(1,0,1)
                    t.probs.for.val = t.probs[l.v.count, ]
                    lower = 0
                    upper = 0
                    count = 0
                    #min/max is to use the values from the values list as indices
                      #Note that gaps between values are not allowed! e.g. 0,1,2 is okay, but 0,2 is not
                    for (j in min(l.values):max(l.values)){
                        count = count + 1
                        upper = t.probs.for.val[count]
                        if (rand.val < upper & rand.val >= lower){
                            new.l.lyr[cell] = j #j corresponds to the value for the given transition probability
                            break #No sense evaluating the rest of the loop once an appropriate match is found
                            }
                        lower = upper # Reset the lower value to be one increment higher
                        }
                    break #If a match was found above, no need to continue the for loop.
                    }
                }
            }
        }

    return(new.l.lyr)
    }

#' Validity Check
#' 
#' Check that probabilities of environmental change are actually probabilities
#' (and if not, adjust them)
#' @param x An input probability to be checked
#'
validity.check = function(x){
    if (x > 1){
        x = 1
        warning("A probability exceeded 1, and was set to 1 instead")
        }
    if (x < 0){
        x = 0
        warning("A probability went below 0, and was set to 0 instead")
        }
    return(x)
    }

#DD# See note on change.cover.my.env
#' Change Transition Rates
#'
#' Function to change the transition rates used for landscape change 
#'
#' @param my.env Information about the environmental layers
#' @param ev An indicator for which environmenal layer to use
change.transition.rates = function(my.env,ev){

    #unpack c.mag
    c.mag = as.character(my.env[ev, "env.change.mag"])
        c.mag = strsplit(c.mag,";")[[1]]
    t.probs = c.mag[2]
      t.probs = as.num(strsplit(t.probs,":")[[1]])
    t.probs.change = c.mag[3]
      t.probs.change = as.num(strsplit(t.probs.change,":")[[1]])            
    #Update transition probabilities
    t.probs = t.probs + t.probs.change

    #If any probs < 0, or > 1, reset to 0 & 1 and issue a warning
    t.probs = sapply(t.probs,validity.check)

    #Update c.mag[2]
    c.mag[2] = listtotext(t.probs,":")

    #Update my.env to include the changed transition probabilities
    my.env[ev, "env.change.mag"] = listtotext(c.mag,";")

    return(my.env)
    }

#DD# Think about routing change.transition.rates & other sub-functions to this documentation
#' Implement environmental change
#'
#' Make environmental changes to a landscape layer according to a set of change
#' instructions.  The instructions are in the my.env input, which contains the
#' following elements "env.change.type", "env.change.mag", and "cond.lyr".
#' "env.change.type" contains the type of change to take place, "env.change.mag"
#' contains details on the magnitude of those changes, and "cond.lyr" contains
#' information on what other layers might affect the change.  The change types
#' depend on whether or not another layer is being used to inform the change
#' process (if so, it is a conditional landscape element). See
#' \link[spatialdemography]{env.change.type} for more details.
#'
#' @param my.env A dataframe containing instructions for how the environment is
#' to change. See \link[spatialdemography]{env.file} for details.
#' @param ev An indicator variable indicating which landscape element is being
#' modified
#' @param landscape A list of environmental layers
#' @param landscape.identifiers A list of identifiers for the environmental
#' layers
#' @param p The number of cells in the landscape
#' @param run.path The path for this model run (only applicable if c.type == "from.file")
#' @param env.lbl A vector containing each environmental layer name in order (only applicable if c.type == "from.file")
#' @param s.lbl A label for the scenario being considered(only applicable if c.type == "from.file")
#' @param landscape.dir The directory containing the landscape layers for when changes are read in from file
#' @param lnd.lbl A scenario specific label identifying the landscape layers to be used.
#' @param change.count An indicator for which environmental change is being used.
#' Note that this will refer to the change PREVIOUS to the current change, as it
#' is updated at the END of the change process. (only applicable if c.type == "from.file")
#'
do.env.change = function(my.env,ev,landscape,landscape.identifiers,p,run.path = NA,env.lbl = NA,s.lbl = NA,landscape.dir = NA,lnd.lbl = NA, change.count = NA){

    #Set up for implementing the change
    l.lyr = landscape[[ev]]
    c.type = as.character(my.env[ev, "env.change.type"])
    cond.lyr = as.character(my.env[ev, "cond.lyr"]) #Determines whether landcover depends on another landcover

    my.env$env.change.mag = as.character(my.env$env.change.mag) #Convert from factor to character to avoid problems.  May want to do this earlier in the code?
    
    #If there is no layer to condition on, do normal change
    if (is.na(cond.lyr)){
    
        #This will rearrange the elements in the landscape, but not change their values.
        # Seems desirable under some circumstances - can have environmental change, but maintain landscape properties.
        if (c.type == "swap"){
            new.l.lyr = sample(l.lyr,length(l.lyr)) 
            }

        #Stochastic change modeled with a markov chain
          #Transition probabilities govern environmental change
        if (c.type == "markov"){
            #c.mag needs to have transition probabilities for each layer
              #layer values are given first, followed by transitions for first layer value (to first, to second, etc), then transitions for the second layer value (to first, to second, etc)
                #Transitions should be cumulative probabilities
              #e.g., 0:1;0.681:1:0.275:1
            new.l.lyr = markov.change(l.lyr,my.env,ev,p)
            }
                
        #Systematic change with stochastic variation modeled with a markov chain
          #Here, the transition probabilities also change with time
        if (c.type == "markov.changing.transitions"){
            #here c.mag needs to have 3 main elements
              #values potentially found in the landscape (need to be consecutive
              #transition probabilities for these values to all other values
              #how the transition probabilities change with time
                #e.g., 0:1;0.681:1:0.275:1;-0.05:0:-0.025:0
                #this would lead to a decrease of 0.05 in the transition probability of going from 0 to 0 (which will lead to a corresponding increase in going from 0 to 1, since there are only two
                #And there would be an decrease in going from 1 to 0, with a corresponding increase in going from 1 to 1.
                  #This would correspond to landuse intensification, with mowing becoming more frequent with time.
            #Change my.env to reflect new changed environmental transition rates
            my.env = change.transition.rates(my.env,ev)
            
            #implement markov chain process
            new.l.lyr = markov.change(l.lyr,my.env,ev,p)
            }
          
        #This changes the cover proportions, then re-assigns cover values to the landscape randomly
        if (c.type == "cover.change.and.swap"){
        
            #Update my.env
            my.env = change.cover.my.env(my.env,ev)
            
            #Update layer
            env.info = setup.env.info("cover",my.env,ev)
            base.lyr = get.base.env.lyr(env.info,p)
            new.l.lyr  = get.env.lyr(base.lyr) #This was separated into two steps to allow for multiple configurations of the same underlying values.  I've broken that functionality in the code rewrite at the moment.
            }

        #This is basically the same approach as a markov chain, but with a continuous state variable
        if (c.type == "rnorm.prob"){
            c.mag = as.character(my.env[ev, "env.change.mag"])
            c.mag = as.num(strsplit(c.mag,";")[[1]])
        
            new.l.lyr = l.lyr #Set the output layer to be equal to the input layer. It will then be updated.
        
            p.change = c.mag[1]
            delta.mean = c.mag[2]
            change.sd = c.mag[3]
            #c.mag format:
              #probability a cell will change
              #change in mean for a cell that changes
              #standard deviation around the mean for a cell that changes
              #e.g., 0.8;0.25;1

            test.probs = runif(p,0,1)

            for (a in 1:p){
                new.val = l.lyr[a]
                #Test if cell changes
                if (test.probs[a] <= p.change){
                    
                    new.mean = new.val + delta.mean
                    new.val = rnorm(1,new.mean,change.sd)
                    }
                new.l.lyr[a] = new.val #Stays the same if new.val is not reassigned, otherwise it changes to the new value (which can be the same as the old value)
                }
            }
  
        #Based on the temperature data, it looks like the relative rankings of sites remained the same, while the values changed
          #Consequently, the plan is to apply a change to the entire landscape based on c.mag
        if (c.type == "landscape.wide.change"){
            c.mag = as.character(my.env[ev, "env.change.mag"])
              c.mag = as.num(strsplit(c.mag,";")[[1]])
            o.mean = c.mag[[1]] #This gives the overall mean for the landscape.  It will be incremented by c.mean, each time there is change
            o.sd = c.mag[[2]]   #This gives the overall standard deviation for the variable draw
            c.mean = c.mag[[3]] #This gives the change in overall mean
            c.sd = c.mag[[4]]   #This gives the change in overall standard deviation

            #change o.mean and o.sd based on long-term change trends
            o.mean = o.mean + c.mean
            o.sd = o.sd + c.sd
            
            #Make sure sd does not go below 0!
            if (o.sd < 0){
                o.sd = 0
                warning("overall standard deviation was reduced below zero.  Set to zero instead")
                }
            
            #update my.env to reflect changed means
            c.mag = c(o.mean,o.sd,c.mean,c.sd)
            c.mag = listtotext(c.mag,";")
            my.env[ev, "env.change.mag"] = c.mag
        
            #The change is based on a mean of 0, because the mean is already added to the landscape, and this just gives the random change component
            change = rnorm(1,0,o.sd)
            
            #Get deviations from landscape mean for each cell
            old.l.mean = mean(l.lyr)
            l.resid = l.lyr - old.l.mean #Get residuals for cells, so that higher cells remain higher and lower cells remain lower
            #New layer is the landscape mean, plus the site's local idiosyncracities, plus the random draw based on the change
            new.l.lyr = round((rep(o.mean,p) + l.resid + change),2)
            }
        
        #Draw a random value from a log-normal distribution
        #NOTE: This could be replaced with the numeric codes and "fx" type
        if (c.type == "logn"){
            c.mag = as.character(my.env[ev, "env.change.mag"])
              c.mag = as.num(strsplit(c.mag,";")[[1]])
            ln.mean = c.mag[[1]] #This gives the mean
            ln.var = c.mag[[2]]   #This gives the sd
        
            new.l.lyr = rlnorm(p,ln.mean,ln.var)
            }
        
        #Draw a random value from a beta value with mean and variance specified in c.mag
        if (c.type == "beta"){
            c.mag = as.character(my.env[ev, "env.change.mag"])
              c.mag = as.num(strsplit(c.mag,";")[[1]])
            b.mean = c.mag[[1]] #This gives themean for the beta distribution (NOT alpha)
            b.var = c.mag[[2]]   #This gives the variance for the beta distribution.  This is not the same as a standard deviation or sd^2
            
            new.l.lyr = calc.beta(p,b.mean,b.var) #calc.beta is a custom function from Jakob
            }

        #Use one of the functions from compute.funct
        if (c.type == "fx"){
            c.mag = as.character(my.env[ev, "env.change.mag"])
              c.mag = as.num(strsplit(c.mag,";")[[1]])
            #funct = c.mag[[1]]
            #par1 = c.mag[[2]]
            #par2 = c.mag[[3]]
            
            new.l.lyr = get.base.env.lyr(c.mag,p)
            }
       
        #Read landscape in from file
        if (c.type == "from.file"){
            lbl = env.lbl[ev]
            c.count = change.count + 1 #change.count + 1 is to get the file from the current change, as change count has not yet been updated
            new.l.lyr = read.landscape.lyr(landscape.dir,lbl,lnd.lbl,c.count,p)
            }

    #Otherwise, include the conditional layer in implementing change
    }else{
        #Re-setup env.info
        env.type = as.character(my.env[ev,"env.type"])
        env.info = setup.env.info(env.type,my.env,ev)

        cond.vals = as.character(my.env[ev, "cond.vals"]) #Determines which values of the landcover depend on the other landcover
          cond.vals = list(as.num(strsplit(cond.vals,";",fixed = T)[[1]])) #what an awful piece of code.  Outside nesting is it needs to be a list at the end.  Next level is as.num, which requires a non-list input.  Hence the [[1]] after strsplit, because strsplit creates a list!  Splits on ";"        
        cond.lyr = cv.setup(cond.lyr,landscape,landscape.identifiers) #This reassigns cond.lyr!

        if (env.type == "rnorm"){
            stop("This option has not yet been scripted")
            }           
            
        #This will rearrange the elements in the landscape, but only for cells where they can be appropriately assigned
          #And different code would be needed or random generation - there re-creating values will not work.
        if (c.type == "swap" & env.type == "cover"){
            #Just re-assign the layer - it will have the same values, because there was no random component.
            #This assumes the conditioning layer has not changed
            new.l.lyr = get.cond.env.lyr(env.info,p,cond.lyr,cond.vals)
            }
        
        #This will change each element according to a set of transition probabilities, but only for elements in a valid type of another layer
        if (c.type == "markov"){
            #NOTE: cond.lyr is a list to allow for the future possibility of conditioning on multiple layers            
            new.l.lyr = markov.change(l.lyr,my.env,ev,p,cond.lyr[[1]],cond.vals)
            
            }

        #Same as above, except the transition probabilities can change over time as well.
        if (c.type == "markov.changing.transitions"){
            #here c.mag needs to have 3 main elements
              #values potentially found in the landscape (need to be consecutive
              #transition probabilities for these values to all other values
              #how the transition probabilities change with time
                #e.g., 0:1;0.681:1:0.275:1;-0.05:0:-0.025:0
                #this would lead to a decrease of 0.05 in the transition probability of going from 0 to 0 (which will lead to a corresponding increase in going from 0 to 1, since there are only two
                #And there would be an decrease in going from 1 to 0, with a corresponding increase in going from 1 to 1.
                  #This would correspond to landuse intensification, with mowing becoming more frequent with time.
            #Change my.env to reflect new changed environmental transition rates
            my.env = change.transition.rates(my.env,ev)
            
            #implement markov chain process
            # NOTE: cond.lyr is a list to allow the future possibility of conditioning on more than one layer
            new.l.lyr = markov.change(l.lyr,my.env,ev,p,cond.lyr[[1]],cond.vals)
            }
            
        if (c.type == "from.file"){
            lbl = env.lbl[ev]
            c.count = change.count + 1 #change.count + 1 is to get the file from the current change, as change count has not yet been updated
            new.l.lyr = read.landscape.lyr(landscape.dir,lbl,lnd.lbl,c.count,p)
            }
        
        }
   
    return(list(new.l.lyr,my.env))
    }

#DD# add in ' again when reenabling the code
# # Write environmental layer to file (old version)
# #
# # Write an environmental layer to file (for visual debugging purposes)
# #
#write.env.lyr.old = function(in.lyr,extent,outfile){
#    in.lyr = matrix(in.lyr, nrow = extent, byrow = T)
#    write.table(in.lyr, file = outfile,sep = ",",row.names = F,col.names = F)
#    }

#' Write environmental layer to file
#'
#' Write an environmental layer to file (for visual debugging purposes)
#' Input is a landscape as a vector.  The landscape can be visualized by putting it into a matrix format (nrow = extent) and using the byrows = T option
#'
#' @param in.lyr The input layer values
#' @param change.count An indicator for the current environmental change step.
#' @param extent One sided length of the landscape
#' @param outfile File to be created
#' @param create.new (default to 0) An indicator for whether to write header info & start a new file
#'
write.env.lyr = function(in.lyr,change.count,extent,outfile, create.new = 0){
    if (length(in.lyr) != extent ^ 2){
        stop("Length of landscape layer is not equal to the extent squared.")
        }

    if (create.new == 1){
        #Add header to landscape file
        n.cells = seq(1,extent^2)
        cell.lbls = sprintf("Cell%s",n.cells)
        cell.lbls = listtotext(cell.lbls, ",")
        hdr = sprintf("ChangeStep,%s\n",cell.lbls)
        cat(hdr, file = outfile) #Creates a new file because append != T
        }

    in.lyr = listtotext(in.lyr,",")
    in.lyr = sprintf("%s,%s\n",change.count,in.lyr) #Add a step label and a hard return
    cat(in.lyr, file = outfile, append = T) #Write to file
    }

#' Write header for species data
#'
#' Like the title says.
#' @param outfile The file to be written to
#' @param p The number of cells in a square landscape
#'
write.sp.hdr.data = function(outfile, p){
    
    n.cells = seq(1,p)
    cell.lbls = sprintf("Cell%s",n.cells)
    cell.lbls = listtotext(cell.lbls, ",")
    hdr = sprintf("LifeStage,Species,TimeStep,%s\n",cell.lbls)
    cat(hdr, file = outfile) #Creates a new file because append != T
    }

#' Write species locations to file
#' 
#' Write the locations of a species to a file
#' Input is species abundances as a vector.  The species abundances on the landscape can be visualized by putting it into a matrix format (nrow = extent) and using the byrows = T option
#'
#' @param LifeStage The life stage of interest
#' @param sp the current species
#' @param RunTime The current timestep
#' @param in.sp.dat in.sp.dat
#' @param extent The one-sided length of the landscape
#' @param outfile the file to be written to
#'
write.sp.data = function(LifeStage,sp,RunTime,in.sp.dat,extent,outfile){

    if (length(in.sp.dat) != extent ^ 2){
        stop("Length of species vector is not equal to the extent squared.")
        }

    in.sp.dat = listtotext(in.sp.dat,",")
    in.sp.dat = sprintf("%s,%s,%s,%s\n",LifeStage,sp,RunTime,in.sp.dat) #Add a step label and a hard return
    cat(in.sp.dat, file = outfile, append = T) #Write to file
    }

#DD# add in ' again when reenabling the code
# # Write species locations to file (old)
# # 
# # Write the locations of a species to a file
# #
#write.sp.mat.data.old = function(in.matrix, in.path,in.lbl,sp,RunTime){
#    out.file = sprintf("%s%s_%s_%s.csv",in.path,in.lbl,sp,RunTime)
#    write.table(in.matrix, file = out.file, sep = ",", row.names = F, col.names = F)
#    }
#

#' Calculate log lambda and dispersion of log lambda
#'
#' Like the title says. %%DD%%
#'
#' @param in.file an input file
#' @param lbl a label
#'
get.log.lambda = function(in.file, lbl){
    
    #Calculate log lambda for overall
    my.dat = read.csv(in.file)
    
    #Sort data (in case someone sorted it into a different order -just to be safe
    my.dat = my.dat[order(my.dat$RunTime), ]

    sp.names = names(my.dat)[2:ncol(my.dat)]  #column 1 is RunTime, all others should correspond to a species

    #Create a header & column in my.dat for each species
    sp.hdrs = sprintf("log.lambda.%s",sp.names)
    for (hdr in sp.hdrs){
        my.dat[[hdr]] = NA
        }

    #Start with 2nd row (first row doesn't have anything to compare to)
    for (a.row in 2:nrow(my.dat)){
        p.row = a.row - 1
        this.time = as.num(my.dat[a.row, "RunTime"])
        last.time = as.num(my.dat[p.row, "RunTime"])
        
        #only calculate log-lambda if there is a 1 unit change between time steps (again, don't know how it wouldn't be, but want to be safe)
        if ((this.time - last.time) == 1){
        
            #loop through species and do calculations
            for (sp in 1:length(sp.names)){
                sp.lbl = sp.names[sp]
                sp.hdr = sp.hdrs[sp]
                
                this.val = as.num(my.dat[a.row, sp.lbl])
                last.val = as.num(my.dat[p.row, sp.lbl])
                
                lambda = this.val / last.val
                log.lambda = log(lambda) #This does log base e.  I'm assuming this is what Jakob used?  Does it matter?
                
                my.dat[[sp.hdr]][this.time] = log.lambda
                }
            }
        }

    #Update in.file to have log.lambdas
    new.file = substr(in.file,1,(nchar(in.file) - 4)) #Remove .csv
    new.file = sprintf("%s_v2.csv",new.file)          #add _v2.csv
    
    write.table(my.dat, file = new.file, sep = ",", row.names = F)
    unlink(in.file) #Delete the original file as it has been replaced (not just overwriting it, in case the code gets run more than once in a weird way)

    
    #Calculate geometric mean of log.lambda & dispersion
    mean.lambda.vec = c()
    sd.lambda.vec = c()
    for (sp.hdr in sp.hdrs){
        #overall.lambda = (prod(my.dat$log.lambda)) ^ (1/length(my.dat$log.lambda)) # NOTE: in log space, you want the arithmetic mean.  if you were in normal space, you would use a geometric mean
        overall.lambda = mean(my.dat[[sp.hdr]], na.rm = T)
        sd.lambda = sd(my.dat[[sp.hdr]], na.rm = T) #What formula did Jakob use?  Should it still be a geometric mean when measured on a log scale? (yes)    

        mean.lambda.vec = c(mean.lambda.vec, overall.lambda)
        sd.lambda.vec = c(sd.lambda.vec, sd.lambda)
        }
    
    return(list(sp.names,lbl,mean.lambda.vec,sd.lambda.vec))    
    }

#' Write log lambdas to file
#'
#' Take several calculations of log lambda, and place them into a single file
#' with species as rows, and values as columns.
#'
#' @param in.lists One or more lists to be written to file.  Each list should 
#' have four components: A vector with species headers, a label for the list, a vector with mean log
#' lambdas, and a vector with sd of log lambda.
#' @param out.file The file to be written
#'
#' @note Should change from vec1 and vec2 to take a list of lists as a more robust approach
compile.log.lambdas = function(in.lists,out.file){

    for (i in 1:length(in.lists)){
        
        my.lst = in.lists[[i]]
        
        #Create a data frame on the first time through the code
        if (i == 1){
            my.dat = data.frame(Species = my.lst[[1]])
            list.len = length(my.lst[[1]]) #get a length to use as a check against all other lists - they should be the same length (hmm, how will this work with the cell lists?)
            }
        
        if (length(my.lst[[1]]) != list.len){
            stop("The lists you are trying to compile are not of the same length.")
            }
        
        mean.lbl = sprintf("mean.%s",my.lst[[2]])
        sd.lbl = sprintf("sd.%s",my.lst[[2]])
        
        my.dat[[mean.lbl]] = my.lst[[3]]
        my.dat[[sd.lbl]] = my.lst[[4]]
        }
    write.table(my.dat, file = out.file, row.names = F, sep = ",")
    }

## Make final graphs
##
## THIS FUNCTION IS IN PROGRESS
##
## @param in.pdf The file to be generated
## @param ResultsFile The file that results will be written to.
## @param cover.levels The percent cover of each layer type
## @param s.lbl a label
##
#make.final.graphs = function(in.pdf,ResultsFile,cover.levels,s.lbl){
#    
#    stop("This function has not been  updated and should not be used")
#    
#    if (!require(Hmisc)){ stop("Please install Hmisc: install.packages('Hmisc')") }
#    
#    # For testing purposes
#    #setwd("C:/docs/beplants/Scripts/")
#    #ResultsFile = "outputs/Results_RTD_for_graph.csv"
#    #in.pdf = "outputs/Plots_RTD_testing1.pdf"
#    #cover.levels = c(100/p,50,100)
#    #s.lbl = c("TD","L1L2","L1L4","L2L2","L2L4","L4L4")
#    
#    trt = length(s.lbl) #number of different treatments/scenarios
#    
#    #Read in results written to file
#    all.data = read.csv(ResultsFile)
#
#    #These will not be aggregated properly and might as well be dropped:
#    all.data$Scenario = NULL
#    all.data$microsites = NULL #I don't care about this for now, and the NA's give me warnings
#
#    #Get limits for dependent variables for plotting purposes
#    mins = apply(all.data,2,min,na.rm = T)  # NOTE:  All fields need to be numeric, or R converts all of these to text, and botches the min/max calculations
#    maxes = apply(all.data,2,max, na.rm = T) #NOTE:  All fields need to be numeric, or R converts all of these to text, and botches the min/max calculations
#
#
#    #Subset all.data to get sample sizes by group of interest.  This really should be done by the aggregate function, but apparently it is defective!
#    n.mat = matrix(rep(NA,(trt * length(cover.levels))),nrow = trt) #hold results for sample sizes
#    for (r in 1:trt){
#        n.subset = all.data[all.data$Scenario.Number == r, ]
#        for (ec in 0:1){ #NOTE: Currently only set up for 2 levels of environmental change!
#            n.subset2 = n.subset[n.subset$Env.Change == ec, ]    
#            for (cc in 1:length(cover.levels)){
#                lvl = cover.levels[cc]
#                n.subset3 = n.subset2[n.subset2$Percent.Cover == lvl, ]
#                this.n = nrow(n.subset3)
#                n.mat[r,cc] = this.n
#                }
#            }
#        }
#            
#    # Note: all scenarios should be run for an equal number of times, so the n should be the same for all.
#    print("this matrix should have all the same number.  If not, the sample sizes are uneven, and the confidence intervals will be wrong!")
#    print(n.mat) #Visually check assumption that all n are equal
#    n.actual = n.mat[1,1] 
#           
#    #Aggregate data into data points by scenario and Percent Cover
#    scn.num = all.data$Scenario.Number
#    all.data$Scenario.Number = NULL #Remove from data frame after making a separate column for aggregation
#    frag = all.data$Percent.Cover
#    all.data$Percent.Cover = NULL #Remove data from data frame after making a separate column
#    enviro.change = all.data$Env.Change 
#    all.data$Env.Change = NULL
#        
#    #Actually aggregate the data
#    mean.data = aggregate(all.data,list(scn.num,frag,enviro.change),mean)
#    sd.data = aggregate(all.data,list(scn.num,frag,enviro.change),sd)
#    #ci.data = (sd.data * 1.96)/sqrt(n.actual) #95% CI as (sd * 1.96) / sqrt(n).  I thought there was a sqrt(n-1) for something, but it is not given in Zar (or Wikipedia)
#    
#    #Create a pdf file to hold the graphical outputs
#    pdf(file = in.pdf)
#    
#    #Make a separate plot for each of the dependent variables of interest
#    iv.vec = c("landscape.Sp.Rich.Final","landscape.Beta.Div.Final","landscape.Biomass.Final","landscape.FTD.UTC.Final","landscape.RTD.UTC.Final","landscape.RTD.UTC.Change") #Change plotted for RTD, because initial conditions are not equal
#    for (iv in iv.vec){
#        
#        #Loop through environmental change
#        for (ec in 0:1){
#            #Subset data to be homogeneous for environmental change type
#            ec.mean.data = mean.data[mean.data$Group.3 == ec, ]
#            ec.sd.data = sd.data[sd.data$Group.3 == ec, ]
#            
#            #Make separate plots for each variable
#            first = 1
#            
#            #Loop through scenarios, and put in the same plot with a legend
#            for (r in 1:trt){
#                
#                #Subset data to only relevant data
#                mean.subset = ec.mean.data[ec.mean.data$Group.1 == r, ]
#                sd.subset = ec.sd.data[ec.sd.data$Group.1 == r, ]
#                xx = mean.subset$Group.2
#                yy = mean.subset[[iv]]
#                ci = (sd.subset[[iv]] * 1.96) /sqrt(n.actual)
#                
#                #Add in a slight off-set so points are not right on top of one another
#                xx = xx + (r - 3.5) #r - 3.5 is so that the points are centered around the actual value
#                
#                ymin = mins[[iv]]
#                ymax = maxes[[iv]]
#                
#                #Special legend plotting
#                legend.loc = "bottomright" #set this as default, will work for most plots
#                if (iv == "landscape.Beta.Div.Final"){
#                    legend.loc = "topright"
#                    }
#                
#                if (first == 1){
#                    my.xlab = sprintf("Habitat Cover (%s), Environmental Change = %s","%",ec) #NOTE: The % alone crashes the function, because it thinks you forgot a letter - hence why it is put in via substitution.
#                    plot(yy ~ xx,xlim = c(0,105),xlab = my.xlab,ylim = c(ymin,ymax),ylab = sprintf("%s",iv), col = r) #Figure out how to fill backgrounds, think about drawing lines
#                    legend(x = legend.loc,legend = s.lbl, col = seq(1,trt,1),pch = 16)
#                } else {
#                    points(yy ~ xx, col = r)
#                    }
#                Hmisc::errbar(xx,yy, yy + ci, yy - ci, add = T, pch = 16, cap = .01,col = r, errbar.col = r) #errbar is from Hmisc
#                    
#                first = 0 #Control what happens on the first run through the loop - here it gets turned off until the next iv variable
#                }
#            }
#        }
#    #Stop writing to the pdf.
#    dev.off()
#    }

#' Prep diagnostic output
#'
#' Reformats the matrix into a vector, and adds desired prefix information
#'
#' @param in.spar.mat in.spar.mat
#' @param change.count An indicator for the current environmental change step.
#' @param sp The species of interest (numeric)
#' @param cell A cell label
prep.output = function(in.spar.mat,change.count,sp,cell = ""){
    #Convert from sparsematrix, so that I can write it to file
    out.dat = as.matrix(in.spar.mat) # Note: after doing this step, the image command will no longer work using nicer method applied to sparsematrices
    
    #**# THIS IS LIKELY A SLOW STEP & SHOULD PROBABLY BE FIXED.
    out.dat = matrix.as.vector(out.dat)


    #Include cell, if cell is included as an input, otherwise exclude it.
    cell.out = cell
    if (cell != ""){
        out.dat = c(change.count,sp,cell.out,out.dat)
    }else{
        out.dat = c(change.count,sp,out.dat)
        }

    out.dat = matrix(out.dat, nrow = 1) #Matrix format is required to get write.table to write it as a row!

    #    cell.out = sprintf("%s,", cell) #Add a comma to cell, if it is not blank
    #    }
    #    
    #out.dat = listtotext(out.dat,",")
    #out.dat = sprintf("%s,%s,%s%s\n",change.count,sp,cell.out,out.dat)
    return(out.dat)
    }

#' matrix.as.vector
#'
#' Converts a matrix to a vector, but does so by rows (I don't know of a pre-existing option for this)
#'
#' @param in.mat A matrix
#'
matrix.as.vector = function(in.mat){

    #Transpose matrix, then convert to one column.
    mat.out = Matrix::t(in.mat)
    mat.out = matrix(mat.out, nrow = 1)
    
    return(mat.out)
    }

#DD# This would be a good place for more documentation as well
#' Compute Diagnostics
#'
#' Function to compute basic matrix diagnostics
#'
#' @param spe The number of species in the species pool
#' @param B1.lst A list of the first demography matrices for each species
#' @param B2.lst A list of the second demography matrices for each species
#' @param M.lst A list of the dispersal matrices for each species
#' @param P The vec-permutation matrix
#' @param p The number of cells in a square landscape
#' @param S The number of stages in the matrix model
#' @param outpath.base The base path for outputs
#' @param vdb.data An indicator for whether or not to create visual debugger data
#' @param change.count An indicator for the current environmental change step.
#' @param run.times A list of intermediate times during the model run
#' @param run.lbl A list of labels for each of the intermediate times
#' @param is.first An indicator for whether this is the first time the code is run (for timing purposes)
#' @param Gave.Warn An indicator for whether a warning message has already been given. If so, a duplicate warning will NOT be reissued.
#' @note The diagnostics DO NOT include effects of the carrying capacity or
#' microsite competition!
#' WARNING This function requires exactly 4 stages, with the first stage as
#' the mobile dispersal stage. is stage specific, and requires 4 stages with the first stage as mobile seeds!
#'
compute.diagnostics = function(spe,B1.lst,B2.lst,M.lst,P,S,p,outpath.base,vdb.data,change.count,run.times,run.lbl, is.first = 0, Gave.Warn = 0){

    #setup path for diagnostics files
    diagnostics.path = sprintf("%sDiagnostics/",outpath.base)
    dir.create(diagnostics.path, showWarnings = F)

    #Set up output files    
    Matrix.Diagnostics = sprintf("%sMatrixDiagnostics.csv", diagnostics.path)
    
    if (vdb.data == 1){
        A.Matrices = sprintf("%sAMatrices.csv", diagnostics.path)
        A.Matrices.Summary = sprintf("%sAMatricesSummaries.csv", diagnostics.path)        
        Transition.Matrices = sprintf("%sTransitionMatrices.csv", diagnostics.path)
    
        if (is.first == 1){

            #Set up header for overall transition matrices
            #Uninformative labels, because I don't know how to properly label each transition rate, and that is not my highest priority.
            n.entries = (S - 1)^2 * p^2
            entries = seq(1,n.entries)
            e.lbls = sprintf("Entry_%s",entries)

            A.mat.hdr = matrix(c("ChangeStep","Species",e.lbls),nrow = 1)
            write.table(A.mat.hdr, file = A.Matrices, sep = ",", append = F, col.names = F, row.names = F)
            
            #Set up header for overall transition matrices summaries
            A.sum.hdr = matrix(c("ChangeStep","Species","Cell","Dominant.EigenValue","Per.Capita.Seeds.Exported","Per.Capita.Seeds.Imported","Prop.seeds.exported"),nrow = 1)
            write.table(A.sum.hdr, file = A.Matrices.Summary, sep = ",", row.names = F, col.names = F, append = F)

            #Set up header for local transition matrices
            n.entries = (S - 1)                      #Get number of entries, excluding mobile seed stages
            r.lbl = rep(seq(1,n.entries),n.entries)  #create an index for each row
            c.lbl = sort(r.lbl)                      #create an index for each column
            e.lbls = sprintf("p%s%s",r.lbl,c.lbl)    #put them together using the notation I've been using elsewhere
            
            trans.hdr = matrix(c("ChangeStep","Species","Cell", e.lbls), nrow = 1)
            write.table(trans.hdr, file = Transition.Matrices, append = F, row.names = F, col.names = F, sep = ",")
            }
        }

    #Initialize dataframe to contain results
    i = rep(NA, spe)  #i for initial, not i as an index.
    diag.results.df = data.frame(ChangeStep = rep(change.count,spe),Species = seq(1,spe),A.Eigen = i, Is.Irreducible = i, Is.Ergodic = i,
                                 Max.Local.Eigen = i, Median.Local.Eigen = i, Min.Local.Eigen = i,
                                 Max.Seed.Export = i,Median.Seed.Export = i, Min.Seed.Export = i,
                                 Max.Seed.Import = i, Median.Seed.Import = i, Min.Seed.Import = i,
                                 Prop.Dispersing = i)

    if (is.first == 1){
        run.times = c(run.times,gettime()); run.lbl = c(run.lbl,"Compute Diagnostics Initialization complete")
        }

    #Loop through each species
    for (sp in 1:spe){

        #Extract matrices for this species
        B1 = B1.lst[[sp]]
        B2 = B2.lst[[sp]]
        M = M.lst[[sp]]
        
        #Calculate overall lambda for each species
        A <- B2 %*% Matrix::t(P) %*% M %*% P %*% B1

        if (is.first == 1 & sp == 1){
            run.times = c(run.times,gettime()); run.lbl = c(run.lbl,"A matrix calculated for sp 1")
            }

        #Drop mobile seeds (issue is that they are effectively dropped by the matrix algebra, but cause problems for the matrix diagnostics)        
        #Mobile seeds correspond to the first row & column in the matrix, and +4 after that        
        mobile.seed.index = seq(1,nrow(A),4) #Increment by 4's, row & column numbers are the same, so just using row here.
        A2 = A[-mobile.seed.index,-mobile.seed.index] #The minus sign says to drop those [rows,columns]

        if (is.first == 1 & sp == 1){
            run.times = c(run.times,gettime()); run.lbl = c(run.lbl,"A2 matrix calculated for sp 1")
            }

        #Two step example with dropping rows & columns manually for a 2 x 2 landscape
        #A2 = A[ ,c(-1,-5,-9,-13)]
        #A2 = A2[c(-1,-5,-9,-13), ]

        #Set up paths for outputting diagnostic data for the visual debugger
        if (vdb.data == 1){
            A2.out = prep.output(A2,change.count,sp)      #Prepare A2 for writing to file
            write.table(A2.out, file = A.Matrices, append = T, row.names = F, col.names = F, sep = ",")
            }

        #Overall eigen vector   #NOTE: at present, imaginary eigenvectors are ignored
        A2.eigen<-eigen(A2) #Calculate eigenvalues for A2 (without mobile seeds). 
        dom.pos<-which.max(Re(A2.eigen$values)) #Get the position of the dominant eigenvalue. Re gets just the real component
        A.dom.eigenv<-Re(A2.eigen$values[dom.pos]) #the dominant eigenvalue. Re gets just the real component
        diag.results.df$A.Eigen[sp] = A.dom.eigenv

        if (is.first == 1 & sp == 1){
            run.times = c(run.times,gettime()); run.lbl = c(run.lbl,"Eigen values for A2 matrix calculated for sp 1")
            }
    
        #If sourcing stottmatrix version:
        if (exists("is.irreducible")){
    
            #Check reducibility
            mat.red = is.irreducible(A2)
            
            #Convert to TRUE/FALSE instead of string
            if (substr(mat.red,1,4) == "TRUE"){
                mat.red = TRUE
            }else{
                if (substr(mat.red,1,5) == "FALSE"){
                    mat.red = FALSE
                }else{
                    stop("Problem in compute.diagnostics function - unexpected output for mat.red")
                    }                    
                }

            if (is.first == 1 & sp == 1){
                run.times = c(run.times,gettime()); run.lbl = c(run.lbl,"Irreducibility checked for sp 1")
                }
                
            #Check ergodicity # WARNING: this function may fail under some conditions
              #May need to use is.primitive and blockmatrix functions for further diagnostics
            mat.erg = is.ergodic(A2)
            
            mat.erg.p1 = mat.erg$ergodic
            mat.erg.p2 = mat.erg$lefteigvec
            if (mat.erg.p1 == 1){
                mat.erg = TRUE
            }else{
                if (mat.erg.p1 == 0){
                    mat.erg = F
                }else{
                    stop("problem in compute diagnostics function for ergodicity - unexpected output for mat.erg")
                    }
                }
            
            if (is.first == 1 & sp == 1){
                run.times = c(run.times,gettime()); run.lbl = c(run.lbl,"Ergodicity checked for sp 1")
                }
            } # NOTE: REMOVE THIS '}' WHEN RE-ENABLING THE ELSE STATEMENT BELOW
                        
        #popdemo version
        #  #**# Needs checking when popdemo is patched.
        #}else{
        #    if (!require(popdemo)){ stop("Please install popdemo: install.packages('popdemo')") }
        #    
        #    #Check reducibility
        #    mat.red = popdemo::is.matrix_irreducible(A2)
        #    
        #    #Check ergodicity #**# Warning, may need to specify a digits argument to round, to avoid small, approximately 0 answers
        #    mat.erg = popdemo::is.matrix_ergodic(A2)
        #    
        #    }

        #Update results for this species            
        diag.results.df$Is.Irreducible[sp] = mat.red
        diag.results.df$Is.Ergodic[sp] = mat.erg

        #if reducible & non-ergodic, check which initial conditions matter
        if (mat.red == F & mat.erg == F){

            # According to Caswell 2001, a useful diagnostic is to
            #block-permute the matrix, then compare the connections between blocks
            #and look at the block eigenvalues. That should tell you the overall, long-term results.
            #And the block structure will be of interest as well.

            # NOTE: NOT YET SCRIPTED            
            }

            
        #Local transition matrices
          #This diagnostic shows which cells/cover types are performing best/worst for each species
          #For this, need to have the eigenvalue, but also the environmental conditions leading to that eigenvalue          
          #It excludes external dispersal
          #Consequently a population may be viable (due to high dispersal rates) but show up as declining based on this diagnostic.

          loc.mat.eigen = c()
          exported.seeds.vec = c()
          imported.seeds.vec = c()
          prop.disp.vec = c()

          #Patch to deal with situation where there is 0 reproduction
          was.warn = 0

          #Loop through cells
          for (cell in 1:p){

              #Extract the demographic matrices pertaining to this cell
              #For 4 stages, 1 is dropped, so cells should be 3 x 3 along the diagonal
              #1:3,1:3, 4:6,4:6, 7:9,7:9, 10:12,10:12
              S2 = S - 1 # to account for the dropped stage
              #Row and column indices are the same
              start.index = (cell - 1) * S2 + 1 #cell - 1 makes it so that the first cell starts at 0, S2 increments by the number of stages, and + 1 ensures it starts at 1 
              end.index = (cell - 1) * S2 + S2 #same logic as above, except the + S2 gives it the correct end point
             
              Local.Matrix = A2[start.index:end.index, start.index:end.index]

              if (vdb.data == 1){
                  #Write local transition matrices
                  loc.mat.out = prep.output(Local.Matrix,change.count,sp,cell)                  
                  write.table(loc.mat.out, file = Transition.Matrices, append = T, sep = ",",row.names = F, col.names = F)
                  }

              #Calculate dominant eigenvalue for the local matrix (does not include imported seeds or seeds lost to dispersal)
              Local.Matrix.eigen<-eigen(Local.Matrix) #Calculate eigenvalues for A2 (without mobile seeds)
              dom.pos<-which.max(Re(Local.Matrix.eigen$values)) #Get the position of the dominant eigenvalue. Re just gets the real component
              Local.Matrix.dom.eigenv<-Re(Local.Matrix.eigen$values[dom.pos]) #the dominant eigenvalue.  Re gets just the real component

              loc.mat.eigen = c(loc.mat.eigen,Local.Matrix.dom.eigenv)

              #Get number of seeds exported to landscape
              export.col.index = cell * S2
              seed.row.indices = seq(1,((p * S2) - (S2 - 1)),S2)
              export.row.indices = seed.row.indices[-cell]
              exported.seed.values = A2[export.row.indices, export.col.index]
              exported.seeds = sum(exported.seed.values)
          
              exported.seeds.vec = c(exported.seeds.vec,exported.seeds)          
              
              #Get proportion of seeds dispersing (seeds dispersing / total seeds produced by cell)
              all.seed.values = sum(A2[seed.row.indices, export.col.index]) #As above, except with all seed cells, and combined into one step
              prop.seeds.dispersing = exported.seeds / all.seed.values
              
              #Check if all.seed.values is 0. If it is, there is no reproduction
              if (all.seed.values == 0){
                  was.warn = 1
                  if (Gave.Warn == 0){
                      warning("At least one species in one cell has 0 reproduction. Diagnostics on proportion dispersing will include NaN as this calculation would require dividing by 0.")
                      Gave.Warn = 1
                      }
                  }
              
              prop.disp.vec = c(prop.disp.vec,prop.seeds.dispersing)
              
              #Get number of seeds received from landscape
              import.row.index = (cell - 1) * S2 + 1 #First row for first cell, 4th row for 2nd cell (for S2 = 3), etc.
              seed.col.indices = seq(S2,ncol(A2),S2) #Start in S2 column, go through all columns in matrix, and do it by increments of S2                           
              import.col.indices = seed.col.indices[-cell]  #seed.col.index, but without the seeds that remain within the cell.  Cell indexes the element that corresponds to the cell of interest.
              
              imported.seed.values = A2[import.row.index,import.col.indices]
              imported.seeds = sum(imported.seed.values)
              
              imported.seeds.vec = c(imported.seeds.vec,imported.seeds)
              
              #Write dominant eigenvalues for local transition matrices, seeds exported, and seeds imported for each cell
              if (vdb.data == 1){           
                sum.info = matrix(c(change.count, sp, cell, Local.Matrix.dom.eigenv,exported.seeds,imported.seeds,prop.seeds.dispersing),nrow = 1)
                write.table(sum.info, file = A.Matrices.Summary, append = T, sep = ",", row.names = F, col.names = F)
                }
            }
        
        if (is.first == 1 & sp == 1){
            run.times = c(run.times,gettime()); run.lbl = c(run.lbl,"Local transition matrices checked for sp 1")
            }
                                    
        #Summarize the results from ALL cells # NOTE: These are potentials - they will/may not be realized if cells are unoccupied
        #Max, Median, and Min eigen value
        diag.results.df$Max.Local.Eigen[sp] = max(loc.mat.eigen)
        diag.results.df$Median.Local.Eigen[sp] = median(loc.mat.eigen)
        diag.results.df$Min.Local.Eigen[sp] = min(loc.mat.eigen)
        
        #Max, Median, and Min exported seeds
        diag.results.df$Max.Seed.Export[sp] = max(exported.seeds.vec)
        diag.results.df$Median.Seed.Export[sp] = median(exported.seeds.vec)
        diag.results.df$Min.Seed.Export[sp] = min(exported.seeds.vec) 
        
        #Max, Median, and Min imported seeds
        diag.results.df$Max.Seed.Import[sp] = max(imported.seeds.vec)
        diag.results.df$Median.Seed.Import[sp] = median(imported.seeds.vec)
        diag.results.df$Min.Seed.Import[sp] = min(imported.seeds.vec)
        
        # NOTE: This check was removed because the proportion dispersing was changing with an ABSORBING edge type (because some dispersers were not being counted because they fell off the edge of the world)
            #Max, min & median for proportion dispersing
            # NOTE: WEIRD BUG! as.character here patches very odd behavior where I had two numbers 0.086 and 0.086, and R was saying they were different, even though they looked exactly alike!
            #check that proportion dispersing is constant (it should be - because it is per capita) #Oct 13 edit - removed NA values, because they legitimately can occur in the case where no seeds are produced.
            #was.warn is a patch to avoid a situation that can legitimately cause this test to fail - when there is no reproduction.
            #if (length(unique(as.character(na.omit(prop.disp.vec)))) != 1 & was.warn == 0){
            #    stop("Proportion dispersing differed among cells.  Unless dispersal varies within species (currently not supported), this should not occur.")
            #    }
        diag.results.df$Prop.Dispersing[sp] = median(prop.disp.vec)
              
        #Perturbation analysis
          # NOTE: NOT YET SCRIPTED
        
        #Damping ratio?
          # NOTE: NOT YET SCRIPTED

        # Proportion of individuals in each cell based on the stable stage distribution 
          # because there are life-stage stages and spatial "stages"?
          # NOTE: NOT YET SCRIPTED
        
        #Other diagnostics?
          # NOTE: NOT YET SCRIPTED
              
        if (is.first == 1 & sp == 1){
            run.times = c(run.times,gettime()); run.lbl = c(run.lbl,"End of diagnostics loop for sp 1")
            }
        
        }

    if (is.first == 1){
        run.times = c(run.times,gettime()); run.lbl = c(run.lbl,"End of diagnostics loop for all species")
        }

    inc.col.names = F
    do.append = T
    if (is.first == 1){
        inc.col.names = T
        do.append = F
        }

    #Write results from matrix diagnostics # WARNING: Watch that header is in the same order as the dataframe - this could cause problems later if changes are made.
    diag.results.df = as.matrix(diag.results.df) #Matrices are faster to write to file
    write.table(file = Matrix.Diagnostics, diag.results.df, col.names = inc.col.names, row.names = F, sep = ",", append = do.append)

    if (is.first == 1){
        run.times = c(run.times,gettime()); run.lbl = c(run.lbl,"Diagnostics written to file")
        }
    return(list(run.times,run.lbl,Gave.Warn))
    }


#' Apply thinning law
#'
#' Calculate a component of transition rates based on a thinning law approach.
#' Inputs may be vectors of values
#'
#' @details The thinning law here affects transition rates
#' (biomass(t + 1) / biomass (t))** m, where m = -4/3 (reciprocal of the logarithmic self-thinning slope.
#' The idea is that there are more individuals than can be supported by the environment
#' and as individuals grow, they interfere with each other's ability to obtain resources.
#' Some individuals die as a consequence of this competition (typically the smaller
#' individuals, although size and competition is not directly modeled (hence
#' assumption #3). \cr
#' Assumptions: \tabular{ll}{
#' (1) \tab There are more individuals at time t than can reach time t + 1 \cr
#' (2) \tab Even aged stand \cr
#' (3) \tab Survival is probabilistic, and can be applied without knowledge of
#' individual characteristics and/or identities. \cr
#' }
#'
#' @param initial.biomass Initial biomass
#' @param final.biomass Final biomass
#' @param m The thinning law, defaults to -4/3
#'
#' @note Thinning laws were developed in a single-species, even-aged stand context.
#' Consequently, their application in a multi-species, uneven-age community context
#' may violate critical assumptions.  See Gerstenlauer et al. for a more
#' thorough discussion.
#' @references Wiegand et al. unpubl. #DD# check if this is published (I think I have it)
#' @template stochasticplants
do.thinning = function(initial.biomass,final.biomass,m = -4/3){

    out.vr = (final.biomass / initial.biomass) ^ m
    
    return(out.vr)    
    }
    
#' Calculate Beta Values
#'
#' Obtain values from a Beta Distribution based on an input mean and variance. 
#' Note that the mean and variance are NOT the same as the Alpha and Beta inputs.
#' I added a check - if Var is 0, then just the mean is returned (and no Beta
#' distribution is applied).  Note that the variance is not like a typical variance
#' in that at low variance values close to the mean are much more likely, and at
#' high values, values far from the mean are much more likely (and the mean itself
#' will have a relatively low probability of being drawn.
#'
#' @details Derivation for constraint on variance based on the mean: \cr
#' Mean must be > 0, Var must be > 0 (if Var == 0, then just the mean will be returned).
#' Addionally, Var must be <= mean * (1 - mean) in order to ensure that alpha is positive
#' alpha and beta, the parameter inputs to the beta distribution are constrained to be
#' >= 0.  Consequently, below, we solve for the equation that determines allowed
#' values of Var depending on the value of the mean. \cr
#' mean * ( (1 - mean) * mean * (1 / Var) - 1) = alpha  \cr
#' mean * ( (1 - mean) * mean * (1 / Var) - 1) >= 0 #alpha must be >= 0 \cr
#' mean * ( mean * (1/Var) - mean^2 * (1 / Var) - 1) >= 0 #expand \cr
#' mean^2 / Var - mean^3 / Var - mean >= 0 #expand further \cr
#' mean^2 - mean^3 - mean * Var >= 0 #multiply by Var.  No change in direction of >= sign, because Var must be > 0 \cr
#' mean^2 - mean^3 >= mean * Var #Add mean * Var to both sides of equation \cr
#' mean - mean^2 >= Var #divide by mean.  No change in direction of >= sign because mean > 0 \cr
#' mean(1 - mean) >= Var #factor \cr
#' Var <= mean(1 - mean) #Re-arrange to be more intuitive \cr
#' Beta imposes the exact same constraint as alpha (can substitute alpha into the equation for beta
#' then use algebra). Beta = alpha/mean - alpha  Not shown here.
#' @param num.out The number of values to generate based on the beta distribution
#' @param mu The desired mean for the distribution.  Must be between 0 and 1.
#' @param Var The desired variance for the distribution
#' @return Returns a vector of length num.out with values from the Beta distribution.
#' @note From Jakob's ScriptA.2_BetaDistribution_2011_01_18.r
#' @author Jakob L. K. Gerstenlauer (main function) and Sasha Keyel (error messages)
#' @export calc.beta
calc.beta = function(num.out, mu, Var){

    if (mu <= 0 | mu >= 1){
        stop("Mean for beta distribution must be between 0 and 1")
        }
    
    if (Var < 0){
        stop("Variance cannot be less than one")
        }
      
    if (Var >= (mu * (1 - mu))){
        stop("Variance cannot equal or exceed: mean * (1 - mean)")
        }
    
    if (Var == 0){
        out.vals = rep(mu,num.out)
    }else{    
        # I do not know the origin of these equations, other than from
        # Jakob's script.  I assume they are correct.
        Alpha<- mu * ( (1-mu)* mu * (1/Var) -1)
        Beta<- (Alpha / mu) - Alpha
        }

    out.vals = rbeta(num.out,Alpha,Beta)
    return(out.vals)
    }

#' Stochastic Plants Setup
#'
#' Set up latin hypercube sampling for plant traits used in stochastic plants simulation 
#' %% #DD# add documentation for env.param.file and sp.param.file
#'
#' @param SampleSize Number of species to generate
#' @param ModelRuns  Number of scenarios to generate
#' @param sp.param.file A file containing species parameters.  See \link[spatialdemography]{sp.param.file} for details.
#' @param env.param.file A file containing environmental parameters.  See \link[spatialdemography]{env.param.file} for details.
#' @param do.seed.clone.override 0 indicates that seed and clone number should be treated as dependent traits
#' 1 allows these traits to be specified as independent traits.
#' @param do.thinning.override 0 indicates that some demographi transitions should be dependent traits
#' based on the thinning law.  1 indicates that these parameters may be independently speciefied.
#' @param NumSVars Number of species variables in sp.param.file input.  Default is 7.
#' @param NumEVars Number of environmental variables in env.param.file.  Default is 7.
#' 
#' @export stoch.plant.setup
stoch.plant.setup = function(SampleSize,ModelRuns,sp.param.file,env.param.file,do.seed.clone.override = 0,do.thinning.override = 0,NumSVars = 7, NumEVars = 7){

    if(!require(lhs)){ stop("Please install lhs: install.packages('lhs')") }

    #NumSVars = 7: seed.disp.ratio, adult.longevity, proportion.clones, seed emergence, seed biomass, adult biomass, clone biomass    
    if (do.seed.clone.override == 1){
        NumSVars = NumSVars + 2 # two optional override values: seed.num and clone.num
        }
    
    if (do.thinning.override == 1){
        NumSVars = NumSVars + 3 # three optional thinning override values: d12, d23, dc1 
        }
    
    #Sample species using latin hypercube design    
    species.lhs = lhs::improvedLHS(n = SampleSize, k = NumSVars, dup = 1)
    
    #Read in species trait ranges from file    #**# Need documentation for this somewhere - probably near the setup.inputs function
    sp.info = read.csv(sp.param.file, row.names = 1)
    
    #Setup species values based on the hypercube sample
    sp.vals = setup.inputs(sp.info, species.lhs)
   
    #NumEVars = 7: cost of reproduction, seed mortality, seedling survival, variance of seedling survival, mean fertility, variance of fertility, correlation between seedling survival and fertility    
    #Sample environmental conditions using a latin hypercube design    
    env.lhs = lhs::improvedLHS(n = ModelRuns, k = NumEVars, dup = 1)
    
    #Read in environmental conditions ranges from file #**# Need documentation for this too.
    env.cond.info = read.csv(env.param.file, row.names = 1)
    
    #Setup environmental conditions based on the hypercube sample
    env.cond = setup.inputs(env.cond.info,env.lhs,in.lbl = "Model.Run")

    return(list(sp.vals,env.cond))
    }

#' Set up species and environmental inputs for stochastic plants simulation
#'
#' For stochastic plants simulation.  Creates according to Gerstenlauer et al. (in prep).
#'
#' @param in.info A file containing data to be sampled according to the hypercube design
#' @param in.lhs A latin hypercube sampling matrix
#' @param in.lbl A label for the dataframe. Can be "Species" or "Model.Run"
#' @return returns the created species file read into R.
#' @export setup.inputs
setup.inputs = function(in.info,in.lhs,in.lbl = "Species"){

    #Get names from input parameters file
    trait.names = row.names(in.info)
    
    #Check that number of trait names match number of traits
    if (length(trait.names) != ncol(in.lhs)){
        stop("Trait names must match number of traits in Latin Hypercube")
        }
    
    in.info$Range = in.info$Max - in.info$Min

    if (in.lbl == "Species"){
        #Create a data frame with a row for each species to be created    
        out.vals = data.frame(Species = seq(1,nrow(in.lhs)))
        }

    if (in.lbl == "Model.Run"){
        #Create a data frame with a row for each model run to be created    
        out.vals = data.frame(Model.Run = seq(1,nrow(in.lhs)))
        }        
    
    #Add the names of the traits to the data frame
    for (nam in trait.names){
        out.vals[[nam]] = rep(0,nrow(in.lhs))
        }

    #**# This may need optimization!
    #loop through Latin hypercube samples
    for (i in 1:nrow(in.lhs)){

        #Restrict to a particular sample from the hypercube
        this.sample = in.lhs[i , ]

         #Loop through each trait, and get it's specific value
          #Max - Min = Range - this is sampled by multiplying by the latin hypercube value for the trait, and then the sampled range is added to the minimum
        for (tr in 1:ncol(in.lhs)){  #Either ncol(in.lhs) or length(trait.names) would work here.
            this.trait = trait.names[tr]
            out.vals[i , this.trait] = in.info[this.trait,"Range"] * this.sample[tr] + in.info[this.trait,"Min"]
            }
        }    
    
    return(out.vals)
    }

## CALCULATE TRAITS
#' Calculate traits (stochastic plants)
#'
#' Calculate traits based on logic in stochastic plants MS
#'
#' @param sp.vals Input species parameters. See details at \link[spatialdemography]{sp.vals}
#' @param these.env.cond Input environmental settings. See details at \link[spatialdemography]{env.param.file}
#' @param sp.out.file Location for the output species file
#' @param env.file Location for the output Environmental Layers file
#' @param do.seed.clone.override An indicator variable to determine whether to override longevity/seed/clone trade-offs
#' @param do.thinning.override An inidicator variable to determine whether to override default thinning behavior.
#' @return No value is returned from the function, but a new species file is written and a new environmental layers file is written.
#' @note Stochastic seed survival probably needs to be stochastic and change at each time step as well.
#' @template stochasticplants
#' @export calculate.traits
calculate.traits = function(sp.vals, these.env.cond, sp.out.file, env.file, do.seed.clone.override, do.thinning.override){ 

    warning("The calculate.traits function is in development and requires further testing")

    # Calculate juvenile biomass
    sp.vals$biomass.juv = mean(c(sp.vals$biomass.seed,sp.vals$biomass.adult)) #Arithmetic mean.

    #Make sure juvenile biomass is greater than or equal to clone biomass, otherwise the thinning law will behave oddly
    if (sp.vals$biomass.juv < sp.vals$biomass.clone){
        sp.vals$biomass.juv = sp.vals$biomass.clone #Set to clone biomass if juv.biomass is is less than clone biomass.
        }

    #Option to override the longevity/reproduction/seeds/clones trade-off
    #If not doing the override (if using the override, the required fields will already be in sp.vals)
    if (do.seed.clone.override == 0){    
        ## Calculate reproductive allocation based on adult longevity
        # reproductive allocation depended on adult longevity based on a Weibull function
          #**# Why the Weibull?
        k1 = 1   #**# Rationale?
        k3 = 0.5 #**# Rationale?
        
        #**# This is giving me a square root of a negative number.  Something is wrong in the equation I have.
        #Perhaps the raising to a power happens after the exp?  that would make sense with how it was written.
        #relative allocation to reproduction (a)
        sp.vals$a = k1 * (exp(-sp.vals$adult.longevity / these.env.cond$k2))^k3
        
        ## Sexual vs. asexual reproduction & seed size vs. seed number
        #total allocation to reproduction (q)
        sp.vals$q = sp.vals$biomass.adult * sp.vals$a * these.env.cond$fertility.mean   #in grams
        #q = num.seeds * biomass.seed + num.clones * biomass.clone
        #g = (num.clones * biomass.clone) / q
        #The below follow algebraically from the above
        sp.vals$num.clones = sp.vals$q * sp.vals$g / sp.vals$biomass.clone
        sp.vals$num.seeds = sp.vals$q * (1 - sp.vals$g) / sp.vals$biomass.seed
        }
    
    ## Thinning law
    
    #If not overriding thinning, get parameters (otherwise, parameters will already be in file)
    if (do.thinning.override == 0){
    
        #Calculate thinning rates
        sp.vals$d12 = do.thinning(sp.vals$biomass.seed,sp.vals$biomass.juv)
        sp.vals$dc2 = do.thinning(sp.vals$biomass.clone,sp.vals$biomass.juv)
        sp.vals$d23 = do.thinning(sp.vals$biomass.juv,sp.vals$biomass.adult)
        }
    
    ## Putting together the vital rates
    #Create a data frame to store the output
    sp.data = data.frame(Species = sp.vals$Species)
    
    #Set up entries in dataframe
    entries =  c("p01","p02","p11","p12","p22","p23","p30","p32","p33","biomass.adult","biomass.juv","biomass.seed","dispersalfunction","disppar1","disppar2","F.p30","F.p32","S.p02","S.p12")
    for (entry in entries){
        sp.data[[entry]] = rep(NA,length(sp.vals$Species))
        }

    #calculate values to go in data frame
    Ss = 1 # Ss is left here to remind me that it matters.  But it will actually be incorporated through a stochastic environmental layer
    sp.data$biomass.adult = sp.vals$biomass.adult
    sp.data$biomass.juv = sp.vals$biomass.juv
    sp.data$biomass.seed = sp.vals$biomass.seed
    sp.data$p01 = (1 - sp.vals$w) * (1 - these.env.cond$seed.mortality) #Number not germinating  * number surviving
    sp.data$p02 = sp.vals$w * sp.vals$d12 * Ss #= p12 #number germinating * thinning * stochastic survival
    sp.data$p11 = (1 - these.env.cond$seed.mortality) * (1 - sp.vals$w) # =p01 number not germinating * number surviving
    sp.data$p12 = sp.vals$w * sp.vals$d12 * Ss #number germinating * thinning * stochastic survival
    sp.data$p22 = 0 #By definition
    sp.data$p23 = sp.vals$d23 #**# Is this defined somewhere?  I'm just assuming this is the thinning component
    sp.data$p33 = sp.vals$adult.longevity #By definition
    sp.data$p30 = sp.vals$num.seeds * (1 - sp.vals$seed.disp.ratio) #Number of seeds, except those that disperse
    sp.data$p32 = sp.vals$num.clones * sp.vals$dc2 #Direct Clonal reproduction.  There is also an adult contribution through mobile seeds that immediately germinate - p02

    #**# Add rounding - but try that after you get match with Jakob's results

    sp.vals$ending = sprintf("%s:%s:%s;%s:%s",sp.vals$biomass.adult,sp.vals$biomass.seed,sp.vals$biomass.clone,sp.vals$a,sp.vals$g)
    sp.data$F.p30 = sprintf("%s;104;%s",sp.data$p30,sp.vals$ending)
    sp.data$F.p32 = sprintf("%s;105;%s",sp.data$p32,sp.vals$ending)
    
    sp.data$S.p02 = sprintf("1;103;%s;%s",these.env.cond$Ss.mean,these.env.cond$Ss.var)
    sp.data$S.p12 = sprintf("1;103;%s;%s",these.env.cond$Ss.mean,these.env.cond$Ss.var)
    
    # Version from MS
    #keeping terminology from SpatialDemography, not from MS
    #no mobile seeds - not spatially explicit
    #p11 = (1 - seed.mortality) * (1 - w)
    #p12 = w * d12 * Ss
    #p22 = 0
    #p23 = d23 #**# Is this defined somewhere?  I'm just assuming this
    #p33 = adult.longevity
    #p31 = num.seeds * (1 - seed.disp.ratio) * (1 - seed.mortality) * (1 - w)
    #p32 = num.clones * dc2 + (1 - seed.disp.ratio) * num.seeds * p12 #Clonal reproduction + those seeds that immediately germinate & turn into juveniles
        
    #Set up dispersal function based on desired seed dispersal ratios
    #**# Umm, how do you go from a seed dispersal ratio to a seed dispersal function?
    #Wait, right now it's wired into the species setup.  So you should actually have the real
    #dispersal function give a seed dispersal rate of 0.  That's kind of confusing, actually.
    #It will work for the non-spatially explicit approach.  But if it is spatially explicit...
    # then this species generation approach would need to be revised.
    # such that the seed dispersal ratio came from the dispersal function.  And then a dispersal
    # function would have been specified.
    # but maybe I should start with specifying a dispersal function, then it will be more general
    # and won't need revision later.
    # do it later if you want this to be spatially explicit.  It will be easy enough to do.
    sp.data$dispersalfunction = 3 #Test a constant dispersal function
    sp.data$disppar1 = 0 #Seed disperse 0 cells - this will keep them all in the cell of origin
    sp.data$disppar2 = NA #Not applicable for constant dispersal function.  #**# hopefully this does not break anything.    

    #Write species file
    write.table(sp.data, file = sp.out.file, sep = ",", row.names = F, col.names = T)
    
    #Write environmental conditions file
    #Write header to file
    hdr = "env.lbl,landscape.identifiers,env.type,cover.level,param1,param2,cond.lyr,cond.vals,env.change.freq,env.change.type,env.change.mag\n"
    cat(hdr, file = env.file)

    #Write values to file
      #env.type == 1 is log-normal distribution, 6 = beta distribution based on a mean and "variance"
    #note: env.type through cond.vals really aren't applicable, as env.change.type will override the behavior due to the copula.
    fert.info = sprintf("1;%s;lnorm;%s;%s",these.env.cond$Ss.fert.cor,these.env.cond$fertility.mean,these.env.cond$fertility.var) #1 indicates copula number, lnorm indicates log-normal distribution
    fertility.data = sprintf("fertility,F,1,NA,%s,%s,NA,NA,1,from.file,%s\n",these.env.cond$fertility.mean,these.env.cond$fertility.var,fert.info)

    Ss.info = sprintf("1;%s;beta;%s;%s",these.env.cond$Ss.fert.cor,these.env.cond$Ss.mean,these.env.cond$Ss.var)
    Ss.data = sprintf("stochastic_seedling_survival,S,6,NA,%s,%s,NA,NA,1,from.file,%s\n", these.env.cond$Ss.mean,these.env.cond$Ss.var,Ss.info)

    cat(fertility.data, file = env.file, append = T)
    cat(Ss.data, file = env.file, append = T)
    
    }

#' Species Values Input
#'
#' Input species file for calculate traits %DD% Add documentation
#'
#' @details \itemize{
#' \item biomass.seed Biomass of the species' seed (= seed mass).  Used in thinning & reproductive trade-off\cr
#' \item biomass.adult Biomass of the adult.  Used in thinning law \cr
#' \item  biomass.clone Biomass of clones produced.  Used in thinning law & reproductive trade-off \cr
#' \item  g The proportion of reproductive effort allocated to producing clones \cr
#' \item  seed.disp.ratio The proportion of seeds dispersing to other cells in the landscape \cr
#' \item  w The proportion of seeds germinating at each time step \cr
#' \item  seed.mortality The proportion of seeds that die \cr
#' \item  adult.longevity The proportion of adults that survive each time step \cr
#' \item  Ss Stochastic seedling survival #DD# needs more documentation \cr
#' \item  k2 A parameter for the Weibull function that determines reproductive allocation \cr
#' \item  fertility A multiplier that determines how many seeds/clones are produced for a given reproductive allocation \cr
#' \item  sp.out.file The path and file where the new species file will be written (or of an existing file to be appended to) \cr
#' \item  do.append Whether to create a new file (F) or to append to an existing file (T) \cr
#' \item  n.seeds.o An option to override seed production (independent of the trade-off).  Both n.seeds.o and n.clones.o must be non-null to do the override. \cr
#' \item  n.clones.o An ooption to override clone production.  Both this and n.seeds.o must be non-null to override the trade-offs. \cr
#' \item  d12.o An option to override the thinning portion of the seed -> juvenile transition rate.  All d**.o must be non-null for the override to take place. \cr
#' \item  d23.o An option to override the thinning portion of the juvenile -> adult transition rate.  All d**.o must be non-null for the override to take place. \cr
#' \item  dc2.o An option to override the thinning portion of the clone -> juvenile transition rate.  All d**.o must be non-null for the override to take place. \cr
#' }
#' @name sp.vals
NULL

#DD# Add documentation for the environmental settings file
#DD# add in ' again when reenabling
#* # Environmental Settings Input
#* #
#* #
#* #
# # @details STUFF GOES HERE.
# #
# # @name these.env.cond
#NULL

# # NOTE: This is not actually used in Jakob's quantile regression script!
# # Compute partial correlations
# #
# # Definition of function that computes partial correlation matrix
# # Used by qr.bqr
# # %% I have absolutely no idea how this works!
# #
# # @author Jakob L.K. Gerstenlauer
#pcor2 <- function(x){
#    conc <- solve(var(x))
#    resid.sd <- 1/sqrt(diag(conc))
#    pcc <- - sweep(sweep(conc, 1, resid.sd, "*"), 2, resid.sd, "*")
#    return(pcc)
#    }

#' Z transform
#'
#' Function for z transforming input variables. Used by qr.bqr
#'
#' @param x an input variable
#' @author Jakob L.K. Gerstenlauer
Ztrans<-function(x){
    return( (x - mean(x)) / sd(x) )
    }

#DD# This function will likely need to be completely redone!
#' ForEach
#'
#' This function implements a simple algorithm transforming all variables that are part of the header vector with the function in FunctionName.
#' Note that all variables are overwritten! by default
#' The <<- operator creates objects in the global environment which is necessary here!
#'
#' @param DataFrame The data frame of reference
#' @param header A vector of type character containing the names of all variables
#' @param FunctionName The name of function which is used to transform all variables
#' @param Appendix appendix to new variable name
#' @author Jakob L.K. Gerstenlauer
ForEach <- function(DataFrame, header, FunctionName, Appendix=""){
    for (i in seq(1,length(header))){
        #print(paste(DataFrame,"$",header[i],Appendix,"<<-",FunctionName,"(",DataFrame,"$",header[i],")",sep=""))
        eval (parse (text=paste(DataFrame,"$",header[i],Appendix,"<<-",FunctionName,"(",DataFrame,"$",header[i],")",sep="")))
        }
    }


# # QR/BQR Header setup
# #
# # Write the header for the quantile regression, boosted quantile regression,
# # or correlation outputs
# #
# # @author Jakob L.K. Gerstenlauer and Sasha Keyel
#make.header = function(in.vars,run.type,out.file){
#
#    #NO LONGER NEEDED - HEADER IS FORMED AT A LATER STEP
#    #RunID is a linking function to join with the original header information
#    #Variable is whether it is loglambda or variation of log lambda
#    #Quantile is the quantile for which the data apply (not applicable for the correlations)    
#    preface = "RunID,Variable,Quantile"
#    if (run.type == "cor"){
#        preface = "RunID,Variable"
#        }
#    
#    in.vars = listtotext(in.vars,",") #convert variables to text
#    hdr = sprintf("%s,%s\n", preface, in.vars)
#    
#    #Write header to file
#    cat(hdr, file = out.file, append = FALSE)
#    }

#' Calculate modified coefficient of varation
#'
#' Calcualte modified coefficient of variation %% #DD# Add description for rationale here
#'
#' @param d2 A data frame
#'
#' @author Jakob Gerstenlauer, converted to function by Sasha Keyel
get.cov = function(d2){

    warning("get.cov is still under development.")

    #**# FLAG - I'm not sure a polynomial is the best fit/choice for this!
    #1) First a modified coefficient of variation (COV) is calculated which reflects the dispersion of stochastic growth independent of mean growth
    m2.lm<-lm(VarianceLogLambda ~ poly(LogLambda,2), d2)
    #use the response residuals from the polynomial model
    d2$COV<- residuals(m2.lm,"response")

    #**# FLAG - THIS CALCULATION SEEMS WRONG
    #rescale to zero as the lowest value #**# Why? Convenience? Or biology?
    MIN<-min(d2$COV)
    d2$COV<-d2$COV + abs(MIN) #**# Does this code actually work? Seems like you should subtract the minimum from all values

    return(d2)
    }


#' Calculate Z-transform
#'
#' @param d2 A data frame
#' @param in.vars input variables
#'
#' @author Jakob Gerstenlauer, converted to function by Sasha Keyel
do.z.transform = function(d2,in.vars){
    
    warning("do.z.transform is still under development")
    
    #Do some standardizations: first z-transform all predictors to provide standardized regression coefficients
    #compare H.Schielzeth 2010 Methods in Ecology & Evolution, 1, 103-113.

    #overwrite all predictors with ztransform
    predictors<-c("SeedDispRatio", "p01", "pveg", "ptbveg", "ptb0", "ptb2", "p22", "MeanSeedNumber");
    ForEach("d2", predictors, "Ztrans")

    #add interactions, e.g. monomials
    d2$p01.p22<- d2$p01 * d2$p22
    d2$p01.pveg<- d2$p01 * d2$pveg
    d2$pveg.p22<- d2$pveg * d2$p22
    d2$ptb2.p22<- d2$ptb2 * d2$p22
    d2$ptb2.p01<- d2$ptb2 * d2$p01
    d2$SDR.p01<- d2$SeedDispRatio * d2$p01
    d2$SDR.p22<- d2$SeedDispRatio * d2$p22
    d2$SDR.ptb2<-d2$SeedDispRatio * d2$ptb2

    predictors<-c("p01.pveg","pveg.p22","ptb2.p22","ptb2.p01","p12","p0s1","pveg1","p01.p22",
                   "SDR.p01", "SDR.p22", "SDR.ptb2");
    ForEach("d2", predictors, "Ztrans")

    #z transform response variables for quantile regression analysis
    d2$LogLambda.z<-Ztrans(d2$LogLambda)
    d2$COV.z<-Ztrans(d2$COV)
    return(d2)
    }

#' Calculatae spearman correlations
#'
#' Calculate spearman correlations for selected variables
#'
#' @param qr.cor.out qr.cor.out
#' @param RunID An ID
#' @param d2 A data frame
#' @param cor.vars cor.vars
#'
#' @author Jakob Gerstenlauer and Sasha Keyel
get.cors = function(qr.cor.out, RunID, d2, cor.vars){

    #test this
    LL.cors = c()
    COV.cors = c()
    for (this.var in cor.vars){
        LL.cor = cor(d2[["LogLambda"]],d2[[this.var]], method = "spearman")
        COV.cor = cor(d2[["COV"]],d2[[this.var]], method = "spearman")
        LL.cors = c(LL.cors,LL.cor)
        COV.cors = c(COV.cors,COV.cor)
        }

    size = length(cor.vars)
    cors.results = data.frame(rep(RunID,size),Predictor = cor.vars, LogLambdaCor = LL.cors, COVCor = COV.cors)

    write.table(cors.results, file = qr.cor.out, row.names = F, sep = ",") 
    return(cors.results)
    }

## Add ' back when reenabling function to renable roxygen
## Quantile regression helper function
##
## Helper function to implement the quantile regression
##
## @param d2 A data frame
## @param qr.results Results
## @param Response Response
## @param Predictor Predictor
## @param this.quantile The quantile of interest
## @param counter A counter
##
#do.qr = function(d2,qr.results,Response,Predictor, this.quantile,counter){
#    #Load required packages
#    if (!require(quantreg)){ stop("Please install quantreg: install.packages('quantreg')") }
#
#    Formula<- as.formula(paste(Response," ~ ",Predictor,sep=""))
#    model.qr<-quantreg::rq( Formula, tau = this.quantile, data=d2);
#
#    SummaryObject<-summary(model.qr)
#
#    Slope<-SummaryObject$coefficients[2,1]
#    StandardError<-SummaryObject$coefficients[2,2]
#
#    Response.SE = sprintf("%s.SE",Response)
#
#    qr.results[[Response]][counter] = Slope
#    qr.results[[Response.SE]][counter] = StandardError
#
#    return(qr.results)
#    }

#DD# Where does this function fit in?
#DD# How should it be implemented? Option in code or as a separate thing to run after code is completed?
#DD# What data is required to run it? Base vital rates, individual species outcomes.
#DD# so, this could be done outside the main function, as an analysis function, that reads in the species file and the summary info.
#DD# So, you'd need an extraction code to get the info of interest
#DD# Then you need to run the analysis
#DD# Then you need to output the results.
# Implement quantile regression
#
# Implement quantile regression
# %% Re-enable this line when code is functional and properly integrated: @export quant.reg
#
# @param qr.out File to which the results should be written. Needs to already exist with header information (created earlier in the code)
# @param RunID ID to join to global scenario conditions
# @param d2 Input dataframe containing values
# @param qr.vars A list of variables in the dataframe to use in quantile regression
# @param quantiles The quantiles at which to do the evaluations
#
#quant.reg = function(qr.out, RunID, d2,qr.vars,quantiles){
#    
#    quant.vals = sort(rep(quantiles,length(qr.vars))) # Get a vector with the quantile values for assigning to results
#    size = length(qr.vars) * length(quantiles) #Get a length variable for use in setting up dataframe
#    qr.results = data.frame(RunID = rep(RunID, size), Predictor = rep(qr.vars,length(quantiles)), Quantile = quant.vals, LogLambda.z = rep(NA,size),LogLambda.z.SE = rep(NA,size), COV.z = rep(NA,size),COV.z.SE = rep(NA,size))
#    
#    counter = 0
#    #Loop through quantiles
#    for (i in 1:length(quantiles)){
#        this.quantile = quant.vals[i]
#    
#        #Loop through variables
#        for (j in 1:length(qr.vars)){
#            counter = counter + 1 #Keep an index of what row the results should be written to.
#
#            #First add the target predictor
#            TargetPredictor = qr.vars[j]
#            
#            #then add all other predictors
#            Predictor = TargetPredictor
#            for (ii in 1:length(qr.vars)){
#                if (ii!=j){Predictor <-paste(Predictor," + ",qr.vars[ii],sep="")}
#                }
#            
#            Responses = c("LogLambda.z","COV.z")
#            for (Response in Responses){
#                qr.results = do.qr(d2,qr.results,Response,Predictor, this.quantile,counter)
#                }
#            LL.Response  <- "LogLambda.z";            
#            }
#        }
#    
#    write.table(qr.results, file = qr.out, row.names = F, append = F)
#    return(qr.results)
#    }
#
#Add ' back to reenable roxygen documentation
## Implement boosted quantile regression
##
## Implement boosted quantile regression
##
## @param bqr.out bqr.out
## @param RunID The run id
## @param d2 A data frame
## @param bqr.vars Variables to include in boosted quantile regression
## @param quantiles The quantiles to evaluate at
## @param NumberOfTrees Number of trees to use in Boosted Quantile Regression.
## 10,000 is suggested as a default. #DD# Look up source from main MS.
#boost.quant.reg = function(bqr.out, RunID, d2, bqr.vars, quantiles,NumberOfTrees=10000){
#    if (!require(gbm)){ stop("Please install gbm: install.packages('gbm')") }
#    
#    #Set up subset of data for analysis
#    ndata.LL = na.omit(subset(d2, select= c(bqr.vars,"LogLambda")))
#    ndata.COV = na.omit(subset(d2, select = c(bqr.vars,"COV")))
#        
#    #Loop through quantiles
#    for (this.quantile in quantiles){
#        LL.gbm = gbm::gbm(LogLambda ~ .,distribution=list(name="quantile",alpha=this.quantile), verbose=FALSE, interaction.depth=3, n.trees = NumberOfTrees, data=ndata.LL)
#        LL.summary = summary(LL.gbm)
#        LL.summary = LL.summary[ order(LL.summary$var) , ] #bring var into alphabetical order
#        
#        COV.gbm = gbm::gbm(COV ~ .,distribution=list(name="quantile",alpha=this.quantile), verbose=FALSE, interaction.depth=3, n.trees = NumberOfTrees, data=ndata.COV)
#        COV.summary = summary(COV.gbm)
#        COV.summary = COV.summary[ order(COV.summary$var) , ] #bring var into alphabetical order
#
#        #create a data frame from these results
#        size = length(bqr.vars)
#        bqr.sub = data.frame(RunID = rep(RunID, size), Predictor = sort(bqr.vars), Quantile = rep(this.quantile,size), RelImp.LogLambda = LL.summary[ ,2], RelImp.COV = COV.summary[ ,2])
#        
#        if (!exists("bqr.results")){
#            bqr.results = bqr.sub
#        }else{
#            bqr.results = merge(bqr.results, bqr.sub, all = T)
#            }
#        }
#
#    write.table(bqr.results, file = bqr.out, row.names = F, append = F, sep = ",")
#    return(bqr.results)
#    }

#**# NEED TO READ UP ON QUANTILE REGRESSION AND BOOSTED QUANTILE REGRESSION!!!
#**# THIS FUNCTION NEEDS REVISING/FIXING (need to add in annotation for roxygen2 as well. But it was doing something weird!
#   # Additional Analyses: Quantile Regression and Boosted Quantile Regression
#   #
#   # The purpose of this function is to conduct additional analyses on the SpatialDemography data
#   #
#   # @author Jakob L.K. Gerstenlauer, modified by Alexander "Sasha" Keyel
#   # %**%@export do.quantile.regression
#do.quantile.regression = function(in.file.base, out.file.base, quantiles){
#
#    #Temporary parameters for code testing
#    #in.file.base = "C:/docs/beplants/Scripts/test/"
#    #out.file.base = in.file.base
#    #quantiles <- c(0.15, 0.5, 0.85)
#
#    #**# Flag - with the new version of the model, more options are available - so you may need to amend these in light of the new model!
#    cor.vars = c("SeedDispRatio","p01","pveg","ptbveg","ptb0","ptb2","p22","MeanSeedNumber","p12","p0s1","pveg1") #List of variables to be examined for correlations
#    bqr.vars = cor.vars
#    qr.vars = c(bqr.vars,"p01.p22","p01.pveg","ptb2.p01","ptb2.p22","pveg.p22","SDR.p01","SDR.p22","SDR.ptb2") #bqr.vars plus interaction terms
#
#    #Set up out files
#    qr.out = sprintf("%sQuantileRegression.csv",out.file.base)
#    bqr.out = sprintf("%sBoostedQuantileRegression.csv", out.file.base)
#    qr.cor.out = sprintf("%sQR_Correlations.csv",out.file.base)
#
#    #Get in.files from in.file.base
#    #**# Get list of files in directory
#    #**# Go through list of files, and if they are the correct type, add to list of in.files
#    # Temporary patch for running the code
#    #in.file = sprintf("%sOutputParameters1022.txt", in.file.base)
#
#    #**# Left off here in revisions
#
#    for (in.file in in.files){
#        d2 = read.table(in.file, sep = "\t", header=TRUE)
#
#        #**# Create the RunID at some point
#
#        #1) Calculate modified coefficient of variation (COV) #Wait, this is calculated separately for each analysis? That seems like a problem!
#        d2 = get.cov(d2)
#
#        #**# Need to fix this to give interactions
#        #1b?) Standardize predictor variables #**# FLAG - Do we really want to do this?
#        #**# Figure out appropriate way to deal with interactions
#          #1: do interactions first, then standardize them
#          #2: do standardization first, then keep interactions
#          #3: standardize non-interactions, then calculate interactions, then standardize the results (Jakob's approach, but this just seems wrong to me!
#        d2 = do.z.transform(d2, qr.vars) #**# fix to deal with interaction terms
#
#        #2) calculate zero order spearman correlations between the mean and COV of the stochastic growth rate and all independent species traits
#        get.my.cors(qr.cor.out, RunID,d2,cor.vars)
#
#        #3) Calculate standardized quantile regression coefficients
#        quant.reg(qr.out, RunID, d2, qr.vars, quantiles)
#
#        #**# FLAG - It looks like the quantile regression is on standardized variables, while the boosted regression is on unstandardized variables - is this correct?
#        #4) Using stochastic gradient boosting to calculate a variable importance measure with both mean and COV of the stochastic growth rate for all traits and severall quantiles
#        boost.quant.reg(bqr.out, RunID, d2, bqr.vars, quantiles)
#        }
#    }


#' Import environmental layer
#'
#' Takes an existing environmental layer in the format of an R matrix or raster and
#' converts to the required input format for SpatialDemography. This may be used for 
#' multiple change steps for a single environmental layer (e.g., if there is
#' mowing data for multiple years)
#'
#' @param lyr.lst A list of environmental layer values (one for each change step)
#' @param lyr.format.lst A list specifying the format for each set of environmental layer values in lyr.lst
#' @param outfile The landscape file to be generated.
#'
#' @export import.env.lyr
import.env.lyr = function(lyr.lst, lyr.format.lst, outfile){
    
    if (length(lyr.lst) != length(lyr.format.lst)){
        stop("lyr.lst must have the same number of elements as lyr.format.lst!")
        }

    for (i in 1:length(lyr.lst)){

        lyr = lyr.lst[[i]]
        lyr.format = lyr.format.lst[[i]]

        #If using an input raster, do additional conversion requiring raster package
        if (lyr.format == "raster"){
            if (!require(raster)){ stop("Please install raster: install.packages('raster')" ) }
    
            #Read in raster object (Logic of the if statement: if the lyr is already in raster format (which will show up as typeof == "S4"), then don't change it
            if (typeof(lyr) != "S4"){
                lyr = raster::raster(lyr)
                }
    
            #convert to matrix format
            lyr = as.matrix(lyr) # WArNING: Watch out for problems downstream - as.matrix from base R is used in other parts of the code and the raster version might mask the normal behavior of this function in other places.
            }
        

        #Write header info
        if (i == 1){
            cells = seq(1, length(lyr))
            cell.lbls = sprintf("Cell%s",cells)
            hdr = c("ChangeStep",cell.lbls)
            hdr = matrix(hdr, nrow = 1) #Reformat for proper output
            write.table(hdr, file = outfile, sep = ",", row.names = F, col.names = F)
            }

        #Convert matrix format to line format for writing to landscape file
        lyr = matrix.as.vector(lyr)
        lyr = c(i,lyr) #Add i as change step column
        lyr = matrix(lyr, nrow = 1) #Reformat so that it is output as a row rather than a column.

        #Write to landscape file
        write.table(lyr, file = outfile, sep = ",", row.names = F, col.names = F, append = T)
        }        
    }

#' Make cumulative probabilities
#'
#' Converts from a series of individual transition probabilities to a cumulative transition probability
#' @param in.transitions a matrix of transition probabilities, with rows corresponding to the transition probabilities for each value
make.cum = function(in.transitions){
    
    #Create a variable to hold the outputs
    out.trans = in.transitions
    #loop through rows
    for (a.row in 1:nrow(in.transitions)){
        this.row = in.transitions[a.row, ]
        cum.tot = 0
        
        #Loop through elements in a row
        for (j in 1:length(this.row)){
            #Get the cumulative value for this row
            cum.tot = cum.tot + this.row[j]
            #Update the proper location with the cumulative value
            out.trans[a.row, j] = cum.tot        
            }
        }    
  
    return(out.trans)
    }

#' Check that cumulative probabilities are correctly made
#'
#' Check that cumulative probabilities are correctly made
#' @param in.vals input values
#' @param check.probs The probabilities to be checked
#' @param check.val The value that the last probability value must have
check.cum = function(in.vals, check.probs, check.val){
    #Check that the ending transitions are correct (1 for transition probabilities, 0 for delta probabilities)
    part1 = length(in.vals) #Get size of in.vals, to know where to check
    size = length(check.probs) #Get size of trans.probs, to know how many things to check
    interval = size / part1 #Get interval at which to evaluate
    
    for (i in 1:interval){
        eval.point = part1 * i
        
        if (check.probs[eval.point] != check.val){
            stop(sprintf("The last cumulative transition probability MUST be %s",check.val))
            }
        }
    }


#' Setup env.change.mag
#'
#' This is a helper function to facilitate setting up the env.change.mag field in
#' the Enviornmental_layers file. This may not be relevant for many environmental change types.
#' %% Add example for Markov version?
#' @param env.change.freq The frequency of environmental change. Mainly included to ensure that it is not 0
#' @param env.change.type The type of environmental change. For more info, see \link[spatialdemography]{env.change.type}
#' @param in.vals Input values excluding transition rates or changes in transition rates (changes to mean or sd SHOULD be included when appropriate for the change type)
#' @param in.transitions Only applies to Markov approaches. This should be a matrix of transitions, starting with the lowest cell value to each other cell value (1st row), then an additional row for each cell value.
#' @param in.delta.transitions Only applies to changing Markov approaches. This should indicate the change in transition probability.
#' @param trans.type Acceptable values: "ind", "cum". Only applies to Markov approaches. Indicates whether transition probabilities are given for each transition ("ind"), or cumulatively to include prior transitions ("cum").
#'
#' @export setup.env.change.mag
setup.env.change.mag = function(env.change.freq, env.change.type, in.vals = NA, in.transitions = NA, in.delta.transitions = NA, trans.type = NA){

    #Check for obvious, simple setup conditions
    if (env.change.freq == 0){ return("0")  } #env.change.mag can be 0/NA if the environment does not change    
    if (env.change.type == "swap") { return("0") } #env.change.mag can be 0 if the values are only swapped
    
    if (env.change.type == "cover.change.and.swap" | env.change.type == "rnorm.prob"){
        stop(sprintf("setup.env.change.mag does not yet support %s. Sorry for the inconvenience", env.change.type))
        }
    
    if (env.change.type == "landscape.wide.change"){
        #check that input is correct length
        if (length(in.vals) != 4){
            stop("Inappropriate input values. Values should be mean, standard deviation, change in overall mean, change in overall standard deviation, and should be in vector format.")
            }
        
        print(sprintf("mean: %s, sd: %s, change in mean: %s, change in sd: %s",in.vals[1],in.vals[2],in.vals[3],in.vals[4]))
        
        return(listtotext(in.vals, ";"))
        }
    
    if (env.change.type == "markov" | env.change.type == "markov.changing.transitions"){
        
        #Check input dimensions
        if ((length(in.vals)^2) != length(in.transitions)){
            stop("There must be a transition probability for every pair of input values.")
            }

        #Check that transition probabilities sum to 1 for individual transitions
        if (trans.type == "ind"){
            test = apply(in.transitions,1,sum)
            test = unique(test) #All values should be 1, so this should be length 1
            
            if (unique(test) != 1 | length(test) != 1){
                stop("Transition probabilities do not sum to 1. Did make a typo or did you mean to use cumulative probabilities?")
                }

            #Convert normal transition probabilities to cumulative transition probabilities
            in.transitions = make.cum(in.transitions)
            }
        
        #Convert probabilities to a vector for use in later code
        trans.probs = matrix.as.vector(in.transitions)
            
        #Check that the probabilities were constructed correctly (i.e. last value == 1)
        check.cum(in.vals,trans.probs,1)

        #Add internal delimiters
        in.vals.out = listtotext(in.vals,":")
        trans.probs.out = listtotext(trans.probs,":")
        
        out.vals = sprintf("%s;%s",in.vals.out,trans.probs.out)


        if (env.change.type == "markov.changing.transitions"){
            # Check input dimensions
            if (length(in.transitions) != length(in.delta.transitions)){
                stop("There must be a change value for each transition value")
                }

            #Convert delta.transitions to cumulative format
            if (trans.type == "ind"){
                in.delta.transitions = make.cum(in.delta.transitions)
                }
            
            delta.probs = matrix.as.vector(in.delta.transitions)

            #Check for an easy error in setting up cumulative changes in transitions
            check.cum(in.vals,delta.probs,0)

            delta.probs.out = listtotext(delta.probs,":")
            out.vals = sprintf("%s;%s",out.vals,delta.probs.out)            
            }

        return(out.vals)
        }
        
    stop("If the code reached this point, something went wrong in setup.env.change.mag. Was env.change.type correctly specified and supported by SpatialDemography?")
    }

#' Test Setup
#'
#' This function helps set up the file structure for the model accuracy/implementation test
#'
#' @param base.dir a base directory containing the relevant files
#' @param run.name the run name and output directory
#' @return Returns a list of the five files that are inputs to SpatialDemography
#' @export test.setup
test.setup = function(base.dir, run.name){
    
    #Create a default value for the locations file
    loc.file = "none"
    
    base.files = list.files(base.dir)

    # Create a folder for the test run & inputs
    inputs.dir = sprintf("%s/inputs/", run.name)    
    dir.create(inputs.dir, recursive = T)    
    
    #Initialize optional file names used in the SpatialDemography call (but not all are actually required)
    sri.file = spi.file = ""

    #Initialize species file
    sp.dir = sprintf("%s/Species/",run.name)
    dir.create(sp.dir, showWarnings = F)
    spf.file = sprintf("%sspecies_file_%s.csv",sp.dir,run.name)

    #loop through each file, and put a copy in the appropriate place    
    for (a.file in base.files){
    
        file.part = substr(a.file,1,3)
        b.file = sprintf("%s%s",base.dir,a.file)
        
        #initial conditions file
        if (file.part == "INC"){
            inc.file = sprintf("%sInitial_conditions_%s.csv",inputs.dir,run.name)
            file.copy(b.file,inc.file) }
        
        #Settings file
        if (file.part == "SET"){
            set.file = sprintf("%sSettings_%s.csv",inputs.dir,run.name)
            file.copy(b.file,set.file) }
            
        #Environmental layers file
        if (file.part == "ENL"){
            enl.file = sprintf("%sEnvironmental_layers_%s.csv",inputs.dir,run.name)
            file.copy(b.file,enl.file) }
        
        #Species file
        if (file.part == "SPF"){
            file.copy(b.file,spf.file) }
        
        #Species base instructions
        if (file.part == "SPI"){
            spi.file = sprintf("%s/inputs/Species_instructions_file_%s.csv",run.name,run.name)
            file.copy(b.file,spi.file) }
            
        #Species response instructions
        if (file.part == "SRI"){
            sri.file = sprintf("%sSpecies_response_instructions_%s.csv",inputs.dir,run.name)
            file.copy(b.file,sri.file) }
        
        #Environmental layers file(s)
        if (file.part == "ELF"){
            el.name = substr(a.file,5, (nchar(a.file) - 4)) #Get the file name exclusive of the code and the .csv
            enl.dir = sprintf("%s/landscape/",run.name)
            dir.create(enl.dir, showWarnings = F)
            this.enl.file = sprintf("%s%s_%s.csv",enl.dir,el.name,run.name)
            file.copy(b.file,this.enl.file) }
        
        #Species locations file
        if (file.part == "LOC"){
            loc.file = sprintf("%slocations_%s.csv",inputs.dir,run.name)
            file.copy(b.file,loc.file) }
        }    

    #Check that all required files have been created
    if (!exists("inc.file") | !exists("set.file") | !exists("enl.file")){
        stop("Not all required files were generated.  Check that an initial conditions file, settings file, and environmental layers file have been created")
        }

    return(list(inc.file,set.file,enl.file,spf.file,spi.file,sri.file,loc.file))
    }

#' Check outputs
#'
#' Check that the outputs generated by the test run match their expected values
#'
#' This will ensure that changes to the code, or to the user's operating system
#' do not change the overall model outcomes in unexpected ways.  The goal is to
#' test the main model options, and ensure that they are still producing the 
#' outputs one would expect from the definition of the code implementation.
#'
#' @param run.name The name of the run
#' @param RunLog A log for the run
#' @export check.outputs
check.outputs = function(run.name, RunLog){

    # WARNING: Copying and pasting out of excel was yielding an erratic number of decimal places (sometimes 5, sometimes 8!)
    #Rounding to 5 places as a patch.  But may need to replace all copied and pasted values (if you paste special instead of copying from formula, you get stable behavior with many more decimal places)
    #I've made rounding standard now, so more decimal places can be input.  This is relevant in that if a number ends in 5 (e.g., 0.65, rounds to 0.6), it will round it differently than if it ends in 51 (0.651 - rounds to 0.7)
    # NOTE: Need to set format to number, and give it at least 2 extra beyond what you want to round to (to avoid the 0.5 problem)
    #7 decimal places (rounding to 5) was not enough for matrix diagnostics, due to a 50 at the end of one value

    rl.cat("SpatialDemography executed successfully",RunLog)
    rl.cat(sprintf("%s Checks beginning",run.name),RunLog)
    
    ## Check that all output files were created & spot check values from output files
    # customize results for each run
    was.vdb = 1
    if (run.name == "TestRun1"){
        #NOTE: An alterantive approach would be to create copies of each of the files, and compare the copy version to the created version.  Then you could check EVERYTHING, and would not need huge text strings as inputs.
        #But it would not deal with an error where your template file becomes corrupted.  There is something nice about having the numbers explicitly here.  And column totals should be enough to check everything.
        
        #Order of entries for each vector:
        #rows,columns,grand.total,c(row.totals),c(col.totals),spotcheck1,spotcheck2,spotcheck3, headers
        # NOTE: Time file check removed because now timing file is optional and checked in the second vignette.
        #chk.time = list(45,3,NA,NA,NA,c(1,2,"Time"),c(8,1,"Dispersal probabilities generated"),c(45,1,"Code completed"),"N")

        #Grand total is 4813.057794, but character entries crash the script later (could convert these to factor and adjust the total)
        #results.row.tots = c(16121.000000,2356.028897,-13663.971103) #NA, because there are character arguments here.
        results.col.tots = c(NA,3,NA,NA,300,4,4.266666,4493.791128,4,4)
        chk.results = list(3,10,NA,NA,results.col.tots,c("c",3,"Scale"),c(1,9,5),c(3,7,-2.866667),"C")
 
        #Hint: Copy paste from excel (transpose to rows for row totals, then copy to TINN-R), use ctrl + R, check regular expressions box, and replace \t with ','.
        occ.row.tots = c(17,18,60,34,35,64,41,42,61,42,43,58,45,46,59,47,48,57,47,48,52,49,50,55,48,49,58,51,51,60,53,52,60,56,56,61,59,57,63,63,61,67,64,64,69,67,66,71,69,67,76,73,70,79,73,73,78,75,77,80,78,81,82,81,82,84,84,85,87,88,88,90,91,91,94,92,93,98,95,94,101,98,99,104,100,101,107,103,104,110,106,106,112,108,109,114,110,111,117,115)
        occ.col.tots = c(5050,158,79,0,0,0,903,343,0,794,0) 
        chk.occ = list(100,11,7327,occ.row.tots,occ.col.tots,c(3,8,11),c(100,11,0),c(90,7,15),"C")

        chg.row.tots = c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104)
        chg.col.tots = c(5050,253)
        chk.change = list(100,2,5303,chg.row.tots,chg.col.tots,c(25,2,2),c(100,2,4),c(49,1,49),"C")
        
        chk.loglambda = list(10,4,NA,NA,NA,c(1,1,-0.1197051),c(9,4,0.239478),c(6,3,0.007529),"A") #NOTE: fix this check to include a grand total and sub-totals once Inf problem is fixed
        chk.SSadult = list(100,21,NA,NA,NA,c(1,2,157.3888),c(100,20,-0.179403),c(56,9,0.018326),"C") # NOTE: Fix this when you fix the log lambda problem
        chk.SSall = list(100,21,NA,NA,NA,c(1,2,54157.3888),c(100,21,3.400067),c(78,7,18988.938449),"C")

        #In two lines of 50 entries, because one line seemed to be exceeding some limit of TINN-R!
        juv.row.tots = c(16001.0000000,20002.0000000,9532.0400000,9533.0400000,12118.9341000,6580.7209826,6581.7209826,8499.6119494,5195.2231538,5196.2231538,6808.1293903,4578.5979201,4579.5979201,6059.8718341,4385.3380115,4386.3380115,5847.4594333,4457.9709438,4458.9709438,5978.3748484,4732.6576163,4723.6576163,6362.4824498,5151.5892293,5146.4900293,6959.0643195,5717.3743230,4077.6275642,5504.4818766,4533.3016894,3323.5364424,4497.5029760,3677.8675905,2773.0496292,3800.5390990,3060.3457800,2388.1759281,3332.3360321,2623.0556088,2132.5079748,3052.0147215,2327.3277867,1981.4615266,2902.4132963,2146.4601132,1918.2901596,2878.0614262,2061.9038246,1931.8368538,2964.5436049,
                         2071.0471984,2015.1900186,2556.3911365,1725.7359195,1680.0684204,2481.6953299,1626.6452619,1585.5249595,2600.3175190,1667.8874988,1637.4606521,2846.9340173,1799.1844973,1762.8495523,3193.2595349,1997.8312875,1955.5090355,3629.9221896,2255.1636329,2205.9920726,4167.7967321,2570.0372444,2512.8533217,4790.0144251,2945.7127674,2879.1582588,5523.7835754,2177.4753653,2109.5594582,4163.9559453,1854.4453918,1779.6591765,3621.3233375,1747.4151886,1680.5487102,3510.8783326,1775.1298537,1713.3585234,3651.9343401,1891.0604818,1841.9102149,3961.3414773,2074.3096080,2022.9343288,4403.3793056,2316.9030909,2262.2267348,4966.2199462,2617.7851443,2558.7136836)        
        juv.col.tots = c(5050.0000000,27432.6619165,16348.8960201,51.9701191,44.1542224,40.3735813,149163.7483060,37129.9576166,41.9021585,153437.6441818,108.2462440)
        chk.SSJuv = list(100,11,388849.554366,juv.row.tots,juv.col.tots,c(1,2,4000),c(100,11,0.1724852),c(5,10,3837.3025),"C")

        #In two lines of 50 entries, because one line seemed to be exceeding some limit of TINN-R!
        seed.row.tots = c(200001.0000000,95292.4000000,95293.4000000,121143.3410000,65752.2098256,65753.2098256,84923.1194941,51870.2315379,51871.2315379,67981.2939025,45676.9792006,45677.9792006,60471.7183414,43717.3801149,43718.3801149,58320.5943326,44416.7094380,44417.7094380,59602.7484838,47136.5761635,47037.5761635,63416.8244984,51298.8922931,51238.9002931,69355.6431953,56929.7432301,40523.2756422,54782.8187658,45062.0168937,32955.3644236,44686.0297605,36480.6759055,27423.4962921,37689.3909896,30278.4577999,23547.7592808,32980.3603213,25878.5560883,20964.0797481,30150.1472147,22894.2778671,19426.6152664,28627.1329628,21058.6011316,18767.9015957,28356.6142625,20186.0382465,18876.3685379,29194.4360487,20250.4719842,
                          19682.9001856,25085.9113654,16770.3591945,16304.6842037,24311.9532993,15752.4526194,15332.2495953,25471.1751905,16137.8749878,15824.6065215,27910.3401731,17423.8449728,17051.4955234,31346.5953493,19383.3128753,18951.0903553,35686.2218959,21929.6363295,21428.9207264,41037.9673208,25051.3724436,24470.5332170,47233.1442507,28781.1276738,28106.5825882,54543.8357542,21071.7536527,20383.5945824,40918.5594527,17814.4539183,17057.5917654,35465.2333754,16717.1518861,16039.4871021,34333.7833259,16967.2985375,16340.5852340,35717.3434012,18099.6048181,17599.1021486,38784.4147735,19905.0960803,19382.3432881,43177.7930560,22304.0309094,21748.2673483,48779.1994618,25285.8514431,24686.1368357,55698.9815126)        
        seed.col.tots = c(5050.0000000,234326.9773246,133489.0588075,519.7161996,441.6363186,403.7358126,1525687.7708736,341525.9222388,419.1542563,1495646.0987093,1134.4546351)
        chk.SSseed = list(100,11,3738644.525176,seed.row.tots,seed.col.tots,c(1,2,50000),c(100,11,51.9921946),c(29,6,13.617064),"C")

        matdiag.row.tots = c(69.59812855,70.596257,71.596257,72.59812855,73.59812855,75.11708996,76.11247886,76.59812855,78.11118759,79.11118759,70.59084518,71.5935497,72.5935497,73.59084518,74.59084518,76.00543201,77.10601083,77.59084518,79.10582685,80.10582685,71.59638013,72.59629472,73.59629472,74.59638013,75.59638013,77.11181031,78.11258207,78.59638013,80.10765322,81.10765322,72.59420323,73.59257921,74.59257921,75.59420323,76.59420323,78.00769945,79.1058636,79.59420323,81.10601419,82.10601419)
        matdiag.col.tots = c(100.0000000,220.0000000,38.72645501,40.0000000,40.0000000,38.3901292,33.0836104,28.2957258,415.0000000,415.0000000,415.0000000,415.0000000,415.0000000,415.0000000,3.3200000) #7 decimal places was not enough, due to the 0.5 problem
        chk.matdiag = list(40,15,3031.815920,matdiag.row.tots,matdiag.col.tots,c(1,3,0.916223),c(40,15,0.083),c(22,9,10.375),"C") #TRUE counts as 1, False as 0.

        #VDB inputs
        spdat.col.tots = c(22000.0000000,202000.0000000,159969.3678078,225137.0719933,230420.1294484,244149.7284733,261371.6202708,1145713.1742795,459748.3052263,341238.1876533,1110709.3326962,534106.4663334,1920273.6995709,322819.9489757,542625.4587332,299973.0956467,247360.0283679,246154.5365964)
        chk.spdat = list(4000,18,8515770.152073,NA,spdat.col.tots ,c(1,1,1),c(4000,18,0.015653),c(1900,13,0.001151),"A2") #Too many row totals for me to conveniently reproduce here!  Will hope that the grand total and column checks will be sufficient!
        amat.row.tots = c(2004.372403,2005.372403,2006.372403,2007.372403,2008.372403,2009.62976,2010.57072,2011.372403,2012.57072,2013.57072,2005.355814,2006.355814,2007.355814,2008.355814,2009.355814,2010.646349,2011.5448,2012.355814,2013.5448,2014.5448,2006.372403,2007.372403,2008.372403,2009.372403,2010.372403,2011.62976,2012.57072,2013.372403,2014.57072,2015.57072,2007.355814,2008.355814,2009.355814,2010.355814,2011.355814,2012.646349,2013.5448,2014.355814,2015.5448,2016.5448)
        chk.amat = list(40,2306,80417.983949,amat.row.tots,NA,c(1,3,0),c(40,2306,0),c(40,2305,0.04096),"C") #Too many col totals for met to conveniently reproduce here!
        
        amatsum.col.tots = c(1600,3520,5440,536.0202456,6640,6640,53.12)
        chk.amatsum = list(640,7,24429.140246,NA,amatsum.col.tots ,c(640,7,0.083),c(1,1,1),c(1,4,0.669792),"C")

        tmat.col.tots = c(1600,3520,5440,0,0,73360,64,0,0,0,33.9839488,0)
        chk.tmat = list(640,12,84017.983949,NA,tmat.col.tots,c(640,12,0),c(1,1,1),c(99,7,0.1),"C")
        
        chk.lst = list(chk.results,chk.occ,chk.change,chk.loglambda,
                       chk.SSadult,chk.SSall,chk.SSJuv,chk.SSseed,chk.matdiag,
                       chk.spdat,chk.amat,chk.amatsum,chk.tmat) #chk.time,
        }
        
    if (run.name == "TestRun2"){

        #Order of entries for each vector:
        #rows,columns,grand.total,c(row.totals),c(col.totals),spotcheck1,spotcheck2,spotcheck3, headers
        #NOTE: Time file check removed because time file is checked in second vignette.
        #chk.time = list(39,3,NA,NA,NA,c(1,1,"Process"),c(26,1,"Seedling competition step finished"),c(36,1,"Calculate change in metrics"),"N")

        #Grand total: 687673.5990616 (but text entries error it, hence NA.
        results.col.tots = c(NA,3,NA,NA,15,4,2,687605.599062,4,4) 
        chk.results = list(3,10,NA,NA,results.col.tots,c("c",6,"Sp.Rich"),c(1,2,1),c(2,10,2),"C")
 
        #Hint: Copy paste from excel (transpose to rows for row totals, then copy to TINN-R), use ctrl + R, check regular expressions box, and replace \t with ','.
        occ.row.tots = c(21,34,35,36,37)
        occ.col.tots = c(15,73,75) 
        chk.occ = list(5,3,163,occ.row.tots,occ.col.tots,c(1,2,9),c(5,3,16),c(1,3,11),"C")

        chg.row.tots = c(2,3,5,6,7)
        chg.col.tots = c(15,8)
        chk.change = list(5,2,23,chg.row.tots,chg.col.tots,c("c",1,"RunTime"),c(5,2,2),c(3,2,2),"C")
        
        chk.loglambda = list(2,4,2.475480642,c(1.4122923,1.063188343),c(0.515552855,0.220388037,1.223721822,0.515817928),c(1,1,0.319909889),c("c",2,"sd.all"),c("r",2,"Sp2"),"A") 

        SSadult.row.tots = c(2814.2288,7700.435113,12834.15028,21324.89222,34386.19393)
        SSadult.col.tots = c(15.0000000,49740.1116811,29299.8937780,2.7498403,2.1450470)
        chk.SSadult = list(5,5,79059.9003464,SSadult.row.tots,SSadult.col.tots,c(1,2,1458),c(5,5,0.3861654),c("c",3,"Sp2"),"C") #na.rm = T below, so do not include NA's when summing (but don't worry about them either

        SSall.row.tots = c(302457.9788,332577.77679653,469500.590225319,629511.36671288,855088.983315117)
        SSall.col.tots = c(15.0000000,1377682.5255441,1211437.1080943,1.2796396,0.7825719)
        chk.SSall = list(5,5,2589136.6958499,SSall.row.tots,SSall.col.tots,c("c",4,"log.lambda.Sp1"),c(5,5,0.244916),c(1,3,164993.9788),"C")

        juv.row.tots = c(81332.25,101388.7808100,203059.2952263,315086.7419066,494723.6119617)
        juv.col.tots = c(15,684613.8642459,510961.8156586)
        chk.SSJuv = list(5,3,1195590.6799046,juv.row.tots,juv.col.tots,c("c",2,"Sp1"),c(1,1,1),c(5,3,190093.437411564),"C")

        seed.row.tots = c(218313.5,223494.3606000,253613.456890,293108.1378578,325989.4929269)
        seed.col.tots = c(15,643328.5496171,671175.3986577)
        chk.SSseed = list(5,3,1314518.9482748,seed.row.tots,seed.col.tots,c("c",3,"Sp2"),c(5,3,159186.26977859),c(2,3,120603.6106),"C")

        matdiag.row.tots = c(74.7497218,75.0433275,75.8432597,75.3379262)
        matdiag.col.tots = c(6.0000000,6.0000000,8.8943991,4.0000000,4.0000000,8.8595318,7.4017187,6.4865857,41.5000000,41.5000000,41.5000000,41.5000000,41.5000000,41.5000000,0.3320000)
        chk.matdiag = list(4,15,300.9742353,matdiag.row.tots,matdiag.col.tots,c(1,3,2.3230986),c("c",3,"A.Eigen"),c("c",15,"Prop.Dispersing"),"C") #TRUE counts as 1, False as 0.

        #VDB inputs
        spdat.row.tots = c(99302.0000000,36707.0000000,1460.0000000,137465.0000000,119015.5000000,44629.2500000,1358.2288000,164996.9788000,102891.7500000,49803.0000000,4384.0824000,157072.8324000,120607.6106000,51590.7808100,3319.3578880,175509.7492980,123273.9509500,110972.0743450,7650.1802800,241888.2055750,130345.5059400,92093.2208813,5188.9660868,227617.6929080,151076.6255188,182520.6153508,13456.5328714,347043.7737409,142038.5123391,132573.1265558,7874.3771799,282474.0160748,166804.2231483,304631.1745502,22809.3161297,494232.7138282,159193.2697786,190100.4374116,11583.9638234,360863.6710135)
        spdat.col.tots = c(60.0000000,120.0000000,303896.7201762,313242.7493124,455272.5429151,469031.9089386,319093.5617628,270247.8404904,228670.5310556,284787.5029071,233877.1553618,303369.6860848,443103.7410257,455272.5429151,234313.2536910,275201.4720508,277363.6805300,311494.3780596)
        chk.spdat = list(40,18,5178419.2672769,spdat.row.tots,spdat.col.tots ,c(1,2,1),c(39,18,43.20325014),c(15,4,521),"A2")

        amat.row.tots = c(2537.2659200,2537.7605376,2538.2400000,2538.7346176)
        chk.amat = list(4,2306,10152.0010752,amat.row.tots,NA,c("c",2306,"Entry_2304"),c("c",1,"ChangeStep"),c(2,308,0.5),"C") #Too many rows for totals
        
        amatsum.row.tots = c(26.1368234,26.4968572,28.1368234,29.1368234,30.1368234,30.7810617,31.7810617,32.7810617,34.1368234,34.4968572,36.1368234,37.1368234,37.7810617,38.7810617,39.7810617,41.1368234,26.4968572,28.1368234,28.4968572,29.4968572,30.4968572,31.2703334,32.2703334,33.7810617,34.4968572,36.1368234,36.4968572,37.4968572,38.2703334,39.2703334,40.7810617,41.4968572,26.7810617,28.1368234,28.7810617,30.1368234,31.1368234,31.7810617,33.1368234,33.7810617,34.7810617,36.1368234,36.7810617,37.7810617,38.7810617,39.7810617,41.1368234,41.7810617,27.2703334,28.4968572,29.7810617,30.4968572,31.4968572,32.7810617,33.4968572,34.7810617,35.2703334,36.4968572,37.2703334,38.7810617,39.7810617,40.7810617,41.4968572,42.2703334)
        amatsum.col.tots = c(96,96,544,121.7350371,664,664,5.312)
        chk.amatsum = list(64,7,2191.0470371,amatsum.row.tots,amatsum.col.tots,c("c",4,"Dominant.EigenValue"),c("c",1,"ChangeStep"),c(63,7,0.083),"C")

        tmat.row.tots = c(150.0625000,151.0034600,152.0625000,153.0625000,154.0625000,155.0265000,156.0265000,157.0265000,158.0625000,159.0034600,160.0625000,161.0625000,162.0265000,163.0265000,164.0265000,165.0625000,151.0034600,152.0625000,153.0034600,154.0034600,155.0034600,155.9887144,156.9887144,158.0265000,159.0034600,160.0625000,161.0034600,162.0034600,162.9887144,163.9887144,165.0265000,166.0034600,151.0265000,152.0625000,153.0265000,154.0625000,155.0625000,156.0265000,157.0625000,158.0265000,159.0265000,160.0625000,161.0265000,162.0265000,163.0265000,164.0265000,165.0625000,166.0265000,151.9887144,153.0034600,154.0265000,155.0034600,156.0034600,157.0265000,158.0034600,159.0265000,159.9887144,161.0034600,161.9887144,163.0265000,164.0265000,165.0265000,166.0034600,166.9887144)
        tmat.col.tots = c(96.0000000,96.0000000,544.0000000,3.2000000,0.0000000,7336.0000000,6.4000000,1.6000000,2013.6000000,0.0000000,4.0010752,44.8000000)
        chk.tmat = list(64,12,10145.6010752,tmat.row.tots,tmat.col.tots,c(64,12,0.7),c(1,1,1),c("c",7,"p12"),"C")
        
        chk.lst = list(chk.results,chk.occ,chk.change,chk.loglambda,
                       chk.SSadult,chk.SSall,chk.SSJuv,chk.SSseed,chk.matdiag,
                       chk.spdat,chk.amat,chk.amatsum,chk.tmat) #chk.time,
        }

    if (run.name == "TestRun3"){
        stop("This option is not yet programmed!")
        }


    if (!exists("chk.lst")){
        stop(sprintf("Error in doing checks for %s.  Please note that checks have only been programmed for TestRuns 1 & 2", run.name))
        }

    do.standard.checks(run.name,RunLog,chk.lst,was.vdb)

    rl.cat(sprintf("%s Checks completed successfully",run.name),RunLog)    
    }

#' Implement checks of data
#'
#' Implement checks of data
#' @details \itemize{
#' \item do.standard.checks These are checks that EVERY test run should pass, given the correct inputs \cr
#' \item do.vdb.checks These are checks that the visual debugger data output correctly \cr
#' \item do.chg.checks Check that environmental change was implemented correctly \cr
#' }
#'
#' @param run.name Need file path for values to check
#' @param RunLog File logging the testing procedure
#' @param chk.lst List of inputs for values specific to the run
#' @param was.vdb indicator for whether visual debugger data were generated
#'
do.standard.checks = function(run.name,RunLog,chk.lst, was.vdb = 1){
    #Check existence of files
    path1 = sprintf("%s/outputs/",run.name)
    path2 = sprintf("%s/%s/",run.name,run.name)
    path3 = sprintf("%sDiagnostics/",path2)
    path.vec = c(rep(path1,1),rep(path2,7),path3) #rep(path1,2)
    name.vec = c("Results","Cells_occupied","change_count_lookup",
                 "LogLambda","SpeciesStats_Adults_v2","SpeciesStats_All_v2",
                 "SpeciesStats_Juvs","SpeciesStats_Seeds","MatrixDiagnostics") #"Timefile_",
    format.vec = c(rep(1,1),rep(2,7),2) #rep(1,2)

    if (was.vdb == 1){
        vdb.paths = c(path2,rep(path3,3))
        vdb.names = c("SpeciesData","AMatrices","AMatricesSummaries","TransitionMatrices")
        vdb.formats = c(2,rep(2,3))
        path.vec = c(path.vec,vdb.paths)
        name.vec = c(name.vec,vdb.names)
        format.vec = c(format.vec,vdb.formats)
        }
    
    #Check that all three vectors have the correct length
    if (length(path.vec) != length(name.vec) | length(path.vec) != length(format.vec)){
        stop("Error in do.standard.checks: vectors for checking inputs do not match")
        }
    
    for (a in 1:length(path.vec)){  #1){ #
        in.path = path.vec[a]
        in.name = name.vec[a]
        in.format = format.vec[a]
        in.chk = chk.lst[[a]]
        
        #Check that file exists
        chk.file(in.path,in.name,in.format,in.chk,run.name,RunLog)
        
        }
    
    #NOTE: Consider adding a check that landscape files are correct & figure out how to do this in a scenario specific way    
    }

#' Check file
#'
#' Check existence of a file & accuracy of values, based on specified inputs
#'
#' @param in.path in.path
#' @param in.name in.name
#' @param in.format in.format
#' @param in.chk in.chk
#' @param run.name Run name
#' @param RunLog A log
#'
chk.file = function(in.path,in.name,in.format,in.chk,run.name,RunLog){
    if (in.format == 1){
        test.file = sprintf("%s%s%s.csv",in.path,in.name,run.name)
        }
    if (in.format == 2){
        test.file = sprintf("%s%s.csv",in.path,in.name)
        }
        
    if (!file.exists(test.file)){
        err.mess = sprintf("No %s file created (or the file was created in an incorrect location",in.name)
        stop(err.mess)
        }
    no.err = sprintf("%s file exists in the correct location",in.name)
    rl.cat(no.err,RunLog)

    #Check accuracy of inputs    
    #unpack in.chk
    t.rows = in.chk[[1]]
    t.col = in.chk[[2]]
    t.tot = in.chk[[3]]
    t.row.tot = in.chk[[4]]
    t.col.tot = in.chk[[5]]
    t.spot1 = in.chk[[6]]
    t.spot2 = in.chk[[7]]
    t.spot3 = in.chk[[8]]
    headers = in.chk[[9]]
    
    #Read in file
    is.error = 0
    #A takes row names and column names
    if (headers == "A"){
        test.dat = read.csv(test.file,row.names = 1) #read.table was giving an error of "too many column names for columns", and I have no idea why (it's a .csv, for crying out loud!)
        }
    
    if (headers == "A2"){
        test.dat = read.csv(test.file)
        proto.row.names = test.dat[ ,1]
        rownames(test.dat) = sprintf("%s%s",seq(1,length(proto.row.names)),proto.row.names)
        test.dat[ ,1] = NULL
        }
    
    #N indicates no row or column names    
    if (headers == "N"){
        test.dat = read.table(test.file, sep = ",")
        }
    #C indicates only column names
    if (headers == "C"){
        test.dat = read.csv(test.file)
        }
    
    if (nrow(test.dat) != t.rows){
        is.error = 1
        rl.cat(sprintf("Row numbers did not match, %s != %s",nrow(test.dat),t.rows),RunLog)  }
    if (ncol(test.dat) != t.col){
        is.error = 1
        rl.cat(sprintf("Col numbers did not match, %s != %s",ncol(test.dat),t.col),RunLog)  }
    if (!is.na(t.tot)){ #NA will indicate that a row/column check is inappropriate
        test.tot = sum(test.dat,na.rm = T)
        test.tot = round(test.tot,5) #Round to 5 places
        t.tot = round(t.tot,5) #make sure t.tot is to 6 places
        if (test.tot != t.tot){
            is.error = 1
            rl.cat(sprintf("Grand totals did not match, %s != %s",test.tot,t.tot), RunLog)
            }
        }
    if (!is.na(t.row.tot[1])){
        test.row.tot = apply(test.dat,1,sum, na.rm = T)
        test.row.tot = round(test.row.tot,5)
        t.row.tot = round(t.row.tot, 5) #Check that this is 6 places as well
        test.row.tot = listtotext(test.row.tot,";")
        t.row.tot = listtotext(t.row.tot,";")
        if (test.row.tot != t.row.tot){
            is.error = 1
            rl.cat(sprintf("Row totals do not match, %s != %s",test.row.tot,t.row.tot), RunLog)
            }
        }
    if (!is.na(t.col.tot[1])){
        test.col.tot = apply(test.dat,2,sum, na.rm = T)
        test.col.tot = round(test.col.tot,5)
        t.col.tot = round(t.col.tot,5)
        test.col.tot = listtotext(test.col.tot,";")
        t.col.tot = listtotext(t.col.tot,";")
        if (test.col.tot != t.col.tot){
            is.error = 1
            rl.cat(sprintf("Column totals do not match, %s != %s",test.col.tot,t.col.tot), RunLog)
            }
        }
    
    is.error = do.spot.chk(t.spot1,test.dat,is.error,RunLog)
    is.error = do.spot.chk(t.spot2,test.dat,is.error,RunLog)
    is.error = do.spot.chk(t.spot3,test.dat,is.error,RunLog)
    
    if (is.error == 1){
        stop(sprintf("There were one or more problems with file %s. See RunLog for details",in.name))
        }
    }

#' Spot check
#'
#' Custom function for doing spot checks
#'
#' @param t.spot t.spot
#' @param test.dat test.dat
#' @param is.error Indicator for whether or not an error occurred
#' @param RunLog A run log
#'
do.spot.chk = function(t.spot,test.dat,is.error,RunLog){

    spot.val = t.spot[3]
    
    #r indicates a spot check should check a row name
    if (t.spot[1] == "r"){
        pos = as.num(t.spot[2])
        r.names = rownames(test.dat) #row.names is used for matrices, rownames for data frames
        test.val = r.names[pos]
        }
    
    #c indicates a spot check should check a column name
    if (t.spot[1] == "c"){
        pos = as.num(t.spot[2])
        c.names = colnames(test.dat) #not col.names, as that is used for matrices
        test.val = c.names[pos]
        }
        
    if (t.spot[1] != "r" & t.spot[1] != "c"){    
        spot.row = as.num(t.spot[1])
        spot.col = as.num(t.spot[2])
        test.val = test.dat[spot.row,spot.col] #Get test value from the data frame
        }
    
    #This one is more complicated, because test.val might not equal spot.val because of rounding issues
    if (test.val != spot.val){
    
        temp.err = 1 #can't use is.error, because that is function wide.  Indicate an error is possible
        
        #check if mismatch is due to rounding (but note that character inputs are also possible!)
        test.val = as.num(test.val)
        #if test.val is character, the above step will yield NA, and the error is legitimate
        if (!is.na(test.val)){
            test.val = round(test.val,5)
            spot.val = round(spot.val,5)
            if (test.val == spot.val){
                temp.err = 0
                }
            }
            
        if (temp.err == 1){ 
            is.error = 1
            rl.cat(sprintf("Spot check failed %s != %s",test.val,spot.val),RunLog)
            }
        }
    return(is.error)
    }

#' RunLog cat
#'
#' Customized cat function for writing to RunLog
#'
#' @param in.text Input text to be written
#' @param RunLog the log to write to.
rl.cat = function(in.text,RunLog){
    in.text = sprintf("%s\n",in.text) #Add new line character that I always forget
    cat(in.text, file = RunLog, append = T) #include append = T call, which I forgot in most previous calls
    }    



####################### BROKEN FUNCTIONS BELOW HERE ############################

##Eigenvalues for each cell for each species
##The overall A eigenvalue for each species
##Dispersal probabilities (or do we want it as observed dispersal?) for each species.
# NOTE: Replaced by compute.diagnostics
#LocalTransitionMatrix = function(SeedDispersalRatio,S,B1.Grassland,B1.Cropland,B1.Forest,B2.Grassland,B2.Cropland,B2.Forest){
#    #Calculate a local transition matrix L_habitat which is the product of B1 and B2 and O a matrix describing only the loss of seeds        
#    #set-up of O: (u: seed dispersal ratio)
#    #(1)(2)(3)(4)
#    # 1-u 0 0 0
#    #  0  1 0 0
#    #  0  0 1 0
#    #  0  0 0 1
#    #
#    #(1):mobile seeds, (2):seed bank, (3): juveniles, (4):adults
#    
#    O <- sparseMatrix(i= c(1,2,3,4), j= c(1,2,3,4),
#                    x= c((1 - SeedDispersalRatio), 1, 1, 1),
#                    dims = c(S,S))
#    
#    #for grassland patches:
#    B.Grassland <- B2.Grassland  %*% O %*% B1.Grassland
#    
#    #for cropland patches:
#    B.Cropland <- B2.Cropland %*% O %*%  B1.Cropland
#    
#    #for forest patches:
#    B.Forest <- B2.Forest %*% O %*% B1.Forest
#    ltm.out = list(O,B.Grassland,B.Cropland,B.Forest)
#    return(ltm.out)
#    }
#
#
#NOTE: Replaced by compute.diagnostics & code is broken due to changes in the code
##Calculate the eigenvalues for the main matrix for each species for the population model
#CalcA = function(B1.lst,B2.lst,P,M.lst,change.count,outpath.base){
#    
#    #Set up file output
#    A.out.path = sprintf("%sAeigen/",outpath.base)  
#    dir.create(A.out.path,recursive = T,showWarnings = F)
#    A.out.file = sprintf("A_%s.csv",change.count)
#    A.header = "Species,Matrix Eigen Value A"
#    cat(A.header,file = A.out.file)
#    
#    #Simple slow way (but may not be optimal
#    for (sp in 1:length(B1.lst)){
#        B1 = B1.lst[[sp]]
#        B2 = B2.lst[[sp]]
#        M = M.lst[[sp]]
#        
#        #This is the way Jakob originally framed it, so this should be correct
#        A <- B2 %*% t(P) %*% M %*% P %*% B1
#
#        A.eigen<-eigen(A, only.values=TRUE)
#        A.eigen = max(A.eigen$values) # NOTE: This fails when A gives (only?) complex numbers
#        
#        #Output the eigenvalues in a way that can be accessed by the visual debugger.
#        this.output = sprintf("%s,%s",sp,A.eigen)
#        cat(this.output, file = A.out.file, append = T)
#        
#        }
#        
#    #return(list(A,A.eigen))
#    }
#
##NOTE: Replaced by compute.diagnostics    
#### Functions pertaining to Eigen-analysis and perturbation analysis
##Calculate eigenvalues for local transition matrices
#  #WARNING: NEED to add change.count
#CalcEigen = function(ToRun,in.df,sp,B.Grassland,B.Forest,B.Cropland){
#
#    #start with the grassland local transition matrix
#    GM.eigen<-eigen(B.Grassland)
#    dom.pos<-which.max(GM.eigen$values)
#    GM.dom.eigenv<-Re(GM.eigen$values[dom.pos]) #the dominant eigenvalue
#    
#    #forest local transition matrix
#    FM.eigen<-eigen(B.Forest)
#    dom.pos<-which.max(FM.eigen$values) #which.max gets the location of the local maximum
#    FM.dom.eigenv<-Re(FM.eigen$values[dom.pos]) # NOTE: Re takes just the real part of a complex number
#    
#    #cropland local transition matrix    
#    CM.eigen<-eigen(B.Cropland)
#    dom.pos<-which.max(CM.eigen$values)
#    CM.dom.eigenv<-Re(CM.eigen$values[dom.pos])
#
#    #Write outputs to file
#    if (ToRun == 1){
#        in.df$Grassland_Eigenvalue[sp] = GM.dom.eigenv
#        in.df$Forest_Eigenvalue[sp] = FM.dom.eigenv
#        in.df$Cropland_Eigenvalue[sp] = CM.dom.eigenv
#        #cat(file=infile, sprintf("%s,",GM.dom.eigenv),append=TRUE)
#        #cat(file=infile, sprintf("%s,",FM.dom.eigenv),append=TRUE)
#        #cat(file=infile, sprintf("%s,",CM.dom.eigenv),append=TRUE)
#        }
#    
#    return(list(list(GM.dom.eigenv,FM.dom.eigenv,CM.dom.eigenv),in.df))
#    }
 
##Calcualte Perturbation-analysis of the dispersal matrix  
#CalcPerturbation = function(ToRun,infile,A,A.eigen,p,DispersalProbabilities){
#
#    #standardize the matrix by dividing through the dominant eigenvalue
#    As <- A / Re(A.eigen$values[1])
#    
#    #Here we calculate several indices proposed by Neubert and Caswell(1997) and Stott et al. (2010) using R code provided by Stott et al.(2010)
#    # We omit the upper and lower Kreissbound indices because the calculation takes very long or even fails
#    # and we know from Stott et al. Table 2 that the indices are strongly correlated with the rho_max and rho_min    
#    rhomax.Stott        <- rhomax(As,30)
#    rhomin.Stott        <- rhomin(As,30)
#    reac.Stott          <- reac(A)
#    firststepatt.Stott  <- firststepatt(A)
#        
#    #Dispersal submatrix Mij
#    #For the dispersal submatrix, the interpretation of eigenvalues and vectors is not clear!
#    DispM<-matrix(as.vector(DispersalProbabilities), nrow=p,byrow=TRUE)
#    #image(DispM)
#    DispM.eigen<-eigen(DispM, symmetric=TRUE)
#    
#    #the stable stage distribution
#    dom.pos<-which.max(DispM.eigen$values)
#    DispM.StableStageDist<-DispM.eigen$vectors[,dom.pos]/sum(DispM.eigen$vectors[,dom.pos])
#    #the stable spatial distribution of the landscape
#
#    #the damping ratio as a measure of the rate of convergence towards the stable spatial distribution
#    #compare: ""Shima, Noonburg, Phillips (2010) Life history and matrix heterogeneity interact to shape
#    #metapopulation connectivity in spatially structured environments, Ecology, 91 (4) pp. 1215-1224.
#    DampRatio<-DispM.eigen$values[1]/Mod(DispM.eigen$values[2])
#
#    #Write outputs to file  #WARNING: This part is broken
#    if (ToRun == 1){
#        cat(file=infile,  rhomax.Stott, rhomin.Stott, reac.Stott,  firststepatt.Stott, Re(A.eigen$values[1:5]),sep="\t",append=TRUE)
#        cat(file=infile, "\t", sep=" ",append=TRUE)
#        cat(file=infile, Re(DispM.eigen$values[1:5]), sep="\t", append=TRUE)
#        cat(file=infile, "\t", sep=" ",append=TRUE)
#        cat(file=infile, 1000 * sd(DispM.StableStageDist), sep="\t", append=TRUE)
#        cat(file=infile, "\t", sep=" ",append=TRUE)
#        cat(file=infile, DampRatio, sep="\t", append=TRUE)
#        cat(file=infile, "\t", sep=" ",append=TRUE)
#        }
#    
#    return(list(rhomax.Stott,rhomin.Stott,reac.Stott,firststepatt.Stott,DispM.eigen,DispM.StableStageDist,DampRatio))
#    }

### Functions for visualizing the spatially explicit landscapes
#  #WARNING: THIS ONLY WORKS FOR SQUARE LANDSCAPES INPUT AS A SINGLE VECTOR STARTING AT THE TOP LEFT CORNER, reading left to right, then down a row, then left to right, until reaching the bottom right corner.
#  #landscape is the original landscape, eigen.vals is a list of dominant eigenvalues, linklist links the cell codes in the landscape to the order of the eigenvalues in the eigen.vals list.
#  #Add change.count and merge with CalcEigen?
#EigenVis = function(landscape,extent,linklist,eigen.pdf){
#    
#    #Set a variable for making the plots
#    maxplotval = 2
#    
#    #Do replacements for each thing in linklist (need to make sure the replacements don't double replace!)
#    #First replacement converts to letters, to avoid double replacement of numbers (e.g., if 1 gets changed by chance to 2, and the next replacement looks for 2's, it will convert the replaced 2, and that would be bad.
#    for (item in linklist){
#        original.value = item[1]
#        letter.value = item[2]
#        landscape[landscape == original.value] <- letter.value
#        }
#
#    #Second replacement changes the letters to the eigen values.  This is to avoid the possibility of double replacements due to overlap between numeric codes and eigenvalues (admittedly a low probability, but I like my codes to work cleanly)
#    for (item in linklist){
#        letter.value = item[2]
#        eigen.val = as.numeric(item[3])
#        landscape[landscape == letter.value] <- eigen.val
#        
#        #For plotting part
#        if (eigen.val > maxplotval){
#            maxplotval = eigen.val
#            }
#        
#        }
#        
#    #R idiotically made landscape all text, so here we convert it back to numbers. #Note that as.numeric on eigen.val above does not fix the problem.  Stupid R!
#    landscape = as.numeric(landscape)
#    
#    #Convert the landscape into a matrix #Need to specify byrow to get it to write each row right to left
#    landscape.matrix = matrix(landscape,nrow = extent,ncol = extent,byrow = TRUE)
#    
#    #Make it into a raster
#    landscape.raster = raster(landscape.matrix)
#    
#    pdf(eigen.pdf)
#    plot(landscape.raster,col = c("firebrick","orange","light green","chartreuse 4"), breaks = c(0,0.75,1,1.25,maxplotval),main = "Dominant Eigenvalues for landscape cells") #
#    dev.off()
#    return(landscape.raster)
#    }
#

##Visualize dispersal probabilities on the landscape
##NOTE: This function could still use improvement.
##NOTE: May be worth linking this to a simulation approach as well - then could look at dispersal effects over time
#  #NOTE: K_g is carrying capacity in terms of biomass, need to divide by plant biomass to get K in terms of numbers of individuals
#  #WARNING - THIS METHOD MAY FAIL IF A POPULATION IS DETERMINISTICALLY DECLINING TO EXTINCITON LOCALLY BUT RECEIVES ENOUGH DISPERSING INDIVIDUALS TO PERSIST.
#    #Need to figure out under what circumstances this may occur.
#  #The plots are still less than satisfying.  But maybe because my landscape is too small.
#  #NOTE: add change.count
#DispersalVis = function(landscape,DispersalProbabilities,K,SeedNum,eigen.vals,extent,disp.pdf) {
#        
#    #Loop through land cover types
#    allseeds = c(0,0,0) #**# This will need to be changed if more than 3 landcover types
#    for (lc in 1:3){ #**# THIS WILL NEED TO BE CHANGED IF MORE THAN 3 landcover types!
#                
#        # Check if eigenvalue is < 1, if so, the population will go extinct, and not produce dispersers at equilibrium
#        if (eigen.vals[[lc]] >= 1){
#
#            #This is number of adults at carrying capacity * seeds produced per adult        
#            #number of adults is equal to the K at equilibrium, #Calculate number of seeds produced per cell
#            TotSeeds = K * SeedNum 
#            allseeds[lc] = TotSeeds #Put number of seeds into a vector
#            }
#        }
#
#    #Set up output landscape
#    disp.landscape = rep(0,length(landscape))
#
#    pdf(disp.pdf)
#    #Go through every cell in the landscape
#    for (cell in 1:length(landscape)){
#
#        #Identify landcover type for cell of interest
#        celltype = landscape[cell] + 1 #+1 is to correspond with allseeds vector.  This will get a number from the landscape, 1 = grassland (0 + 1), 2 = forest (1 + 1), 3 = cropland (2 + 1)
#    
#        #Get total seeds produced for that landscape type
#        cell.seeds = allseeds[celltype]
#                       
#        #Use dispersal probabilites to calculate dispersal
#        #Looks like DispersalProbabilities comes by cell (cells selected from left to right by rows from top to bottom), with distances to each other cell, going left to right by rows from top to bottom,
#          #So, most of the dispersing seeds in Jakob's model remain in the cell of origin (actually seems reasonable), then a small portion disperse out to other cells.
#        
#        #Read Dispersal probabilities in blocks of extent^2
#        blocksize = extent*extent #The blocks in dispersal probabilities are the square of the extent (also equal to landscape length, but for different reasons)
#        blockstart = 1 + (cell -1) * blocksize #Starts at 1, second block starts at 401, 3rd block at 801, etc. (for block size = 400)
#        blockend = cell * blocksize  #Ends at 400, then 800, etc. for blocksize = 400
#        
#        cell.dispersal.prob = DispersalProbabilities[blockstart:blockend]
#        
#        #Convert from Dispersal probabilities to realized dispersal at equilibrium  (total seeds produced * Dispersal probabilitie for a given cell)  
#        cell.dispersal = cell.dispersal.prob * cell.seeds 
#        
#        #Add values to total dispersal for the landscape
#        disp.landscape = disp.landscape + cell.dispersal
#        
#        #Display dispersal per cell
#        cell.matrix = matrix(cell.dispersal, ncol = extent, nrow = extent,byrow = T)
#        cell.raster = raster(log(cell.matrix)) # Can get rid of log if desired.
#        plot(cell.raster, main = sprintf("Maximum Log Dispersal for cell %s", cell),col = colorRampPalette(c("dark red","yellow","chartreuse 4"))(255))
#        }
#    
#    #Display overall dispersal map
#    landscape.disp.matrix = matrix(disp.landscape,ncol = extent, nrow = extent, byrow = T)
#    landscape.disp.raster = raster(log(landscape.disp.matrix))
#    plot(landscape.disp.raster,col = colorRampPalette(c("dark red","yellow","chartreuse 4"))(255), main = "Maximum Log Dispersal for the whole landscape") #255 says how many colors to use in the color ramp.
#    dev.off()
#    }
#
    
