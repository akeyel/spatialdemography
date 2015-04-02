# Purpose is to save code used to test code during development

#I'm sure there is a better way to do this!

# A.C. Keyel, begun 2015-03-04

#Species Loop - testing revisions

land.sp = sample(seq(1,16), 16)
patch.sp.num = 1
p = 16

#Test passed - each cell has a unique species and species check comes out correct.

#Check with 2 species per patch
land.sp = sample(seq(1,32), 32)
patch.sp.num = 2
p = 16


## Test new subsetting to just one environmental layer value type
#test of scl.subset
scale.cells.lst = "subset;L;1"
env.lbl = c("L","E")
landscape = list(c(1,2,2,1,1,2), c(0,1,2,3,4,5) ) 

scl.subset(scale.cells.lst, env.lbl, landscape)  
#Test passed - all 2's are now 0's

#Test with more values
scale.cells.lst = "subset;E;3"
scl.subset(scale.cells.lst, env.lbl, landscape)  
# Test passed - data were correctly recoded.